package okay.scala3

import okay.scala3.std.*

import scala.concurrent.ExecutionContext

/** side effects, fibers, shared state, retries, scopes — on every platform */
class MoreEffectsSuite extends munit.FunSuite:
  given ExecutionContext = Async.defaultContext



  def counting[R[_]: Has[Sync]](log: collection.mutable.Buffer[String]): Int ! R =
    for
      _ <- Sync.delay(log += "a")
      n <- Sync.delay(log.size)
      _ <- Sync.delay(log += "b")
    yield n

  test("Sync: run in place, in order; each one seen by an observer") {
    val log = collection.mutable.Buffer.empty[String]
    assertEquals(run(Sync.run(counting[Sync](log))), 1)
    assertEquals(log.toList, List("a", "b"))
    var seen = 0
    val seeing = observe[Sync, Sync, Int](counting[Sync](log))([X] => (_: Sync[X]) => pure[Unit, Sync]({ seen += 1 }))
    assertEquals(run(Sync.run(seeing)), 3)
    assertEquals(seen, 3)
    assertEquals(Sync.attempt[Int, Sync](throw IllegalStateException("x")).pipe(p => run(Sync.run(p))).isLeft, true)
  }

  test("Sync.on: each side effect on a pool, the program resumed; a throw fails the run") {
    val p: Int ! Async = Sync.on(summon[ExecutionContext])(Sync.delay[Int, Async + Sync](20).map(_ + 22))
    val bad: Int ! Async =
      Sync.on(summon[ExecutionContext])(Sync.delay[Int, Async + Sync](throw IllegalStateException("no")))
    for
      v <- Async.toFuture(p)
      e <- Async.toFuture(bad).failed
    yield
      assertEquals(v, 42)
      assertEquals(e.getMessage, "no")
  }

  test("fork, join; a fiber cancelled never runs its rest; never, timed out") {
    var ran = false
    val p: (Int, Option[Int]) ! Async =
      for
        a <- Async.fork[Int, Async](Async.sleep[Async](20).as(1))
        b <- Async.fork[Unit, Async](Async.sleep[Async](50).map(_ => ran = true))
        _ <- b.cancel[Async]
        x <- a.join[Async]
        n <- Async.timeout(30)(Async.never[Int, Async])
        _ <- Async.sleep[Async](80)
      yield (x, n)
    Async.toFuture(p).map { r =>
      assertEquals(r, (1, None))
      assert(!ran, "a cancelled fiber ran on")
    }
  }

  test("Ref: updates from many fibers at once, none lost") {
    val p: Int ! Async =
      for
        ref <- Ref.make[Int, Async](0)
        _   <- Async.parTraverse((1 to 200).toList)(_ => Async.sleep[Async](1).andThen(ref.update[Async](_ + 1)))
        n   <- ref.get[Async]
      yield n
    Async.toFuture(p).map(n => assertEquals(n, 200))
  }

  test("Deferred: waited for by fibers, given once") {
    val p: (List[Int], Boolean) ! Async =
      for
        d      <- Deferred.make[Int, Async]
        waits  <- Async.fork[List[Int], Async](Async.parTraverse(List(1, 2, 3))(i => d.get[Async].map(_ + i)))
        _      <- Async.sleep[Async](20)
        first  <- d.complete[Async](10)
        second <- d.complete[Async](20)
        vs     <- waits.join[Async]
      yield (vs, first && !second)
    Async.toFuture(p).map(r => assertEquals(r, (List(11, 12, 13), true)))
  }

  test("Semaphore: at most n at once; a wait cancelled takes no permit") {
    val p: (Int, Long) ! Async =
      for
        sem  <- Semaphore.make[Async](2)
        now  <- Ref.make[Int, Async](0)
        most <- Ref.make[Int, Async](0)
        _    <- Async.parTraverse((1 to 6).toList)(_ => sem.withPermit(
                  for
                    n <- now.updateAndGet[Async](_ + 1)
                    _ <- most.update[Async](math.max(_, n))
                    _ <- Async.sleep[Async](20)
                    _ <- now.update[Async](_ - 1)
                  yield ()))
        m    <- most.get[Async]
        _    <- sem.acquire[Async]
        _    <- sem.acquire[Async]
        late <- Async.timeout(20)(sem.acquire[Async])
        _    <- sem.release[Async]
        _    <- sem.release[Async]
        free <- sem.available[Async]
      yield (m, if late.isEmpty then free else -1L)
    Async.toFuture(p).map(r => assertEquals(r, (2, 2L)))
  }

  test("Queue: a producer waits while it is full, a consumer while it is empty") {
    val p: List[Int] ! Async =
      for
        q     <- Queue.bounded[Int, Async](1)
        taker <- Async.fork[List[Int], Async](Async.parTraverse(List(1, 2, 3, 4))(_ => q.take[Async]).map(_.sorted))
        _     <- Async.parTraverse(List(1, 2, 3, 4))(i => q.offer[Async](i))
        vs    <- taker.join[Async]
      yield vs
    Async.toFuture(p).map(vs => assertEquals(vs, List(1, 2, 3, 4)))
  }

  type F[X] = (Fail % String)[X]

  test("Retry.times: run again while it fails, the last error raised on") {
    var tries = 0
    def flaky(okAt: Int): Int ! F = defer {
      tries += 1
      if tries < okAt then Fail.raise[String, F](s"try $tries") else pure(tries)
    }
    assertEquals(run(Fail.run(Retry.times[String, F, Pure, Int](3)(flaky(3)))), Right(3))
    tries = 0
    assertEquals(run(Fail.run(Retry.times[String, F, Pure, Int](1)(flaky(5)))), Left("try 2"))
    tries = 0
    assertEquals(run(Fail.run(Retry.times[String, F, Pure, Int](5, _ != "try 1")(flaky(5)))), Left("try 1"))
  }

  test("Retry.backoff: waits between runs") {
    var tries = 0
    type A[X] = (Async + Fail % String)[X]
    val flaky: Int ! A = defer {
      tries += 1
      if tries < 3 then Fail.raise[String, A]("no") else pure(tries)
    }
    val t0 = System.nanoTime()
    Async.toFuture(Fail.run(Retry.backoff[String, A, Async, Int](3, 20)(flaky))).map { r =>
      assertEquals(r, Right(3))
      assert((System.nanoTime() - t0) / 1000000 >= 55, "no wait")
    }
  }

  test("Scope: released when it ends or fails, the last first; its fibers cancelled") {
    val log = collection.mutable.Buffer.empty[String]
    type T[X] = (Fail % String + Scope)[X]
    def use(fail: Boolean): Int ! T =
      for
        a <- Scope.acquire[String, T](pure("a"))(s => log += s"release $s")
        b <- Scope.acquire[String, T](pure("b"))(s => log += s"release $s")
        _ <- if fail then Fail.raise[String, T]("boom") else unit[T]
      yield 1
    assertEquals(run(Fail.run(Scope.run(use(false)))), Right(1))
    assertEquals(run(Scope.run(Fail.run(use(true)))), Left("boom"))
    assertEquals(log.toList, List("release b", "release a", "release b", "release a"))
    var ran = false
    type W[X] = (Async + Scope)[X]
    val scoped: Unit ! Async = Scope.run(
      Scope.fork[Unit, W](Async.sleep[Async](40).map(_ => ran = true)).andThen(Async.sleep[W](5)))
    Async.toFuture(scoped.andThen(Async.sleep[Async](80))).map(_ => assert(!ran, "a fiber outlived its scope"))
  }

extension [A](a: A) private def pipe[B](f: A => B): B = f(a)
