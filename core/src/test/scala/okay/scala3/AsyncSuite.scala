package okay.scala3

import okay.scala3.std.*

import scala.concurrent.{ExecutionContext, Future}

/** asynchrony: futures awaited, timers, both at once, races, time-outs, cancelling — on every platform */
class AsyncSuite extends munit.FunSuite:
  given ExecutionContext = Async.defaultContext

  test("a future's value, awaited") {
    val p: Int ! Async = Async.fromFuture(Future(20)).flatMap(a => Async.fromFuture(Future(a + 1))).map(_ * 2)
    Async.toFuture(p).map(v => assertEquals(v, 42))
  }

  test("a failure, from a future or thrown, fails the run") {
    val failed: Int ! Async = Async.fromFuture(Future.failed(IllegalStateException("no")))
    Async.toFuture(failed).failed.map(e => assertEquals(e.getMessage, "no"))
  }

  test("both at once: as long as the longer, not their sum") {
    def after[A](ms: Long, a: A): A ! Async = Async.sleep[Async](ms).as(a)
    val t0 = System.nanoTime()
    Async.toFuture(Async.both(after(200, "a"), after(200, "b"))).map { ab =>
      assertEquals(ab, ("a", "b"))
      val ms = (System.nanoTime() - t0) / 1000000
      assert(ms < 390, s"$ms ms: one after the other")
    }
  }

  test("a race: the first to end, the other cancelled — its rest never run") {
    var slowRan = false
    val fast: String ! Async = Async.sleep[Async](20).as("fast")
    val slow: String ! Async = Async.sleep[Async](300).map(_ => { slowRan = true; "slow" })
    Async.toFuture(Async.race(fast, slow)).flatMap { r =>
      assertEquals(r, Left("fast"))
      Async.toFuture(Async.sleep[Async](400)).map(_ => assert(!slowRan, "the loser ran on"))
    }
  }

  test("a time-out: None, the program cancelled") {
    Async.toFuture(Async.timeout(30)(Async.sleep[Async](500).as(1))).map(r => assertEquals(r, None))
  }

  test("parTraverse: each at once, values in order") {
    Async.toFuture(Async.parTraverse(List(3, 1, 2))(n => Async.sleep[Async](n * 30L).as(n * 10)))
      .map(r => assertEquals(r, List(30, 10, 20)))
  }

  test("other effects handled first, the row then Async alone") {
    type R[X] = (Async + State % Int)[X]
    val p: Int ! R = for
      n <- State.get[Int, R]
      m <- Async.fromFuture[Int, R](Future(n + 5))
      _ <- State.put[Int, R](m)
    yield m * 2
    Async.toFuture(State.run[Int, R, Async, Int](1)(p)).map(r => assertEquals(r, (6, 12)))
  }

  test("a hundred thousand futures already done: taken in the loop, no stack") {
    def count(i: Int, acc: Long): Long ! Async =
      if i == 0 then pure(acc) else Async.fromFuture(Future.successful(i)).flatMap(n => count(i - 1, acc + n))
    Async.toFuture(defer(count(100000, 0))).map(r => assertEquals(r, 5000050000L))
  }

  test("a run cancelled: its future fails with Cancelled") {
    val running = Async.start(Async.sleep[Async](1000).as(1))
    running.cancel()
    running.future.failed.map(e => assert(e.isInstanceOf[Cancelled], e))
  }

  test("ensuring: a program that waits is kept, and its finalizer runs when it ends") {
    var closed = false
    var answer: Either[Throwable, Int] => Unit = _ => ()
    val p: Int ! Async = ensuring(Async.async[Int, Async](cb => answer = cb).map(_ + 1)) { closed = true }
    val running = Async.start(p)
    assert(!closed, "closed while it waits")
    answer(Right(1))
    running.future.map { v =>
      assertEquals(v, 2)
      assert(closed)
    }
  }

  test("a run cancelled is left: its finalizers run, the innermost first; one that waits ends at its wait") {
    var log = List.empty[String]
    val waits: Unit ! Async =
      defer(pure[Unit, Async] { log ::= "inner" }).flatMap(_ => Async.sleep[Async](1)).map(_ => log ::= "not run")
    val p: Int ! Async = ensuring(ensuring(Async.never[Int, Async])(waits))(log ::= "outer")
    val running = Async.start(p)
    running.cancel()
    assertEquals(log.reverse, List("inner", "outer"))
    running.future.failed.map(e => assert(e.isInstanceOf[Cancelled], e))
  }

  test("a race: the loser is left, and its finalizer runs") {
    var closed = List.empty[String]
    val fast: String ! Async = ensuring(Async.sleep[Async](10).as("fast"))(closed ::= "fast")
    val slow: String ! Async = ensuring(Async.sleep[Async](300).as("slow"))(closed ::= "slow")
    Async.toFuture(Async.race(fast, slow)).map { r =>
      assertEquals(r, Left("fast"))
      assertEquals(closed.sorted, List("fast", "slow"))
    }
  }

  test("a wait that fails leaves the program: its finalizers run, and the run fails") {
    var closed = false
    val failed: Int ! Async = Async.fromFuture(Future.failed(IllegalStateException("no")))
    Async.toFuture(ensuring(failed.map(_ + 1)) { closed = true }).failed.map { e =>
      assertEquals(e.getMessage, "no")
      assert(closed)
    }
  }

  test("a permit held by a fiber that is cancelled is given back") {
    val p: Long ! Async =
      for
        permits <- Semaphore.make[Async](1)
        fiber <- Async.fork[Unit, Async](permits.withPermit(Async.never[Unit, Async]))
        _ <- Async.sleep[Async](10)
        held <- permits.available[Async]
        _ <- fiber.cancel[Async]
        free <- permits.available[Async]
      yield held * 10 + free
    Async.toFuture(p).map(v => assertEquals(v, 1L))
  }
