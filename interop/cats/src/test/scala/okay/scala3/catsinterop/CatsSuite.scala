package okay.scala3.catsinterop

import cats.Traverse
import cats.effect.{IO, Ref, Resource}
import cats.effect.unsafe.IORuntime
import okay.scala3.*
import okay.scala3.std.*

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.*

/** cats and cats-effect over the effects — on every platform */
class CatsSuite extends munit.FunSuite:
  given ExecutionContext = Async.defaultContext
  given IORuntime = cats.effect.unsafe.implicits.global

  type St[X] = (State % Int)[X]

  test("cats' monad over the programs: a traverse, a state summed") {
    val p = Traverse[List].traverse[Program[St], Int, Int](List(1, 2, 3))(x => State.modify[Int, St](_ + x).as(x * 10))
    assertEquals(run(State.run(0)(p)), (6, List(10, 20, 30)))
  }

  test("cats' tailRecM in constant stack: 100 000 steps") {
    val p = monad[St].tailRecM(0)(i =>
      if i == 100000 then pure[Either[Int, Int], St](Right(i)) else State.modify[Int, St](_ + 1).as(Left(i + 1)))
    assertEquals(run(State.run(0)(p)), (100000, 100000))
  }

  type LE[X] = Lifted[[A] =>> Either[String, A]][X]

  test("a program of a cats monad's lifted, lowered to it by cats' monad") {
    val p: Int ! LE = Lift.lift[[A] =>> Either[String, A], Int, LE](Right(1))
      .flatMap(a => Lift.lift[[A] =>> Either[String, A], Int, LE](Left(s"no $a")))
    assertEquals(Lift.lower[[A] =>> Either[String, A], Int](p)(using lowering), Left("no 1"))
  }

  type LIO[X] = Lifted[IO][X]

  test("a program of IOs lifted, lowered to one IO, run") {
    val p: Int ! LIO = Lift.lift[IO, Int, LIO](IO(20)).flatMap(a => Lift.lift[IO, Int, LIO](IO(a + 22)))
    Lift.lower[IO, Int](p).unsafeToFuture().map(v => assertEquals(v, 42))
  }

  test("via: IOs as Futures, on an explicit runtime, one control monad lowered") {
    type R[X] = (Lifted[IO] + Lifted[Future])[X]
    val p: Int ! R = Lift.lift[Future, Int, R](Future(20)).flatMap(a => Lift.lift[IO, Int, R](IO(a + 22)))
    val f: Future[Int] = Lift.lower(Lift.via[IO, Future](IOs.toFuture(summon[IORuntime]))(p))
    f.map(v => assertEquals(v, 42))
  }

  test("fromIO: an IO's value, or its failure, the program's") {
    val p: Int ! Async = IOs.fromIO[Int, Async](IO(20)).flatMap(a => IOs.fromIO[Int, Async](IO(a + 22)))
    val failed: Int ! Async = IOs.fromIO[Int, Async](IO.raiseError(RuntimeException("boom")))
    Async.toFuture(p).map(v => assertEquals(v, 42))
      .flatMap(_ => Async.toFuture(failed).failed).map(e => assertEquals(e.getMessage, "boom"))
  }

  test("fromIO cancelled: the IO cancelled") {
    val cancelled = Promise[Unit]()
    val never: IO[Int] = IO.never[Int].onCancel(IO(cancelled.trySuccess(())).void)
    for
      r <- Async.toFuture(Async.timeout(30)(IOs.fromIO[Int, Async](never)))
      _ <- cancelled.future
    yield assertEquals(r, None)
  }

  test("toIO: a program as an IO; the IO cancelled, the program's wait cancelled") {
    val stopped = Promise[Unit]()
    val waiting: Int ! Async = Async.cancellable[Int, Async](_ => () => stopped.trySuccess(()))
    for
      v <- IOs.toIO(Async.sleep[Async](10).as(7)).unsafeToFuture()
      t <- IOs.toIO(waiting).timeout(30.millis).attempt.unsafeToFuture()
      _ <- stopped.future
    yield
      assertEquals(v, 7)
      assert(t.isLeft, t)
  }

  test("a Resource used: acquired, used, released — after a failure too") {
    val log = collection.mutable.ListBuffer.empty[String]
    def resource(name: String): Resource[IO, String] =
      Resource.make(IO { log += s"open $name"; name })(n => IO { log += s"close $n"; () })
    val used: Int ! Async = IOs.use[String, Int, Async](resource("a"))(n => Async.sleep[Async](5).as(n.length))
    val failing: Int ! Async = IOs.use[String, Int, Async](resource("b"))(_ => Async.fromFuture(Future.failed(
      RuntimeException("bad"))))
    for
      n <- Async.toFuture(used)
      e <- Async.toFuture(failing).failed
    yield
      assertEquals(n, 1)
      assertEquals(e.getMessage, "bad")
      assertEquals(log.toList, List("open a", "close a", "open b", "close b"))
  }

  test("both and race as IO does them") {
    def slow[A](ms: Long, a: A): A ! Async = Async.sleep[Async](ms).as(a)
    for
      ab <- Async.toFuture(IOs.both[String, Int, Async](slow(30, "a"), slow(10, 1)))
      r <- Async.toFuture(IOs.race[String, Int, Async](slow(200, "slow"), slow(10, 2)))
    yield
      assertEquals(ab, ("a", 1))
      assertEquals(r, Right(2))
  }

  test("a program, an IO's Ref and all: IO's own words over a program") {
    val p: Int ! Async = IOs.fromIO[Int, Async](for
      ref <- Ref.of[IO, Int](0)
      _ <- IOs.toIO(Async.sleep[Async](5)).flatMap(_ => ref.update(_ + 1))
      v <- ref.get
    yield v)
    Async.toFuture(p).map(v => assertEquals(v, 1))
  }
