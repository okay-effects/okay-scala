package okay.scala3.ziointerop

import okay.scala3.*
import okay.scala3.std.*

import scala.concurrent.{ExecutionContext, Future, Promise}
import zio.{Runtime, Unsafe, ZEnvironment, ZIO, ZLayer}

trait Greeting:
  def greet(name: String): String

/** ZIO and the effects: tasks lifted and lowered; typed, ZIO's environment a Reader, its error a Fail, its run an
 * Async; layers — on every platform */
class ZioSuite extends munit.FunSuite:
  given ExecutionContext = Async.defaultContext

  /** the task run by ZIO's runtime, its value a `Future`: munit waits for it */
  private def unsafeRun[A](task: zio.Task[A]): Future[A] =
    Unsafe.unsafe { implicit u => Runtime.default.unsafe.runToFuture(task) }

  test("a program of ZIO tasks, lowered to one task, run by ZIO's runtime") {
    val p: Int ! Tasks =
      fromZIOTask[Int, Tasks](ZIO.succeed(20)).flatMap(a => fromZIOTask[Int, Tasks](ZIO.attempt(a + 22)))
    unsafeRun(toZIOTask(p)).map(v => assertEquals(v, 42))
  }

  test("a task failed: the program's failure ZIO's") {
    val p: Int ! Tasks = fromZIOTask[Int, Tasks](ZIO.fail(RuntimeException("boom"))).map(_ + 1)
    unsafeRun(toZIOTask(p).either).map(v => assertEquals(v.left.map(_.getMessage), Left("boom")))
  }

  type Row[X] = ZioRow[Greeting, String][X]
  val hello: Greeting = (name: String) => s"hello, $name"

  /** run here: the reader given, the failure run, the async driven to a future */
  def here[A](p: A ! Row): Future[Either[String, A]] =
    Async.toFuture(Fail.run[String, Async + Fail % String, Async, A](
      Reader.run[ZEnvironment[Greeting], Row, Async + Fail % String, A](ZEnvironment(hello))(p)))

  test("typed: the environment a Reader") {
    here(fromZIOTyped[Greeting, String, String, Row](ZIO.serviceWith[Greeting](_.greet("Ann"))))
      .map(v => assertEquals(v, Right("hello, Ann")))
  }

  test("typed: a failure a Fail; a defect the run's failure") {
    here(fromZIOTyped[Greeting, String, Int, Row](ZIO.fail("bad"))).map(v => assertEquals(v, Left("bad")))
      .flatMap(_ => here(fromZIOTyped[Greeting, String, Int, Row](ZIO.die(RuntimeException("defect")))).failed)
      .map(e => assertEquals(e.getMessage, "defect"))
  }

  test("toZIOTyped: the Reader ZIO's environment, a Fail ZIO's failure") {
    val p: String ! Row = Reader.ask[ZEnvironment[Greeting], Row].map(_.get[Greeting].greet("Bo"))
    val q: Int ! Row = Fail.raise[String, Row]("no")
    unsafeRun(toZIOTyped(p).provideEnvironment(ZEnvironment(hello)).either)
      .map(v => assertEquals(v, Right("hello, Bo")))
      .flatMap(_ => unsafeRun(toZIOTyped(q).provideEnvironment(ZEnvironment(hello)).either))
      .map(v => assertEquals(v, Left("no")))
  }

  test("cancelled here: the ZIO's fiber interrupted") {
    val interrupted = Promise[Unit]()
    val never: ZIO[Greeting, String, Int] = ZIO.never.onInterrupt(ZIO.succeed(interrupted.trySuccess(())))
    val p: Option[Either[String, Int]] ! Async = Async.timeout(30)(
      Fail.run[String, Async + Fail % String, Async, Int](
        Reader.run[ZEnvironment[Greeting], Row, Async + Fail % String, Int](ZEnvironment(hello))(
          fromZIOTyped[Greeting, String, Int, Row](never))))
    for t <- Async.toFuture(p); _ <- interrupted.future yield assertEquals(t, None)
  }

  test("interrupted in ZIO: the program cancelled, the wait it was in cancelled") {
    val stopped = Promise[Unit]()
    val waiting: Int ! Row = Async.cancellable[Int, Row](_ => () => stopped.trySuccess(()))
    for
      r <- unsafeRun(toZIOTyped(waiting).provideEnvironment(ZEnvironment(hello)).timeout(zio.Duration.fromMillis(30))
        .either)
      _ <- stopped.future
    yield assertEquals(r, Right(None))
  }

  test("layers: a program's environment built by a layer; a program making a service, a layer") {
    type NoEnv[X] = ZioRow[Any, String][X]
    val greeted: String ! Row = Reader.ask[ZEnvironment[Greeting], Row].map(_.get[Greeting].greet("Cy"))
    val provided: String ! NoEnv = provideLayer[Any, String, Greeting, String, NoEnv](ZLayer.succeed(hello))(greeted)
    val made: ZLayer[Any, String, Greeting] = layer[Any, String, Greeting](pure[Greeting, NoEnv](hello))
    Async.toFuture(Fail.run[String, Async + Fail % String, Async, String](
      Reader.run[ZEnvironment[Any], NoEnv, Async + Fail % String, String](ZEnvironment.empty)(provided)))
      .map(v => assertEquals(v, Right("hello, Cy")))
      .flatMap(_ => unsafeRun(ZIO.serviceWith[Greeting](_.greet("Di")).provideLayer(made).either))
      .map(v => assertEquals(v, Right("hello, Di")))
  }

  test("there and back: a ZIO, a program, a ZIO") {
    val z0: ZIO[Greeting, String, Int] = ZIO.serviceWith[Greeting](_.greet("x").length)
    unsafeRun(toZIOTyped(fromZIOTyped[Greeting, String, Int, Row](z0)).provideEnvironment(ZEnvironment(hello)).either)
      .map(v => assertEquals(v, Right(8)))
  }
