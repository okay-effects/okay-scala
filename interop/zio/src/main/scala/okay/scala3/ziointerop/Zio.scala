package okay.scala3.ziointerop

import okay.scala3.*
import okay.scala3.std.*

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}
import zio.{Exit, Runtime, Tag, Task, Unsafe, ZEnvironment, ZIO, ZLayer}

/** the effect of ZIO's tasks: the standard `Lift` of `Task`, a task performed as it is */
type Tasks[X] = Lifted[Task][X]

/** what lowers a program of tasks to one: ZIO's `succeed` and its bind, the rest suspended in it */
given taskLowering: Lowering[Task] with
  def pure[A](a: A): Task[A] = ZIO.succeed(a)
  def flatMap[A, B](ga: Task[A])(f: A => Task[B]): Task[B] = ga.flatMap(a => ZIO.suspend(f(a)))

/** a ZIO task performed inside a program */
def fromZIOTask[X, R[_]](task: Task[X])(using In[Tasks, R]): X ! R = Lift.lift[Task, X, R](task)

/** a program whose one effect is ZIO's tasks, as ONE task: a throw in it the task's failure */
def toZIOTask[A](p: A ! Tasks): Task[A] = ZIO.suspend(Lift.lower[Task, A](p)(using taskLowering))

/** a ZIO task as a `Future`, run on `runtime` when lifted: `Lift.via(taskToFuture())(p)` brings a program's `Tasks`
 * to its `Lifted[Future]`, to lower one control monad */
def taskToFuture(runtime: Runtime[Any] = Runtime.default): Task ~> Future =
  [X] => (task: Task[X]) => Unsafe.unsafe(implicit u => runtime.unsafe.runToFuture(task): Future[X])

/**
 * THE ROW OF A ZIO, typed, each of ZIO's channels one of the standard effects: its environment `Env` a `Reader %
 * ZEnvironment[Env]`, asked when it runs; its typed error `Err` a `Fail % Err`, raised; its run an `Async`. ZIO is
 * contravariant in `Env` and covariant in `Err`, so one row `ZioRow[Db & Log, AppErr]` takes every ZIO of a program.
 */
type ZioRow[Env, Err] = [X] =>> (Async + Fail % Err + Reader % ZEnvironment[Env])[X]

/** a ZIO that cannot fail, run on `runtime` as an `Async`: its exit the callback's — its value, or a defect as a
 * throwable; the wait cancelled, its fiber interrupted */
def fromTaskAsync[A, R[_]](z: ZIO[Any, Nothing, A], runtime: Runtime[Any] = Runtime.default)(using In[Async, R])
  : A ! R =
  Async.cancellable[A, R] { cb =>
    Unsafe.unsafe { implicit u =>
      val fiber = runtime.unsafe.fork(z)
      fiber.unsafe.addObserver {
        case Exit.Success(a) => cb(Right(a))
        case Exit.Failure(cause) => cb(Left(cause.squashTrace))
      }
      () =>
        runtime.unsafe.fork(fiber.interrupt)
        ()
    }
  }

/** a ZIO in the effects: its environment asked of the `Reader`, its typed error raised to the `Fail`, its run an
 * `Async` */
def fromZIOTyped[Env, Err, A, R[_]](z: ZIO[Env, Err, A], runtime: Runtime[Any] = Runtime.default)(
  using In[Reader % ZEnvironment[Env], R], In[Fail % Err, R], In[Async, R]): A ! R =
  Reader.ask[ZEnvironment[Env], R].flatMap(env =>
    fromTaskAsync[Either[Err, A], R](z.provideEnvironment(env).either, runtime)).flatMap {
    case Right(a) => pure(a)
    case Left(e) => Fail.raise[Err, R](e)
  }

/** the program as a ZIO: its `Reader` ZIO's environment, its `Fail` ZIO's failure, a throw out of it a defect; the
 * ZIO interrupted, the program cancelled — and the ZIOs it waits on interrupted in turn */
def toZIOTyped[Env, Err, A](p: A ! ZioRow[Env, Err]): ZIO[Env, Err, A] =
  ZIO.environmentWithZIO[Env] { (env: ZEnvironment[Env]) =>
    ZIO.asyncInterrupt[Any, Err, A] { done =>
      given ExecutionContext = Async.defaultContext
      val read = Reader.run[ZEnvironment[Env], ZioRow[Env, Err], Async + Fail % Err, A](env)(p)
      val running = Async.start(Fail.run[Err, Async + Fail % Err, Async, A](read))
      running.future.onComplete {
        case Success(Right(a)) => done(ZIO.succeed(a))
        case Success(Left(e)) => done(ZIO.fail(e))
        case Failure(t) => done(ZIO.die(t))
      }(using ExecutionContext.parasitic)
      Left(ZIO.succeed(running.cancel()))
    }
  }

/** the program's environment built by `layer` for its run, released after: its `Reader % ZEnvironment[Env]`
 * answered, the layer's input `In0` asked instead */
def provideLayer[In0, Err, Env, A, R[_]](layer: ZLayer[In0, Err, Env])(p: A ! ZioRow[Env, Err])(
  using In[Reader % ZEnvironment[In0], R], In[Fail % Err, R], In[Async, R]): A ! R =
  fromZIOTyped[In0, Err, A, R](toZIOTyped(p).provideLayer(layer))

/** a program making a service, as a layer: its row's environment the layer's input */
def layer[In0, Err, A: Tag](p: A ! ZioRow[In0, Err]): ZLayer[In0, Err, A] = ZLayer(toZIOTyped(p))
