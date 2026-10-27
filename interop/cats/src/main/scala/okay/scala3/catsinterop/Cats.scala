package okay.scala3.catsinterop

import cats.{Monad, StackSafeMonad}
import cats.effect.{IO, Resource}
import cats.effect.unsafe.IORuntime
import okay.scala3.*
import okay.scala3.std.*

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** a program over the row `R`, as cats sees it: a type constructor of its value */
type Program[R[_]] = [A] =>> A ! R

/** cats' `Monad` of the programs over the row `R`, stack safe — `tailRecM` by `flatMap`, the machine running in
 * constant stack: code written over `F[_]: Monad` runs on them */
given monad[R[_]]: StackSafeMonad[Program[R]] with
  def pure[A](a: A): A ! R = okay.scala3.pure(a)
  def flatMap[A, B](p: A ! R)(f: A => B ! R): B ! R = p.flatMap(f)

/** any cats monad as the standard effects' `Lowering`: `Lift.lift(ga)` performs a `G[X]` in a program, and
 * `Lift.lower(p)` gives the program back as a `G[A]` — `IO`, `Either`, `Option`, any other */
given lowering[G[_]](using G: Monad[G]): Lowering[G] with
  def pure[A](a: A): G[A] = G.pure(a)
  def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B] = G.flatMap(ga)(f)

/**
 * CATS-EFFECT'S IO AND THE EFFECTS, through the standard `Async`: an `IO` run on its runtime as an asynchronous
 * operation, cancelled with it; a program of `Async` an `IO`, cancelled with it. On them, a `Resource` used by a
 * program, and both and race as `IO` does them.
 */
object IOs:
  /** an `IO` run on `runtime`, its outcome the program's: its value, or its failure raised as the run's; the wait
   * cancelled, the `IO` cancelled */
  def fromIO[A, R[_]](io: IO[A])(using runtime: IORuntime, at: In[Async, R]): A ! R =
    Async.cancellable[A, R] { cb =>
      val (future, cancel) = io.unsafeToFutureCancelable()(using runtime)
      future.onComplete(r => cb(r.toEither))(using ExecutionContext.parasitic)
      () =>
        cancel()
        ()
    }

  /** a program of `Async` alone as an `IO`: its value, or what it threw; the `IO` cancelled, the program cancelled */
  def toIO[A](p: A ! Async): IO[A] = IO.async[A] { cb =>
    IO {
      val running = Async.start(p)(using Async.defaultContext)
      running.future.onComplete {
        case Success(a) => cb(Right(a))
        case Failure(t) => cb(Left(t))
      }(using ExecutionContext.parasitic)
      Some(IO(running.cancel()))
    }
  }

  /** an `IO` as a `Future`, run on `runtime` when lifted: `Lift.via(IOs.toFuture(runtime))(p)` brings a program's
   * `Lifted[IO]` to its `Lifted[Future]`, to lower one control monad */
  def toFuture(runtime: IORuntime): IO ~> Future = [X] => (io: IO[X]) => io.unsafeToFuture()(using runtime)

  /** a `Resource` acquired, used by `f`, released after — when `f` ends, fails, or is cancelled */
  def use[A, B, R[_]](resource: Resource[IO, A])(f: A => B ! Async)(using IORuntime, In[Async, R]): B ! R =
    fromIO[B, R](resource.use(a => toIO(f(a))))

  /** both at once, as `IO.both`: both values; one failing, the other cancelled */
  def both[A, B, R[_]](p: A ! Async, q: B ! Async)(using IORuntime, In[Async, R]): (A, B) ! R =
    fromIO[(A, B), R](IO.both(toIO(p), toIO(q)))

  /** the first to end, as `IO.race`: its value, the other cancelled */
  def race[A, B, R[_]](p: A ! Async, q: B ! Async)(using IORuntime, In[Async, R]): Either[A, B] ! R =
    fromIO[Either[A, B], R](IO.race(toIO(p), toIO(q)))
