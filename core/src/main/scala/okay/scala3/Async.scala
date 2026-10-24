package okay.scala3

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

/** AN ASYNCHRONOUS OPERATION: `Suspend(register)`, the program waiting until `register`'s callback is called — with
 * its value, or its failure; `register` gives back how to cancel the wait (a timer cleared, another run cancelled).
 * okay-simple's `async`, on this machine */
enum Async[X]:
  case Suspend[X](register: (Either[Throwable, X] => Unit) => (() => Unit)) extends Async[X]

/** a program running: its value to come, and how to cancel it — the wait it is in cancelled, the rest never run, the
 * future failed with [[Cancelled]] */
final class Running[A](val future: Future[A], cancelling: () => Unit):
  def cancel(): Unit = cancelling()

/** what a cancelled program's future fails with */
final class Cancelled extends RuntimeException("cancelled")

/**
 * ASYNCHRONY with no runtime of its own: a program of `Async` waits on callbacks, and `toFuture` drives it — runs it to
 * its next wait, registers the callback, and when it is called resumes the rest on the execution context, until it is
 * done: a `Future` of its value, failed if a callback gave a failure or the program threw. A program's other effects
 * are handled first, its row then `Async` alone. On every platform; `sleep` by the platform's timer.
 */
object Async:
  /** an operation answered by a callback, called once; it cannot be cancelled */
  def async[X, R[_]](register: (Either[Throwable, X] => Unit) => Unit)(using In[Async, R]): X ! R =
    cancellable[X, R] { cb =>
      register(cb)
      noCancel
    }

  /** an operation answered by a callback, called once; `register` gives back how to cancel it */
  def cancellable[X, R[_]](register: (Either[Throwable, X] => Unit) => (() => Unit))(using In[Async, R]): X ! R =
    perform[Async](Suspend(register))

  /** `f`'s value when it completes; `f` made when the program gets here */
  def fromFuture[X, R[_]](f: => Future[X])(using In[Async, R]): X ! R =
    async[X, R](cb => f.onComplete {
      case Success(x) => cb(Right(x))
      case Failure(e) => cb(Left(e))
    }(using ExecutionContext.parasitic))

  /** `ms` milliseconds later */
  def sleep[R[_]](ms: Long)(using In[Async, R]): Unit ! R =
    cancellable[Unit, R](cb => Timers.after(ms)(() => cb(Right(()))))

  /** the platform's execution context: the global one on the JVM and Scala Native, the microtask queue on Scala.js */
  def defaultContext: ExecutionContext = Timers.executionContext

  /** the program driven to its end: its value, a `Future` */
  def toFuture[A](p: A ! Async)(using ExecutionContext): Future[A] = start(p).future

  /** the program started: its future, and how to cancel it */
  def start[A](p: A ! Async)(using ec: ExecutionContext): Running[A] =
    val done = Promise[A]()
    val run = Run()
    drive(coroutine(p), done, run)
    Running(done.future, () => run.cancel(done))

  /** both at once, each run on its own: both values; one failing, the other cancelled; this cancelled, both */
  def both[A, B](p: A ! Async, q: B ! Async)(using ExecutionContext): (A, B) ! Async =
    cancellable[(A, B), Async] { cb =>
      val ra = start(p)
      val rb = start(q)
      ra.future.failed.foreach(_ => rb.cancel())
      rb.future.failed.foreach(_ => ra.cancel())
      ra.future.zip(rb.future).onComplete {
        case Success(ab) => cb(Right(ab))
        case Failure(e) => cb(Left(e))
      }
      () =>
        ra.cancel()
        rb.cancel()
    }

  /** each element's program at once: their values, in order; one failing, the others cancelled */
  def parTraverse[X, A](xs: List[X])(f: X => A ! Async)(using ExecutionContext): List[A] ! Async =
    cancellable[List[A], Async] { cb =>
      val runs = xs.map(x => start(f(x)))
      runs.foreach(r => r.future.failed.foreach(_ => runs.foreach(_.cancel())))
      Future.sequence(runs.map(_.future)).onComplete {
        case Success(as) => cb(Right(as))
        case Failure(e) => cb(Left(e))
      }
      () => runs.foreach(_.cancel())
    }

  /** the first to end: its value — the other cancelled */
  def race[A, B](p: A ! Async, q: B ! Async)(using ExecutionContext): Either[A, B] ! Async =
    cancellable[Either[A, B], Async] { cb =>
      val ra = start(p)
      val rb = start(q)
      Future.firstCompletedOf(List(ra.future.map(Left(_)), rb.future.map(Right(_)))).onComplete { r =>
        ra.cancel()
        rb.cancel()
        r match
          case Success(v) => cb(Right(v))
          case Failure(e) => cb(Left(e))
      }
      () =>
        ra.cancel()
        rb.cancel()
    }

  /** the value if the program ends within `ms` milliseconds: else `None`, the program cancelled */
  def timeout[A](ms: Long)(p: A ! Async)(using ExecutionContext): Option[A] ! Async =
    race(p, sleep[Async](ms)).map(_.left.toOption)

  /** a wait that cannot be cancelled */
  val noCancel: () => Unit = () => ()

  // ---- THE DRIVER: the program as a coroutine of waits, each registered, the rest resumed by its callback

  /** where a program stands: done, or waiting — `k` the rest, a program of no effect */
  private enum Waiting[A]:
    case Done(a: A)
    case Suspended[A, X](register: (Either[Throwable, X] => Unit) => (() => Unit), k: X => Waiting[A] ! Pure)
      extends Waiting[A]

  private def coroutine[A](p: A ! Async): Waiting[A] ! Pure =
    handle(p, Handler.general[Async, Pure, A, Waiting[A]](a => pure(Waiting.Done(a)))(
      [X] => (op: Async[X], k: X => Waiting[A] ! Pure) => op match
        case Suspend(register) => pure(Waiting.Suspended(register, k))))

  /** a run's state: cancelled or not, and how to cancel the wait it is in */
  private final class Run:
    @volatile var cancelled = false
    val waiting = AtomicReference[() => Unit](noCancel)
    def cancel[B](done: Promise[B]): Unit = if !cancelled then
      cancelled = true
      waiting.getAndSet(noCancel)()
      done.tryFailure(Cancelled())
    /** the wait now: cancelled at once if the run was, meanwhile */
    def waitingOn(c: () => Unit): Unit =
      waiting.set(c)
      if cancelled then waiting.getAndSet(noCancel)()

  /** the slot's mark: `register` returned before its callback was called */
  private object Later extends RuntimeException("later", null, false, false)
  private val later: Either[Throwable, Nothing] = Left(Later)

  /** a loop: a callback called while `register` runs — a future already done — is taken here and the loop goes on;
   * one called later resumes the rest on the execution context. Which of the two, one `compareAndSet` decides; a run
   * cancelled goes no further */
  private def drive[A](p: Waiting[A] ! Pure, done: Promise[A], run: Run)(using ec: ExecutionContext): Unit =
    var next: Option[Waiting[A] ! Pure] = Some(p)
    try
      while next.isDefined && !run.cancelled do
        val now = next.get
        next = None
        okay.scala3.run(now) match
          case Waiting.Done(a) => done.trySuccess(a)
          case s: Waiting.Suspended[a, x] =>
            val slot = AtomicReference[Either[Throwable, x]](null)
            val cancel = s.register { r =>
              if !slot.compareAndSet(null, r) && !run.cancelled then
                run.waiting.set(noCancel)
                r match
                  case Right(x) => ec.execute(() => drive(s.k(x), done, run))
                  case Left(e) => done.tryFailure(e)
            }
            if slot.compareAndSet(null, later) then run.waitingOn(cancel)
            else slot.get match
              case Right(x) => next = Some(s.k(x))
              case Left(e) => done.tryFailure(e)
    catch case NonFatal(e) => done.tryFailure(e)
