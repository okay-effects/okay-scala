package okay.scala3.std

import okay.scala3.*

import scala.concurrent.ExecutionContext
import scala.util.Try

/** A SIDE EFFECT, as an operation: `Delay(f)`, `f` run by the handler — at once in place (`run`), on a pool for
 * blocking calls (`on`), or not at all in a test (a handler of one's own, or `intercept`). What `IO.delay` is to a
 * monad: the program says it does something, the handler decides when and where */
enum Sync[X]:
  case Delay[X](f: () => X) extends Sync[X]

object Sync:
  /** `f` run when the program gets here, by `Sync`'s handler */
  def delay[X, R[_]](f: => X)(using In[Sync, R]): X ! R = perform[Sync](Delay(() => f))

  /** `f` run, what it throws its `Left` */
  def attempt[X, R[_]](f: => X)(using In[Sync, R]): Either[Throwable, X] ! R = delay(Try(f).toEither)

  /** each side effect run at once, on the thread running the program: what it throws, thrown out of the run */
  def run[R[_], Rest[_], A](p: A ! R)(using Split[Sync, R, Rest]): A ! Rest =
    handle(p, Handler.tail[Sync, Rest, A, A](pure)([X] => (op: Sync[X]) => op match
      case Delay(f) => f()))

  /** each side effect run on `pool` — a blocking call, a thread of its own — and the program resumed when it ends: no
   * thread waits for it; what it throws fails the run */
  def on[R[_], Rest[_], A](pool: ExecutionContext)(p: A ! R)(using Split[Sync, R, Rest], In[Async, Rest]): A ! Rest =
    handle(p, Handler.general[Sync, Rest, A, A](pure)([X] => (op: Sync[X], k: X => A ! Rest) => op match
      case Delay(f) => Async.blocking[X, Rest](f())(using pool).flatMap(k)))
