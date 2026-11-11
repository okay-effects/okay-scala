package okay.scala3.std

import okay.scala3.*

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

/** A SCOPE OF RESOURCES: each acquired in it released when the scope is left — it ends, or a handler outside unwinds
 * it: a failure, a choice of nothing, a fiber cancelled (`ensuring`) — the last acquired first. Fibers forked in it
 * cancelled then too: structured concurrency, no fiber outliving its scope */
enum Scope[X]:
  case Register(release: () => Unit) extends Scope[Unit]
  /** what is registered released, the last first — the scope's own finalizer asks it, as the scope is left */
  case Close() extends Scope[Unit]

object Scope:
  /** `acquire`d, and `release`d when the scope ends */
  def acquire[S, R[_]](acquire: S ! R)(release: S => Unit)(using In[Scope, R]): S ! R =
    acquire.flatMap(s => perform[Scope](Register(() => release(s))).as(s))

  /** `release` when the scope ends */
  def defer[R[_]](release: => Unit)(using In[Scope, R]): Unit ! R = perform[Scope](Register(() => release))

  /** `p` forked, and cancelled when the scope ends if it runs still */
  def fork[A, R[_]](p: A ! Async)(using ExecutionContext, In[Async, R], In[Scope, R]): Fiber[A] ! R =
    Async.fork[A, R](p).flatMap(f => perform[Scope](Register(() => f.running.cancel())).as(f))

  /** the scope: what is registered in `p` released when it is left, the last first; a release that throws does not
   * stop the others, the first throw thrown after them. The releases are the handler's state, so each path of a
   * program resumed twice has its own; and its finalizer is a program that asks the handler for them */
  def run[R[_], Rest[_], A](p: A ! R)(using at: Split[Scope, R, Rest]): A ! Rest =
    handle(ensuring(p)(perform[Scope](Close())(using at)),
      Handler.stateful[Scope, Rest, A, A, List[() => Unit]](Nil)((_, a) => pure(a))(
        [X] => (op: Scope[X], cell: Cell[List[() => Unit]]) => op match
          case Register(r) => cell.set(r :: cell.get)
          case Close() =>
            val releases = cell.get
            cell.set(Nil)
            releaseAll(releases)))

  private def releaseAll(rs: List[() => Unit]): Unit =
    var first: Throwable = null
    rs.foreach(r => try r() catch case NonFatal(e) => if first == null then first = e)
    if first != null then throw first
