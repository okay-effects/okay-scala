package okay.scala3.std

import okay.scala3.*

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

/** A SCOPE OF RESOURCES: each acquired in it released when the scope ends — ends, fails, or is dropped — the last
 * acquired first. Fibers forked in it cancelled then too: structured concurrency, no fiber outliving its scope */
enum Scope[X]:
  case Register(release: () => Unit) extends Scope[Unit]

object Scope:
  /** `acquire`d, and `release`d when the scope ends */
  def acquire[S, R[_]](acquire: S ! R)(release: S => Unit)(using In[Scope, R]): S ! R =
    acquire.flatMap(s => perform[Scope](Register(() => release(s))).as(s))

  /** `release` when the scope ends */
  def defer[R[_]](release: => Unit)(using In[Scope, R]): Unit ! R = perform[Scope](Register(() => release))

  /** `p` forked, and cancelled when the scope ends if it runs still */
  def fork[A, R[_]](p: A ! Async)(using ExecutionContext, In[Async, R], In[Scope, R]): Fiber[A] ! R =
    Async.fork[A, R](p).flatMap(f => perform[Scope](Register(() => f.running.cancel())).as(f))

  /** the scope: what is registered in `p` released when it ends, the last first; a release that throws does not stop
   * the others, the first throw thrown after them */
  def run[R[_], Rest[_], A](p: A ! R)(using Split[Scope, R, Rest]): A ! Rest =
    okay.scala3.defer {
      var releases = List.empty[() => Unit]
      ensuring(handle(p, Handler.tail[Scope, Rest, A, A](pure)([X] => (op: Scope[X]) => op match
        case Register(r) => releases = r :: releases)))(releaseAll(releases))
    }

  private def releaseAll(rs: List[() => Unit]): Unit =
    var first: Throwable = null
    rs.foreach(r => try r() catch case NonFatal(e) => if first == null then first = e)
    if first != null then throw first
