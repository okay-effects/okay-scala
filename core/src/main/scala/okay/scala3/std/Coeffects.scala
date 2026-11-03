package okay.scala3.std

import okay.scala3.*

/** THE HISTORY BEHIND A POINT: the past, in order, and the focus — never empty; a comonad. `extract` the focus,
 * `extend(f)` f at every point, each with the history up to it: a decision that reads what came before, made again
 * at every step (an audit) */
final case class History[A](past: Vector[A], focus: A):
  def extract: A = focus
  def extend[B](f: History[A] => B): History[B] =
    History(past.indices.toVector.map(i => f(History(past.take(i), past(i)))), f(this))
  def duplicate: History[History[A]] = extend(identity)
  def map[B](f: A => B): History[B] = History(past.map(f), f(focus))
  /** every point, the focus last */
  def all: Vector[A] = past :+ focus
  /** this history with `next` its focus, this one's focus gone into the past */
  def :+(next: A): History[A] = History(all, next)

object History:
  /** the history of these points, the last its focus; none of none */
  def of[A](points: Vector[A]): Option[History[A]] = points.lastOption.map(History(points.init, _))

/**
 * COEFFECTS — what a computation needs from its context (Petricek, Orchard and Mycroft, 2014), where effects are what
 * it does to the world. A flat one, a value read anywhere, is an effect already: an operation a handler answers. One
 * that needs the PAST is a comonad's: `overHistory` keeps the history of an effect's operations and decides each by a
 * function of it.
 */
object Coeffects:
  /** each of `p`'s operations of `E` decided by `decide` from the history behind it — the operations before it, as
   * kept (`pending` before one runs, `ended` after, with its answer), and it the focus: `None` it goes on, `Some(x)`
   * it is answered here with `x`, not passed on. `History.extend` of the same decision over the history kept is its
   * audit. The history is the program's as it runs (one run) */
  def overHistory[E[_], H, R[_], A](p: A ! R)(pending: [X] => E[X] => H, ended: [X] => (E[X], X) => H)(
      decide: [X] => (History[H], E[X]) => Option[X])(using In[E, R]): A ! R =
    var past = Vector.empty[H]
    around[E, R, A](p)([X] => (op: E[X], proceed: X ! R) => decide[X](History(past, pending[X](op)), op) match
      case Some(x) =>
        past :+= ended[X](op, x)
        pure[X, R](x)
      case None => proceed.map { x =>
        past :+= ended[X](op, x)
        x
      })
