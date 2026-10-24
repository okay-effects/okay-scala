package okay.scala3.std

import okay.scala3.*

/** DELIMITED CONTROL: `Shift(f)`, `f` given the rest of the program up to the nearest `reset` — `k`, resumable any
 * number of times — its value the `reset`'s. `P` is the answer program, `B ! R` */
enum Control[P, X]:
  case Shift[P, X](f: (X => P) => P) extends Control[P, X]

object Control:
  /** the rest up to the nearest `reset` taken as `k`, and `f` run in its place */
  def shift[A, B, R[_], Rr[_]](f: (A => B ! R) => B ! R)(using In[Control % (B ! R), Rr]): A ! Rr =
    perform[Control % (B ! R)](Shift(f))

  /** the delimiter `shift` takes the rest up to: `Control`'s handler, answering `Shift(f)` with `f(k)` */
  def reset[B, R[_]](p: B ! (R + Control % (B ! R))): B ! R =
    handle(p, Handler.general[Control % (B ! R), R, B, B](pure)(
      [X] => (op: Control[B ! R, X], k: X => B ! R) => op match
        case Shift(f) => f(k)))
