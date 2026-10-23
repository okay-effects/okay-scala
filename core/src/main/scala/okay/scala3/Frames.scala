package okay.scala3

import scala.annotation.tailrec

// THE MACHINE'S CONTINUATION, K, in its parts, each with what is done to it: σ the binds of the body running, κ the
// levels — the handlers and renamings around it — and what a `k` took of them, put back when it is resumed

/** σ, THE PURE CONTINUATION: the binds from a value `X` to the value `V` of the nearest handler's body, at `R` */
enum Frames[R[_], -X, +V]:
  case End[R[_], V]() extends Frames[R, V, V]
  case Bind[R[_], X, Y, V](f: X => Prog[R, Y], rest: Frames[R, Y, V]) extends Frames[R, X, V]
  case Map[R[_], X, Y, V](f: X => Y, rest: Frames[R, Y, V]) extends Frames[R, X, V]
  /** `release` run as a value passes, or as the frames are dropped by a handler aborting */
  case Finally[R[_], X, V](release: () => Unit, rest: Frames[R, X, V]) extends Frames[R, X, V]

object Frames:
  /** the frames' `release`s run, innermost first: they are dropped */
  @tailrec private[scala3] def released(σ: Frames[?, ?, ?]): Unit = σ match
    case f: Frames.Finally[r, x, v] =>
      f.release()
      released(f.rest)
    case b: Frames.Bind[r, x, y, v] => released(b.rest)
    case m: Frames.Map[r, x, y, v] => released(m.rest)
    case _: Frames.End[r, v] => ()

/** κ, THE HANDLERS: the program inside at `R`, its body's value `V`, the run's `Z` */
enum Levels[R[_], V, Z]:
  /** the top: the run's row `P`, of no effect; an operation that got here, past every handler, `escape` refutes */
  case Top[P[_], Z](escape: Escape[P]) extends Levels[P, Z, Z]
  /** a handler of `E` at its place `at` in `R`; outside it, the frames from `B` to `W` at `Rest`, and the levels */
  case Handled[E[_], R[_], Rest[_], A, B, W, Z](at: At[E, R, Rest], h: Handler[E, Rest, A, B],
    outside: Frames[Rest, B, W], up: Levels[Rest, W, Z]) extends Levels[R, A, Z]
  /** a renaming of places, from the program's `R1` to `R2` outside */
  case Renamed[R1[_], R2[_], A, W, Z](rename: Sub[R1, R2], outside: Frames[R2, A, W], up: Levels[R2, W, Z])
    extends Levels[R1, A, Z]

/** the levels a `k` took, from the innermost's body at `RIn` of `VIn` to the outermost's at `ROut` of `VOut`: data, put
 * back by `Crossed.put` wherever `k` is resumed */
enum Crossed[RIn[_], VIn, ROut[_], VOut]:
  case Nothing[R[_], V]() extends Crossed[R, V, R, V]
  case Handled[RIn[_], VIn, E[_], R[_], Rest[_], A, B, W](inner: Crossed[RIn, VIn, R, A], at: At[E, R, Rest],
                                                          h: Handler[E, Rest, A, B], outside: Frames[Rest, B, W]) extends Crossed[RIn, VIn, Rest, W]
  case Renamed[RIn[_], VIn, R1[_], R2[_], A, W](inner: Crossed[RIn, VIn, R1, A], rename: Sub[R1, R2],
                                                outside: Frames[R2, A, W]) extends Crossed[RIn, VIn, R2, W]

object Crossed:
  /** the levels crossed put back on κ */
  @tailrec private[scala3] def put[RIn[_], VIn, ROut[_], VOut, Z](crossed: Crossed[RIn, VIn, ROut, VOut],
    κ: Levels[ROut, VOut, Z]): Levels[RIn, VIn, Z] = crossed match
    case _: Crossed.Nothing[r, v] => κ
    case h: Crossed.Handled[ri, vi, e, r, rest, a, b, w] => put(h.inner, Levels.Handled(h.at, Handler.fresh(h.h), h.outside, κ))
    case r: Crossed.Renamed[ri, vi, r1, r2, a, w] => put(r.inner, Levels.Renamed(r.rename, r.outside, κ))

  /** the levels crossed released, innermost first: each level's frames outside it */
  private[scala3] def released(crossed: Crossed[?, ?, ?, ?]): Unit = crossed match
    case _: Crossed.Nothing[r, v] => ()
    case h: Crossed.Handled[ri, vi, e, r, rest, a, b, w] =>
      released(h.inner)
      Frames.released(h.outside)
    case r: Crossed.Renamed[ri, vi, r1, r2, a, w] =>
      released(r.inner)
      Frames.released(r.outside)
