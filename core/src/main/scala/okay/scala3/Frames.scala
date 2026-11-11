package okay.scala3

import scala.annotation.tailrec

// THE MACHINE'S CONTINUATION, K, in its parts, each with what is done to it: σ the binds of the body running, κ the
// levels — the handlers and renamings around it — and what a `k` took of them, put back when it is resumed

/** σ, THE PURE CONTINUATION: the binds from a value `X` to the value `V` of the nearest handler's body, at `R` */
enum Frames[R[_], -X, +V]:
  case End[R[_], V]() extends Frames[R, V, V]
  case Bind[R[_], X, Y, V](f: X => Prog[R, Y], rest: Frames[R, Y, V]) extends Frames[R, X, V]
  case Map[R[_], X, Y, V](f: X => Y, rest: Frames[R, Y, V]) extends Frames[R, X, V]
  /** `ensuring`: `release` made and run as a value passes, and when the frames are unwound — a program of the row,
   * or a thunk that has run by then */
  case Finally[R[_], X, V](release: () => Prog[R, Any] | Unit, rest: Frames[R, X, V]) extends Frames[R, X, V]
  /** an unwinding goes on from here, to its end `to`: what comes here — a finalizer's value, the value of a level
   * that is being unwound — is dropped, and `rest` is unwound */
  case Unwinding[R[_], Y, V](rest: Frames[R, Y, V], to: AnyRef) extends Frames[R, Any, V]
  /** the end of an unwinding: `()` to what waited for it */
  case Unwound[R[_], V](rest: Frames[R, Unit, V]) extends Frames[R, Any, V]

object Frames:
  /** as 0.1.1 had it, kept for its bytecode: the frames' thunks run, innermost first. Nothing calls it: frames are
   * left by the machine's walk, which runs a finalizer that is a program too (`Cek.walk`) */
  @tailrec private[scala3] def released(σ: Frames[?, ?, ?]): Unit = σ match
    case f: Frames.Finally[r, x, v] =>
      f.release()
      released(f.rest)
    case b: Frames.Bind[r, x, y, v] => released(b.rest)
    case m: Frames.Map[r, x, y, v] => released(m.rest)
    case u: Frames.Unwinding[r, y, v] => released(u.rest)
    case e: Frames.Unwound[r, v] => released(e.rest)
    case _: Frames.End[r, v] => ()

  /** a finalizer waits in the frames: unwinding them would run something */
  @tailrec private[scala3] def holds(σ: Frames[?, ?, ?]): Boolean = σ match
    case _: Frames.Finally[r, x, v] => true
    case b: Frames.Bind[r, x, y, v] => holds(b.rest)
    case m: Frames.Map[r, x, y, v] => holds(m.rest)
    case u: Frames.Unwinding[r, y, v] => holds(u.rest)
    case e: Frames.Unwound[r, v] => holds(e.rest)
    case _: Frames.End[r, v] => false

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
  /** WHERE A HANDLER WAS, while its `k` is unwound: it has left, and answers nothing. An operation of its own that a
   * finalizer asks goes on from `kept`, its effect's place outside, when the handler kept its effect in the row
   * (`intercept`); with none (`handle`) nobody is left to answer, and the finalizer ends there. Outside the level is
   * the end of the unwinding */
  case Gone[E[_], R[_], Rest[_], A, W, Z](at: At[E, R, Rest], kept: At[E, Rest, ?] | Null,
    outside: Frames[Rest, Any, W], up: Levels[Rest, W, Z]) extends Levels[R, A, Z]

/** the levels a `k` took, from the innermost's body at `RIn` of `VIn` to the outermost's at `ROut` of `VOut`: data, put
 * back by `Crossed.put` wherever `k` is resumed, by `Crossed.putLeft` where it is unwound */
enum Crossed[RIn[_], VIn, ROut[_], VOut]:
  case Nothing[R[_], V]() extends Crossed[R, V, R, V]
  case Handled[RIn[_], VIn, E[_], R[_], Rest[_], A, B, W](inner: Crossed[RIn, VIn, R, A], at: At[E, R, Rest],
                                                          h: Handler[E, Rest, A, B], outside: Frames[Rest, B, W]) extends Crossed[RIn, VIn, Rest, W]
  case Renamed[RIn[_], VIn, R1[_], R2[_], A, W](inner: Crossed[RIn, VIn, R1, A], rename: Sub[R1, R2],
                                                outside: Frames[R2, A, W]) extends Crossed[RIn, VIn, R2, W]
  /** a level whose handler had left (`Levels.Gone`), taken by a `k` in its turn */
  case Gone[RIn[_], VIn, E[_], R[_], Rest[_], A, W](inner: Crossed[RIn, VIn, R, A], at: At[E, R, Rest],
    kept: At[E, Rest, ?] | Null, outside: Frames[Rest, Any, W]) extends Crossed[RIn, VIn, Rest, W]

object Crossed:
  /** the levels crossed put back on κ */
  @tailrec private[scala3] def put[RIn[_], VIn, ROut[_], VOut, Z](crossed: Crossed[RIn, VIn, ROut, VOut],
    κ: Levels[ROut, VOut, Z]): Levels[RIn, VIn, Z] = crossed match
    case _: Crossed.Nothing[r, v] => κ
    case h: Crossed.Handled[ri, vi, e, r, rest, a, b, w] => put(h.inner, Levels.Handled(h.at, Handler.fresh(h.h), h.outside, κ))
    case r: Crossed.Renamed[ri, vi, r1, r2, a, w] => put(r.inner, Levels.Renamed(r.rename, r.outside, κ))
    case g: Crossed.Gone[ri, vi, e, r, rest, a, w] => put(g.inner, Levels.Gone(g.at, g.kept, g.outside, κ))

  /** the levels crossed put back on κ to be unwound, to `to`: what comes out of a level goes on unwinding */
  @tailrec private[scala3] def putLeft[RIn[_], VIn, ROut[_], VOut, Z](crossed: Crossed[RIn, VIn, ROut, VOut],
    κ: Levels[ROut, VOut, Z], to: AnyRef): Levels[RIn, VIn, Z] = crossed match
    case _: Crossed.Nothing[r, v] => κ
    case h: Crossed.Handled[ri, vi, e, r, rest, a, b, w] =>
      putLeft(h.inner, Levels.Handled(h.at, Handler.fresh(h.h), Frames.Unwinding(h.outside, to), κ), to)
    case r: Crossed.Renamed[ri, vi, r1, r2, a, w] =>
      putLeft(r.inner, Levels.Renamed(r.rename, Frames.Unwinding(r.outside, to), κ), to)
    case g: Crossed.Gone[ri, vi, e, r, rest, a, w] =>
      putLeft(g.inner, Levels.Gone(g.at, g.kept, Frames.Unwinding(g.outside, to), κ), to)

  /** as 0.1.1 had it, kept for its bytecode, as `Frames.released`: nothing calls it */
  private[scala3] def released(crossed: Crossed[?, ?, ?, ?]): Unit = crossed match
    case _: Crossed.Nothing[r, v] => ()
    case h: Crossed.Handled[ri, vi, e, r, rest, a, b, w] =>
      released(h.inner)
      Frames.released(h.outside)
    case r: Crossed.Renamed[ri, vi, r1, r2, a, w] =>
      released(r.inner)
      Frames.released(r.outside)
    case g: Crossed.Gone[ri, vi, e, r, rest, a, w] =>
      released(g.inner)
      Frames.released(g.outside)

  /** a finalizer waits in the levels crossed: in the frames outside one of them */
  @tailrec private[scala3] def holds(crossed: Crossed[?, ?, ?, ?]): Boolean = crossed match
    case _: Crossed.Nothing[r, v] => false
    case h: Crossed.Handled[ri, vi, e, r, rest, a, b, w] => Frames.holds(h.outside) || holds(h.inner)
    case r: Crossed.Renamed[ri, vi, r1, r2, a, w] => Frames.holds(r.outside) || holds(r.inner)
    case g: Crossed.Gone[ri, vi, e, r, rest, a, w] => Frames.holds(g.outside) || holds(g.inner)
