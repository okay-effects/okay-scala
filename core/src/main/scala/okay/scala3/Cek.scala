package okay.scala3

import scala.annotation.tailrec

/**
 * THE CEK MACHINE (Felleisen and Friedman), with the generalised continuations of Hillerström, Lindley and Atkey
 * ("Effect handlers via generalised continuations", 2020): its state is ⟨C, E, K⟩ — C the program to run, E its
 * environment (Scala's closures carry it), K the continuation, split in two: σ, the frames up to the nearest handler,
 * and κ, the handlers with the frames outside each. A step is one case of `steps`, a `@tailrec` loop: so a run takes
 * no stack, however deep its binds, its handlers, its resumptions.
 *
 * An operation goes out through κ to its handler, its place matched against each handler's: `Taken` says, by the
 * types, that it is the handler's own, or gives its place outside. This library's places, `Place`, are compared here at
 * once, the last place against the last; any other representation of places (`At`) compares its own. Answered in place (`Tail`, `Stateful`), the answer
 * goes on into σ at once. Taken by a general handler, σ and the handlers crossed become `k` — data, `Resume`, put back
 * wherever `k` is resumed, as many times as it is: multi-shot. Everything is typed, by enums and Scala 3's GADT
 * inference: no cast.
 *
 * LEAVING: `ensuring` puts a frame on σ that holds the finalizer, a program; a value that comes to it waits while the
 * finalizer runs. A `k` unwound (`Unwind`) is put back as it is resumed, but without its handler — where that was is
 * a level that answers nothing (`Gone`) — and its frames are passed with no value (`walk`): a bind's is dropped, a
 * finalizer's runs its finalizer, a level is left without its handler's value clause.
 */
object Cek:
  /** where the run goes on after a step out of the loop */
  final case class State[R[_], X, V, Z](c: Prog[R, X], σ: Frames[R, X, V], κ: Levels[R, V, Z])

  /** the handler of the operation at `at`, if it answers in place; `null` if it does not, or there is none. Small,
   * for the JIT to inline: the commonest places compared here, anything else by `inPlaceTaken` */
  @tailrec private def inPlace[E[_], R[_], V, Z](at: At[E, R, ?], κ: Levels[R, V, Z]): Handler[E, ?, ?, ?] | Null =
    κ match
      case l: Levels.Handled[e, r, rest, a, b, w, z] => (l.at: @unchecked) match
          case _: Place.Last[r1, e1] => (at: @unchecked) match
              case _: Place.Last[r2, e2] => l.h
              case i: Place.Init[e2, r2, g, rest2] => inPlace[E, rest, w, Z](i.at, l.up)
              case _ => inPlaceTaken(at, κ)
          case _: Place.Only[e1] => (at: @unchecked) match
              case _: Place.Only[e2] => l.h
              case _ => inPlaceTaken(at, κ)
          case _ => inPlaceTaken(at, κ)
      case r: Levels.Renamed[r1, r2, a, w, z] => inPlace[E, r2, w, Z](r.rename(at), r.up)
      case _: Levels.Top[p, z] => null
      case _ => inPlaceTaken(at, κ)

  /** the same, each place asked by `outside` for the operation's place beyond it, and the one it stops at by `take` */
  @tailrec private def inPlaceTaken[E[_], R[_], V, Z](at: At[E, R, ?], κ: Levels[R, V, Z]): Handler[E, ?, ?, ?] | Null =
    κ match
      case l: Levels.Handled[e, r, rest, a, b, w, z] => l.at.outside(at) match
          case null => l.at.take(at) match
              case _: Taken.Mine[e2, r2] => l.h
              case _ => null // `outside` and `take` disagree: not a place of one row
          case beyond: At[E, rest, ?] @unchecked => inPlaceTaken[E, rest, w, Z](beyond, l.up)
      case r: Levels.Renamed[r1, r2, a, w, z] => inPlaceTaken[E, r2, w, Z](r.rename(at), r.up)
      case _: Levels.Top[p, z] => null
      // a handler that has left: past it, or from where it kept its effect; its own with none kept, nobody answers
      case g: Levels.Gone[e, r, rest, a, w, z] =>
        val beyond = g.at.outside(at)
        if beyond != null then inPlaceTaken[E, rest, w, Z](beyond, g.up)
        else g.at.take(at) match
          case _: Taken.Mine[e2, r2] =>
            val kept = g.kept
            if kept != null then inPlaceTaken[E, rest, w, Z](kept, g.up) else null
          case _ => null

  /** the machine's steps, one per case: the hot ones here, small; a handler entered or left (`enter`, `leave`), an operation going out to a
   * general handler (`out`), each a method of its own */
  @tailrec private def steps[R[_], X, V, Z](c: Prog[R, X], σ: Frames[R, X, V], κ: Levels[R, V, Z]): Z = c match
    case v: Prog.Value[r, x] => σ match
        case b: Frames.Bind[r2, a, y, v2] => steps[R, y, V, Z](b.f(v.a), b.rest, κ)
        case m: Frames.Map[r2, a, y, v2] => steps[R, y, V, Z](Prog.Value(m.f(v.a)), m.rest, κ)
        case _: Frames.End[r2, v2] => κ match
            case _: Levels.Top[p, z] => v.a
            case _ =>
              val s = leave(v.a, κ)
              steps(s.c, s.σ, s.κ)
        case f: Frames.Finally[r2, x2, v2] => f.release() match
            // a finalizer that is a program runs, and the value waits for it; a thunk has run
            case p: Prog[R, Any] => steps[R, Any, V, Z](p, Frames.Bind[R, Any, X, V](_ => c, f.rest), κ)
            case _ => steps[R, X, V, Z](c, f.rest, κ)
        // a value come to an unwinding is dropped: the unwinding goes on, or at its end gives `()`
        case u: Frames.Unwinding[r2, y, v2] =>
          val s = walk[R, y, V, Z](u.rest, κ, u.to)
          steps(s.c, s.σ, s.κ)
        case e: Frames.Unwound[r2, v2] => steps[R, Unit, V, Z](Prog.Value(()), e.rest, κ)
    case b: Prog.FlatMap[r, y, x] => steps[R, y, V, Z](b.p, Frames.Bind[R, y, x, V](b.f, σ), κ)
    case m: Prog.Map[r, y, x] => steps[R, y, V, Z](m.p, Frames.Map[R, y, x, V](m.f, σ), κ)
    case p: Prog.Perform[r, e, x] => inPlace[e, R, V, Z](p.at, κ) match
        // answered in place, the answer straight into the next frame
        case t: Handler.Tail[e2, t2, a2, b2] =>
          val x = t.answer[x](p.op)
          σ match
            case b: Frames.Bind[r2, a3, y, w] => steps[R, y, V, Z](b.f(x), b.rest, κ)
            case _ => steps[R, X, V, Z](Prog.Value(x), σ, κ)
        case s: Handler.Stateful[e2, t2, a2, b2, st] =>
          val x = s.answer[x](p.op, s.cell)
          σ match
            case b: Frames.Bind[r2, a3, y, w] => steps[R, y, V, Z](b.f(x), b.rest, κ)
            case _ => steps[R, X, V, Z](Prog.Value(x), σ, κ)
        case _ =>
          val s = out[e, R, x, V, R, V, Z](p.at, p.op, σ, Crossed.Nothing[R, V](), κ)
          steps(s.c, s.σ, s.κ)
    case d: Prog.Defer[r, x] => steps[R, X, V, Z](d.p(), σ, κ)
    // `k` resumed: the levels it took and its handler put back, the value straight into the frame inside
    case r: Prog.Resume[rest, x, b, r0, v0, e, rr, a] =>
      val κ2 = Crossed.put(r.crossed, Levels.Handled[e, rr, R, a, b, V, Z](r.at, r.h, σ, κ))
      r.inner match
        case f: Frames.Bind[r2, x2, y, v] => steps[r0, y, v0, Z](f.f(r.x), f.rest, κ2)
        case _ => steps[r0, x, v0, Z](Prog.Value(r.x), r.inner, κ2)
    case _ =>
      val s = enter(c, σ, κ)
      steps(s.c, s.σ, s.κ)

  /** a handler's body ended with `a`, at its level: the handler's value clause, or a renaming left */
  private def leave[R[_], V, Z](a: V, κ: Levels[R, V, Z]): State[?, ?, ?, Z] = (κ: @unchecked) match
    case l: Levels.Handled[e, r, rest, a, b, w, z] => State[rest, b, w, Z](Handler.ret(l.h, a), l.outside, l.up)
    case l: Levels.Renamed[r1, r2, a, w, z] => State[r2, a, w, Z](Prog.Value(a), l.outside, l.up)
    case g: Levels.Gone[e, r, rest, a, w, z] => State[rest, Any, w, Z](Prog.Value(a), g.outside, g.up)

  /** a handler or a renaming entered: a level, the body's frames from nothing. A finalizer: its frame. A
   * `k` unwound: its levels put back over the end of the unwinding — what waited for it — and walked */
  private def enter[R[_], X, V, Z](c: Prog[R, X], σ: Frames[R, X, V], κ: Levels[R, V, Z]): State[?, ?, ?, Z] =
    (c: @unchecked) match
      case h: Prog.Handle[e, r, rest, a, b] =>
        State[r, a, a, Z](h.p, Frames.End(), Levels.Handled[e, r, R, a, b, V, Z](h.at, Handler.fresh(h.h), σ, κ))
      case r: Prog.Remap[r1, r2, a] => remapped[r1, R, a, V, Z](r, σ, κ)
      case e: Prog.Ensure[r, a] => State[R, a, V, Z](e.p, Frames.Finally[R, a, V](e.release, σ), κ)
      case u: Prog.Unwind[rest, x, b, r0, v0, e, rr, a] =>
        val end = Frames.Unwound[R, V](σ)
        val gone = Levels.Gone[e, rr, R, a, V, Z](u.at, Handler.kept(u.h), end, κ)
        walk[r0, x, v0, Z](u.inner, Crossed.putLeft(u.crossed, gone, end), end)

  /** A PROGRAM OF THE ROW `R1` ENTERED, in the row `R`: a level that renames. IN TAIL POSITION — no frame waits for
   * its value — under a level that renames, it needs none of its own: that one renames for both, by the two
   * renamings as one (`Sub.composed`). So calls in tail position from row to row, however many, keep one level, and
   * its renaming stays as big as the row. Under a handler's level, or the run's, it has its own: nothing is
   * composed across a handler; nor is a renaming of a shape `composed` does not know */
  private def remapped[R1[_], R[_], A, V, Z](r: Prog.Remap[R1, R, A], σ: Frames[R, A, V], κ: Levels[R, V, Z])
  : State[?, ?, ?, Z] = σ match
    case _: Frames.End[r0, v] => κ match
        case lower: Levels.Renamed[r0, r2, a, w, z] =>
          val both = Sub.composed(r.rename, lower.rename)
          if both == null then State[R1, A, A, Z](r.p, Frames.End(), Levels.Renamed[R1, R, A, V, Z](r.rename, σ, κ))
          else State[R1, A, A, Z](r.p, Frames.End(), Levels.Renamed[R1, r2, A, w, Z](both, lower.outside, lower.up))
        case _ => State[R1, A, A, Z](r.p, Frames.End(), Levels.Renamed[R1, R, A, V, Z](r.rename, σ, κ))
    case _ => State[R1, A, A, Z](r.p, Frames.End(), Levels.Renamed[R1, R, A, V, Z](r.rename, σ, κ))

  /** UNWINDING, to its end `to`: the frames passed with no value. A finalizer's frame: a program runs, and the
   * unwinding goes on after it (`Unwinding`); a thunk has run. The end of a level's frames: the level is left without
   * its handler's value clause, and the frames outside it are unwound. The end of another unwinding, begun inside this
   * one, is passed */
  @tailrec private def walk[R[_], X, V, Z](σ: Frames[R, X, V], κ: Levels[R, V, Z], to: AnyRef): State[?, ?, ?, Z] =
    σ match
      case b: Frames.Bind[r, x, y, v] => walk[R, y, V, Z](b.rest, κ, to)
      case m: Frames.Map[r, x, y, v] => walk[R, y, V, Z](m.rest, κ, to)
      case f: Frames.Finally[r, x, v] => f.release() match
          case p: Prog[R, Any] => State[R, Any, V, Z](p, Frames.Unwinding[R, X, V](f.rest, to), κ)
          case _ => walk[R, X, V, Z](f.rest, κ, to)
      case u: Frames.Unwinding[r, y, v] => walk[R, y, V, Z](u.rest, κ, to)
      case e: Frames.Unwound[r, v] =>
        if e eq to then State[R, Unit, V, Z](Prog.Value(()), e.rest, κ) else walk[R, Unit, V, Z](e.rest, κ, to)
      case _: Frames.End[r, v] => κ match
          case l: Levels.Handled[e, r, rest, a, b, w, z] => walk[rest, b, w, Z](l.outside, l.up, to)
          case l: Levels.Renamed[r1, r2, a, w, z] => walk[r2, a, w, Z](l.outside, l.up, to)
          case g: Levels.Gone[e, r, rest, a, w, z] => walk[rest, Any, w, Z](g.outside, g.up, to)
          // the rest of an unwinding, kept by a handler inside it and resumed where the unwinding is not
          case _: Levels.Top[p, z] => throw NoUnwinding()

  /** an operation going out to its handler: the levels it crosses taken into `crossed` */
  @tailrec private def out[E[_], R0[_], X, V0, R[_], V, Z](at: At[E, R, ?], op: E[X], inner: Frames[R0, X, V0],
    crossed: Crossed[R0, V0, R, V], κ: Levels[R, V, Z]): State[?, ?, ?, Z] = κ match
    case l: Levels.Handled[e, r, rest, a, b, w, z] => (l.at: @unchecked) match
        // a handler at the last place, nearly every one: the two places compared here, nothing made
        case _: Place.Last[r1, e1] => (at: @unchecked) match
            case _: Place.Last[r2, e2] => here[E, R0, X, V0, R, rest, a, b, w, Z](l, op, inner, crossed)
            case i: Place.Init[e2, r2, g, rest2] =>
              out[E, R0, X, V0, rest, w, Z](i.at, op, inner, Crossed.Handled(crossed, l.at, l.h, l.outside), l.up)
            case _ => outTaken(at, op, inner, crossed, κ)
        case _ => outTaken(at, op, inner, crossed, κ)
    case r: Levels.Renamed[r1, r2, a, w, z] =>
      out[E, R0, X, V0, r2, w, Z](r.rename(at), op, inner, Crossed.Renamed(crossed, r.rename, r.outside), r.up)
    // past every handler: the run's row has no effect, so no operation gets here — its places say why
    case top: Levels.Top[p, z] => top.escape(at, op)
    case _ => outTaken(at, op, inner, crossed, κ)

  /** the same, each place asked by `outside`, in a loop of its own: once off the commonest places, it stays here */
  @tailrec private def outTaken[E[_], R0[_], X, V0, R[_], V, Z](at: At[E, R, ?], op: E[X], inner: Frames[R0, X, V0],
    crossed: Crossed[R0, V0, R, V], κ: Levels[R, V, Z]): State[?, ?, ?, Z] = κ match
    case l: Levels.Handled[e, r, rest, a, b, w, z] => l.at.outside(at) match
        case null => l.at.take(at) match
            case _: Taken.Mine[e2, r2] => here[E, R0, X, V0, R, rest, a, b, w, Z](l, op, inner, crossed)
            case t: Taken.Theirs[e2, e3, r2] =>
              outTaken[E, R0, X, V0, rest, w, Z](t.at, op, inner, Crossed.Handled(crossed, l.at, l.h, l.outside), l.up)
        case beyond: At[E, rest, ?] @unchecked =>
          outTaken[E, R0, X, V0, rest, w, Z](beyond, op, inner, Crossed.Handled(crossed, l.at, l.h, l.outside), l.up)
    case r: Levels.Renamed[r1, r2, a, w, z] =>
      outTaken[E, R0, X, V0, r2, w, Z](r.rename(at), op, inner, Crossed.Renamed(crossed, r.rename, r.outside), r.up)
    case top: Levels.Top[p, z] => top.escape(at, op)
    // a handler that has left: past it, or from where it kept its effect; its own with none kept, nobody answers
    case g: Levels.Gone[e, r, rest, a, w, z] =>
      val beyond = g.at.outside(at)
      if beyond != null then
        outTaken[E, R0, X, V0, rest, w, Z](beyond, op, inner, Crossed.Gone(crossed, g.at, g.kept, g.outside), g.up)
      else g.at.take(at) match
        case _: Taken.Mine[e2, r2] =>
          val kept = g.kept
          if kept == null then stopped(g, inner, crossed)
          else outTaken[E, R0, X, V0, rest, w, Z](kept, op, inner, Crossed.Gone(crossed, g.at, kept, g.outside), g.up)
        case t: Taken.Theirs[e2, e3, r2] =>
          outTaken[E, R0, X, V0, rest, w, Z](t.at, op, inner, Crossed.Gone(crossed, g.at, g.kept, g.outside), g.up)

  /** an operation of a handler that has left, come to where it was: nobody answers. The finalizer that asks ends
   * there: the levels it crossed are put back, and the unwinding goes on from the frames at the operation */
  private def stopped[R0[_], X, V0, E[_], R[_], Rest[_], A, W, Z](g: Levels.Gone[E, R, Rest, A, W, Z],
    inner: Frames[R0, X, V0], crossed: Crossed[R0, V0, R, A]): State[?, ?, ?, Z] =
    // the unwinding that goes on here: the one this level was put back in, or else its own
    val to: AnyRef = g.outside match
      case u: Frames.Unwinding[r, y, v] => u.to
      case own => own
    walk[R0, X, V0, Z](inner, Crossed.put(crossed, g), to)

  /** an operation at its own handler `l`. A general handler's, the hot case, here and small, for the JIT to inline:
   * the clause run outside the handler, with `k` — also where `intercept` put it, with its effect's place (`Kept`) */
  private def here[E[_], R0[_], X, V0, R[_], Rest[_], A, B, W, Z](l: Levels.Handled[E, R, Rest, A, B, W, Z],
    op: E[X], inner: Frames[R0, X, V0], crossed: Crossed[R0, V0, R, A]): State[?, ?, ?, Z] = l.h match
    case g: Handler.General[e, t, a, b] => general[E, R0, X, V0, R, Rest, A, B, W, Z](l, g, op, inner, crossed)
    case k: Handler.Kept[e, t, a, b] => k.h match
        case g: Handler.General[e2, t2, a2, b2] => general[E, R0, X, V0, R, Rest, A, B, W, Z](l, g, op, inner, crossed)
        case h => clause[E, R0, X, V0, R, Rest, A, B, W, Z](l, h, op, inner, crossed)
    case h => clause[E, R0, X, V0, R, Rest, A, B, W, Z](l, h, op, inner, crossed)

  private def general[E[_], R0[_], X, V0, R[_], Rest[_], A, B, W, Z](l: Levels.Handled[E, R, Rest, A, B, W, Z],
    g: Handler.General[E, Rest, A, B], op: E[X], inner: Frames[R0, X, V0], crossed: Crossed[R0, V0, R, A])
  : State[?, ?, ?, Z] =
    State[Rest, B, W, Z](g.op[X](op, K[Rest, X, B, R0, V0, E, R, A](inner, crossed, l.at, l.h)), l.outside, l.up)

  /** the same by the other shapes of `h`, the handler of `l` or the one it holds: `k` as it is; or the rest left,
   * and the clause; or answered in place, the levels crossed put back */
  private def clause[E[_], R0[_], X, V0, R[_], Rest[_], A, B, W, Z](l: Levels.Handled[E, R, Rest, A, B, W, Z],
    h: Handler[E, Rest, A, B], op: E[X], inner: Frames[R0, X, V0], crossed: Crossed[R0, V0, R, A])
  : State[?, ?, ?, Z] = h match
    case c: Handler.Control[e, t, a, b] =>
      State[Rest, B, W, Z](c.op[X](op, K[Rest, X, B, R0, V0, E, R, A](inner, crossed, l.at, l.h)), l.outside, l.up)
    case a: Handler.Abort[e, t, a2, b] =>
      // the rest is left: unwound first if it holds a finalizer; holding none, nothing is made for it
      if Frames.holds(inner) || Crossed.holds(crossed) then
        val unwind = Prog.Unwind[Rest, X, B, R0, V0, E, R, A](inner, crossed, l.at, l.h)
        State[Rest, B, W, Z](Prog.FlatMap[Rest, Unit, B](unwind, _ => a.abort[X](op)), l.outside, l.up)
      else State[Rest, B, W, Z](a.abort[X](op), l.outside, l.up)
    case t: Handler.Tail[e, t2, a2, b] => State[R0, X, V0, Z](Prog.Value(t.answer[X](op)), inner, Crossed.put(crossed, l))
    case s: Handler.Stateful[e, t, a2, b, st] =>
      State[R0, X, V0, Z](Prog.Value(s.answer[X](op, s.cell)), inner, Crossed.put(crossed, l))
    case g: Handler.General[e, t, a, b] => general[E, R0, X, V0, R, Rest, A, B, W, Z](l, g, op, inner, crossed)
    case k: Handler.Kept[e, t, a, b] => clause[E, R0, X, V0, R, Rest, A, B, W, Z](l, k.h, op, inner, crossed)

  /** `k`: the frames at the operation, the levels it crossed, its handler and its place — data. Resumed, they are
   * put back (`Resume`); unwound, put back without the handler, and walked (`Unwind`) */
  private final class K[Rest[_], X, B, R0[_], V0, E[_], R[_], A](inner: Frames[R0, X, V0],
    crossed: Crossed[R0, V0, R, A], at: At[E, R, Rest], h: Handler[E, Rest, A, B]) extends Continuation[X, Rest, B]:
    def apply(x: X): Prog[Rest, B] = Prog.Resume(inner, crossed, at, h, x)
    def holds: Boolean = Frames.holds(inner) || Crossed.holds(crossed)
    def unwind: Prog[Rest, Unit] = if holds then Prog.Unwind(inner, crossed, at, h) else Prog.Value(())

  def run[A](p: Prog[Pure, A]): A = steps[Pure, A, A, A](p, Frames.End(), Levels.Top(Escape.pure))

  /** a program over `P`, a row of no effect of another representation, its places' `escape` refuting what gets past
   * every handler: how another library's rows run on this machine */
  def run[P[_], A](p: Prog[P, A], escape: Escape[P]): A = steps[P, A, A, A](p, Frames.End(), Levels.Top(escape))
