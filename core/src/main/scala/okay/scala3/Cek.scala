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

  /** the machine's steps, one per case: the hot ones here, small; a handler entered or left (`enter`, `leave`), an operation going out to a
   * general handler (`out`), each a method of its own */
  @tailrec private def steps[R[_], X, V, Z](c: Prog[R, X], σ: Frames[R, X, V], κ: Levels[R, V, Z]): Z = c match
    case v: Prog.Value[r, x] => σ match
        case b: Frames.Bind[r2, a, y, v2] => steps[R, y, V, Z](b.f(v.a), b.rest, κ)
        case m: Frames.Map[r2, a, y, v2] => steps[R, y, V, Z](Prog.Value(m.f(v.a)), m.rest, κ)
        case f: Frames.Finally[r2, x2, v2] =>
          f.release()
          steps[R, X, V, Z](c, f.rest, κ)
        case _: Frames.End[r2, v2] => κ match
            case _: Levels.Top[p, z] => v.a
            case _ =>
              val s = leave(v.a, κ)
              steps(s.c, s.σ, s.κ)
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

  /** a handler or a renaming entered: a level of its own, the body's frames from nothing */
  private def enter[R[_], X, V, Z](c: Prog[R, X], σ: Frames[R, X, V], κ: Levels[R, V, Z]): State[?, ?, ?, Z] =
    (c: @unchecked) match
      case h: Prog.Handle[e, r, rest, a, b] =>
        State[r, a, a, Z](h.p, Frames.End(), Levels.Handled[e, r, R, a, b, V, Z](h.at, Handler.fresh(h.h), σ, κ))
      case r: Prog.Remap[r1, r2, a] =>
        State[r1, a, a, Z](r.p, Frames.End(), Levels.Renamed[r1, R, a, V, Z](r.rename, σ, κ))
      case e: Prog.Ensure[r, a] => State[R, a, V, Z](e.p, Frames.Finally[R, a, V](e.release, σ), κ)

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

  /** an operation at its own handler `l`: the clause run outside it, with `k`; or no `k`, aborting; or answered in
   * place, the levels crossed put back */
  private def here[E[_], R0[_], X, V0, R[_], Rest[_], A, B, W, Z](l: Levels.Handled[E, R, Rest, A, B, W, Z],
    op: E[X], inner: Frames[R0, X, V0], crossed: Crossed[R0, V0, R, A]): State[?, ?, ?, Z] = l.h match
    case g: Handler.General[e, t, a, b] =>
      val k = (x: X) => Prog.Resume[Rest, X, B, R0, V0, E, R, A](inner, crossed, l.at, g, x)
      State[Rest, B, W, Z](g.op[X](op, k), l.outside, l.up)
    case a: Handler.Abort[e, t, a2, b] =>
      // the rest dropped: what it holds released, innermost first
      Frames.released(inner)
      Crossed.released(crossed)
      State[Rest, B, W, Z](a.abort[X](op), l.outside, l.up)
    case t: Handler.Tail[e, t2, a2, b] => State[R0, X, V0, Z](Prog.Value(t.answer[X](op)), inner, Crossed.put(crossed, l))
    case s: Handler.Stateful[e, t, a2, b, st] =>
      State[R0, X, V0, Z](Prog.Value(s.answer[X](op, s.cell)), inner, Crossed.put(crossed, l))

  def run[A](p: Prog[Pure, A]): A = steps[Pure, A, A, A](p, Frames.End(), Levels.Top(Escape.pure))

  /** a program over `P`, a row of no effect of another representation, its places' `escape` refuting what gets past
   * every handler: how another library's rows run on this machine */
  def run[P[_], A](p: Prog[P, A], escape: Escape[P]): A = steps[P, A, A, A](p, Frames.End(), Levels.Top(escape))
