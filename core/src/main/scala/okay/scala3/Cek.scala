package okay.scala3

import scala.annotation.tailrec

// THE MACHINE'S CONTINUATIONS, typed by the row the program inside them is at

/** σ, THE PURE CONTINUATION: the binds from a value `X` to the value `V` of the nearest handler's body, at `R` */
enum Frames[R[_], -X, +V]:
  case End[R[_], V]() extends Frames[R, V, V]
  case Bind[R[_], X, Y, V](f: X => Prog[R, Y], rest: Frames[R, Y, V]) extends Frames[R, X, V]
  case Map[R[_], X, Y, V](f: X => Y, rest: Frames[R, Y, V]) extends Frames[R, X, V]

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
 * back by `Cek.put` wherever `k` is resumed */
enum Crossed[RIn[_], VIn, ROut[_], VOut]:
  case Nothing[R[_], V]() extends Crossed[R, V, R, V]
  case Handled[RIn[_], VIn, E[_], R[_], Rest[_], A, B, W](inner: Crossed[RIn, VIn, R, A], at: At[E, R, Rest],
    h: Handler[E, Rest, A, B], outside: Frames[Rest, B, W]) extends Crossed[RIn, VIn, Rest, W]
  case Renamed[RIn[_], VIn, R1[_], R2[_], A, W](inner: Crossed[RIn, VIn, R1, A], rename: Sub[R1, R2],
    outside: Frames[R2, A, W]) extends Crossed[RIn, VIn, R2, W]

/**
 * THE CEK MACHINE (Felleisen and Friedman), with the generalised continuations of Hillerström, Lindley and Atkey
 * ("Effect handlers via generalised continuations", 2020): its state is ⟨C, E, K⟩ — C the program to run, E its
 * environment (Scala's closures carry it), K the continuation, split in two: σ, the frames up to the nearest handler,
 * and κ, the handlers with the frames outside each. A step is one case of `loop`, a `@tailrec` loop: so a run takes
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
  /** where the run goes on after a cold step */
  final case class State[R[_], X, V, Z](c: Prog[R, X], σ: Frames[R, X, V], κ: Levels[R, V, Z])

  /** a handler as it is installed, or put back by `k`: a stateful one with a cell of its own, from its state now */
  private inline def fresh[E[_], T[_], A, B](h: Handler[E, T, A, B]): Handler[E, T, A, B] = h match
    case s: Handler.Stateful[e, t, a, b, st] => Handler.Stateful(Cell(s.cell.state), s.ret, s.answer)
    case _ => h

  /** the body's value given to the handler */
  private inline def ret[E[_], T[_], A, B](h: Handler[E, T, A, B], a: A): Prog[T, B] = h match
    case g: Handler.General[e, t, a2, b] => g.ret(a)
    case t: Handler.Tail[e, t2, a2, b] => t.ret(a)
    case s: Handler.Stateful[e, t, a2, b, st] => s.ret(s.cell.state, a)
    case x: Handler.Abort[e, t, a2, b] => x.ret(a)

  /** the levels crossed put back on κ */
  @tailrec private def put[RIn[_], VIn, ROut[_], VOut, Z](crossed: Crossed[RIn, VIn, ROut, VOut],
    κ: Levels[ROut, VOut, Z]): Levels[RIn, VIn, Z] = crossed match
    case _: Crossed.Nothing[r, v] => κ
    case h: Crossed.Handled[ri, vi, e, r, rest, a, b, w] => put(h.inner, Levels.Handled(h.at, fresh(h.h), h.outside, κ))
    case r: Crossed.Renamed[ri, vi, r1, r2, a, w] => put(r.inner, Levels.Renamed(r.rename, r.outside, κ))

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

  /** the same, each place compared by `take`: this library's statically, another representation's by its own */
  @tailrec private def inPlaceTaken[E[_], R[_], V, Z](at: At[E, R, ?], κ: Levels[R, V, Z]): Handler[E, ?, ?, ?] | Null =
    κ match
      case l: Levels.Handled[e, r, rest, a, b, w, z] => taken(l.at, at) match
          case _: Taken.Mine[e2, r2] => l.h
          case t: Taken.Theirs[e2, e3, r2] => inPlaceTaken[E, rest, w, Z](t.at, l.up)
      case r: Levels.Renamed[r1, r2, a, w, z] => inPlaceTaken[E, r2, w, Z](r.rename(at), r.up)
      case _: Levels.Top[p, z] => null

  /** an operation's place `at` met by a handler's place `mine` */
  private def taken[E[_], E2[_], R[_], Rest[_]](mine: At[E, R, Rest], at: At[E2, R, ?]): Taken[E, E2, Rest] =
    mine match
      case place: Place[E, R, Rest] @unchecked => Taken(place, at)
      case _ => mine.take(at)

  /** the loop: the hot steps here, small; a handler entered or left, an operation going out, in `cold` */
  @tailrec private def loop[R[_], X, V, Z](c: Prog[R, X], σ: Frames[R, X, V], κ: Levels[R, V, Z]): Z = c match
    case v: Prog.Value[r, x] => σ match
        case b: Frames.Bind[r2, a, y, v2] => loop[R, y, V, Z](b.f(v.a), b.rest, κ)
        case m: Frames.Map[r2, a, y, v2] => loop[R, y, V, Z](Prog.Value(m.f(v.a)), m.rest, κ)
        case _: Frames.End[r2, v2] => κ match
            case _: Levels.Top[p, z] => v.a
            case _ =>
              val s = cold(c, σ, κ)
              loop(s.c, s.σ, s.κ)
    case b: Prog.FlatMap[r, y, x] => loop[R, y, V, Z](b.p, Frames.Bind[R, y, x, V](b.f, σ), κ)
    case m: Prog.Map[r, y, x] => loop[R, y, V, Z](m.p, Frames.Map[R, y, x, V](m.f, σ), κ)
    case p: Prog.Perform[r, e, x] => inPlace[e, R, V, Z](p.at, κ) match
        // answered in place, the answer straight into the next frame
        case t: Handler.Tail[e2, t2, a2, b2] =>
          val x = t.answer[x](p.op)
          σ match
            case b: Frames.Bind[r2, a3, y, w] => loop[R, y, V, Z](b.f(x), b.rest, κ)
            case _ => loop[R, X, V, Z](Prog.Value(x), σ, κ)
        case s: Handler.Stateful[e2, t2, a2, b2, st] =>
          val x = s.answer[x](p.op, s.cell)
          σ match
            case b: Frames.Bind[r2, a3, y, w] => loop[R, y, V, Z](b.f(x), b.rest, κ)
            case _ => loop[R, X, V, Z](Prog.Value(x), σ, κ)
        case _ =>
          val s = out[e, R, x, V, R, V, Z](p.at, p.op, σ, Crossed.Nothing[R, V](), κ)
          loop(s.c, s.σ, s.κ)
    case d: Prog.Defer[r, x] => loop[R, X, V, Z](d.p(), σ, κ)
    // `k` resumed: the levels it took and its handler put back, the value straight into the frame inside
    case r: Prog.Resume[rest, x, b, r0, v0, e, rr, a] =>
      val κ2 = put(r.crossed, Levels.Handled[e, rr, R, a, b, V, Z](r.at, r.h, σ, κ))
      r.inner match
        case f: Frames.Bind[r2, x2, y, v] => loop[r0, y, v0, Z](f.f(r.x), f.rest, κ2)
        case _ => loop[r0, x, v0, Z](Prog.Value(r.x), r.inner, κ2)
    case _ =>
      val s = cold(c, σ, κ)
      loop(s.c, s.σ, s.κ)

  /** the cold steps: a handler's body ended, a handler or a renaming entered */
  private def cold[R[_], X, V, Z](c: Prog[R, X], σ: Frames[R, X, V], κ: Levels[R, V, Z]): State[?, ?, ?, Z] = c match
    case v: Prog.Value[r, x] => σ match
        case b: Frames.Bind[r2, a, y, v2] => State[R, y, V, Z](b.f(v.a), b.rest, κ)
        case m: Frames.Map[r2, a, y, v2] => State[R, y, V, Z](Prog.Value(m.f(v.a)), m.rest, κ)
        case _: Frames.End[r2, v2] => κ match
            case l: Levels.Handled[e, r, rest, a, b, w, z] => State[rest, b, w, Z](ret(l.h, v.a), l.outside, l.up)
            case l: Levels.Renamed[r1, r2, a, w, z] => State[r2, a, w, Z](Prog.Value(v.a), l.outside, l.up)
            case _: Levels.Top[p, z] => State[R, X, V, Z](c, σ, κ) // the end: given back to the loop as it is
    case h: Prog.Handle[e, r, rest, a, b] =>
      State[r, a, a, Z](h.p, Frames.End(), Levels.Handled[e, r, R, a, b, V, Z](h.at, fresh(h.h), σ, κ))
    case r: Prog.Remap[r1, r2, a] => State[r1, a, a, Z](r.p, Frames.End(), Levels.Renamed[r1, R, a, V, Z](r.rename, σ, κ))
    case r: Prog.Resume[rest, x, b, r0, v0, e, rr, a] =>
      State[r0, x, v0, Z](Prog.Value(r.x), r.inner, put(r.crossed, Levels.Handled[e, rr, R, a, b, V, Z](r.at, r.h, σ, κ)))
    case p: Prog.Perform[r, e, x] => out[e, R, x, V, R, V, Z](p.at, p.op, σ, Crossed.Nothing[R, V](), κ)
    case b: Prog.FlatMap[r, y, x] => State[R, y, V, Z](b.p, Frames.Bind[R, y, x, V](b.f, σ), κ)
    case m: Prog.Map[r, y, x] => State[R, y, V, Z](m.p, Frames.Map[R, y, x, V](m.f, σ), κ)
    case d: Prog.Defer[r, x] => State[R, X, V, Z](d.p(), σ, κ)

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

  /** the same, each place compared by `take`, in a loop of its own: once off the commonest places, it stays here */
  @tailrec private def outTaken[E[_], R0[_], X, V0, R[_], V, Z](at: At[E, R, ?], op: E[X], inner: Frames[R0, X, V0],
    crossed: Crossed[R0, V0, R, V], κ: Levels[R, V, Z]): State[?, ?, ?, Z] = κ match
    case l: Levels.Handled[e, r, rest, a, b, w, z] => taken(l.at, at) match
        case _: Taken.Mine[e2, r2] => here[E, R0, X, V0, R, rest, a, b, w, Z](l, op, inner, crossed)
        case t: Taken.Theirs[e2, e3, r2] =>
          outTaken[E, R0, X, V0, rest, w, Z](t.at, op, inner, Crossed.Handled(crossed, l.at, l.h, l.outside), l.up)
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
    case a: Handler.Abort[e, t, a2, b] => State[Rest, B, W, Z](a.abort[X](op), l.outside, l.up)
    case t: Handler.Tail[e, t2, a2, b] => State[R0, X, V0, Z](Prog.Value(t.answer[X](op)), inner, put(crossed, l))
    case s: Handler.Stateful[e, t, a2, b, st] =>
      State[R0, X, V0, Z](Prog.Value(s.answer[X](op, s.cell)), inner, put(crossed, l))

  def run[A](p: Prog[Pure, A]): A = loop[Pure, A, A, A](p, Frames.End(), Levels.Top(Escape.pure))

  /** a program over `P`, a row of no effect of another representation, its places' `escape` refuting what gets past
   * every handler: how another library's rows run on this machine */
  def run[P[_], A](p: Prog[P, A], escape: Escape[P]): A = loop[P, A, A, A](p, Frames.End(), Levels.Top(escape))
