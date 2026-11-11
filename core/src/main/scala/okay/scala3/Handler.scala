package okay.scala3

// A HANDLER: what an effect's operations mean — its shapes, each run its own way by the machine — a stateful one's
// cell, and the rest of the program as a clause is given it

/** a stateful handler's state, read and written by its clause in place: `get` and `set` final, nothing virtual on
 * the way of an operation. `fresh` is the cell a handler is given when it is installed or put back by a `k` — a
 * subclass may carry another library's view of a cell, and give its own kind again */
class Cell[S](var state: S):
  final def get: S = state
  final def set(s: S): Unit = state = s
  def fresh: Cell[S] = Cell(state)

/**
 * THE REST OF A PROGRAM, as a handler's clause is given it: `k`. A function — `k(x)` is the program that RESUMES it
 * with the answer `x` — and `k.unwind` is the program that LEAVES it: the finalizers that wait in it (`ensuring`) run,
 * the innermost first, each where it was installed, and nothing else of it does; then `()`. The handler that unwinds
 * has left: an operation of its own that a finalizer asks then is not answered by it — it goes to the handler
 * outside, if this one kept its effect in the row (`intercept`), and else the finalizer ends there. `k` is a value,
 * and nothing is remembered of what was done with it: every resumption is a path, which runs its finalizers at its
 * end, and every unwinding is the end of a path — unwound twice, the finalizers run twice. A clause that neither
 * resumes nor unwinds `k` runs nothing of it: the rest is kept, and may be resumed later (`Async`).
 */
abstract class Continuation[-X, R[_], +B] private[scala3] () extends (X => B ! R):
  /** this continuation left: its finalizers, then `()` */
  def unwind: Unit ! R
  /** a finalizer waits in it: unwinding it would run something. It is data, so that is known now — and a handler
   * of this library makes nothing to leave a rest that holds none */
  private[scala3] def holds: Boolean

/** The rest of an unwinding resumed where the unwinding is not: a continuation taken from a finalizer while it ran
 * in an unwinding, by a handler inside the part unwound, was kept and resumed on another stack. The finalizers have
 * run; there is no value to go on with. */
final class NoUnwinding extends RuntimeException("the rest of an unwinding was resumed outside the unwinding")

/**
 * A HANDLER OF `E`, the row outside it `R`, the body's value `A`, its own `B`: of one of these shapes, which the
 * machine runs each its own way. Each clause takes an operation `E[X]` as it is, and its match learns `X` from the
 * case — `case Get() => s` answers an `S`:
 * {{{
 * Handler.tail[Reader % String, R, A, A](pure)([X] => (op: Reader[String, X]) => op match
 *   case Reader.Ask() => "Ann")
 * }}}
 * Made by `Handler.general`, `control`, `tail`, `abort`, `stateful`, or by the cases themselves.
 */
enum Handler[E[_], R[_], A, B]:
  /** `k` the rest of the body, under this handler: resumed any number of times, or none */
  case General(ret: A => B ! R, op: [X] => (E[X], X => B ! R) => B ! R)
  /** an operation ANSWERED IN PLACE: the body goes on at once, no `k` made */
  case Tail(ret: A => B ! R, answer: [X] => E[X] => X)
  /** an operation ABORTS: the rest is left — unwound, its finalizers run — and its clause is the handler's result */
  case Abort(ret: A => B ! R, abort: [X] => E[X] => B ! R)
  /** answered in place with a state of its own, `cell`: each time the handler is installed, or a `k` taken further
   * out resumes it, the machine gives it a cell of its own, from the state of its time */
  case Stateful[E[_], R[_], A, B, S](cell: Cell[S], ret: (S, A) => B ! R,
                                     answer: [X] => (E[X], Cell[S]) => X) extends Handler[E, R, A, B]
  /** as `General`, `k` given as it is: to resume, or to unwind (`Continuation`) */
  case Control(ret: A => B ! R, op: [X] => (E[X], Continuation[X, R, B]) => B ! R)
  /** `h`, ITS EFFECT KEPT IN THE ROW, at `at` outside it — what `intercept` installs: while its `k` is unwound, an
   * operation of its effect that a finalizer asks goes on from there, to the handler outside */
  case Kept(h: Handler[E, R, A, B], at: At[E, R, ?])

/** a handler of each shape, its value's clause first, its operations' after: `Handler.tail[Reader % String, R, A,
 * A](pure)([X] => (op: Reader[String, X]) => op match ...)`. Each gives a `Handler`, whatever its shape */
object Handler:
  /** `op` gets the operation and `k`, the rest of the body under this handler: resumed once, many times, or never */
  def general[E[_], R[_], A, B](ret: A => B ! R)(op: [X] => (E[X], X => B ! R) => B ! R): Handler[E, R, A, B] =
    General(ret, op)

  /** `op` gets the operation and `k` as it is: to resume, or to unwind — a clause that does not go on with the rest
   * of the program says `k.unwind` first, so that what the rest holds is released (`ensuring`) */
  def control[E[_], R[_], A, B](ret: A => B ! R)(op: [X] => (E[X], Continuation[X, R, B]) => B ! R)
  : Handler[E, R, A, B] = Control(ret, op)

  /** each operation answered in place: the body goes on at once with `answer`'s value, no `k` made */
  def tail[E[_], R[_], A, B](ret: A => B ! R)(answer: [X] => E[X] => X): Handler[E, R, A, B] = Tail(ret, answer)

  /** each operation aborts: the rest is left — unwound, its finalizers run — and `abort`'s program is the handler's
   * result; a rest that holds no finalizer is dropped as it is, nothing made for it */
  def abort[E[_], R[_], A, B](ret: A => B ! R)(abort: [X] => E[X] => B ! R): Handler[E, R, A, B] = Abort(ret, abort)

  /** a stateful handler from the state `init` */
  def stateful[E[_], R[_], A, B, S](init: S)(ret: (S, A) => B ! R)(answer: [X] => (E[X], Cell[S]) => X)
  : Handler[E, R, A, B] = Stateful(Cell(init), ret, answer)

  /** the same from a cell of one's own kind, its `fresh` giving the same kind */
  def stateful[E[_], R[_], A, B, S](cell: Cell[S])(ret: (S, A) => B ! R)(answer: [X] => (E[X], Cell[S]) => X)
  : Handler[E, R, A, B] = Stateful(cell, ret, answer)

  // -- the machine's view of a handler

  /** a handler as it is installed, or put back by `k`: a stateful one with a cell of its own, from its state now */
  private[scala3] inline def fresh[E[_], T[_], A, B](h: Handler[E, T, A, B]): Handler[E, T, A, B] = h match
    case s: Handler.Stateful[e, t, a, b, st] => Handler.Stateful(s.cell.fresh, s.ret, s.answer)
    case _ => h

  /** the body's value given to the handler */
  private[scala3] inline def ret[E[_], T[_], A, B](h: Handler[E, T, A, B], a: A): Prog[T, B] = h match
    case g: Handler.General[e, t, a2, b] => g.ret(a)
    case t: Handler.Tail[e, t2, a2, b] => t.ret(a)
    case s: Handler.Stateful[e, t, a2, b, st] => s.ret(s.cell.state, a)
    case x: Handler.Abort[e, t, a2, b] => x.ret(a)
    case c: Handler.Control[e, t, a2, b] => c.ret(a)
    case k: Handler.Kept[e, t, a2, b] => retKept(k, a)

  private[scala3] def retKept[E[_], T[_], A, B](k: Handler.Kept[E, T, A, B], a: A): Prog[T, B] = ret(k.h, a)

  /** `h` as `intercept` installs it: one that can leave the rest, with the place its effect is kept at */
  private[scala3] def keeping[E[_], T[_], A, B](h: Handler[E, T, A, B], at: At[E, T, ?]): Handler[E, T, A, B] =
    h match
      case _: Handler.Tail[e, t, a, b] => h
      case _: Handler.Stateful[e, t, a, b, s] => h
      case _ => Handler.Kept(h, at)

  /** the place the handler's effect is kept at, outside it; `null` when it took its effect out of the row */
  private[scala3] def kept[E[_], T[_], A, B](h: Handler[E, T, A, B]): At[E, T, ?] | Null = h match
    case k: Handler.Kept[e, t, a, b] => k.at
    case _ => null
