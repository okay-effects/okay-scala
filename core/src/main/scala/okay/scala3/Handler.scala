package okay.scala3

// A HANDLER: what an effect's operations mean — four shapes, each run its own way by the machine — and a stateful
// one's cell

/** a stateful handler's state, read and written by its clause in place: `get` and `set` final, nothing virtual on
 * the way of an operation. `fresh` is the cell a handler is given when it is installed or put back by a `k` — a
 * subclass may carry another library's view of a cell, and give its own kind again */
class Cell[S](var state: S):
  final def get: S = state
  final def set(s: S): Unit = state = s
  def fresh: Cell[S] = Cell(state)

/**
 * A HANDLER OF `E`, the row outside it `R`, the body's value `A`, its own `B`: of one of four shapes, which the
 * machine runs each its own way. Each clause takes an operation `E[X]` as it is, and its match learns `X` from the
 * case — `case Get() => s` answers an `S`:
 * {{{
 * Handler.tail[Reader % String, R, A, A](pure)([X] => (op: Reader[String, X]) => op match
 *   case Reader.Ask() => "Ann")
 * }}}
 * Made by `Handler.general`, `tail`, `abort`, `stateful`, or by the cases themselves.
 */
enum Handler[E[_], R[_], A, B]:
  /** `k` the rest of the body, under this handler: resumed any number of times, or none */
  case General(ret: A => B ! R, op: [X] => (E[X], X => B ! R) => B ! R)
  /** an operation ANSWERED IN PLACE: the body goes on at once, no `k` made */
  case Tail(ret: A => B ! R, answer: [X] => E[X] => X)
  /** an operation ABORTS: its clause is the handler's result, the rest dropped, no `k` made */
  case Abort(ret: A => B ! R, abort: [X] => E[X] => B ! R)
  /** answered in place with a state of its own, `cell`: each time the handler is installed, or a `k` taken further
   * out resumes it, the machine gives it a cell of its own, from the state of its time */
  case Stateful[E[_], R[_], A, B, S](cell: Cell[S], ret: (S, A) => B ! R,
                                     answer: [X] => (E[X], Cell[S]) => X) extends Handler[E, R, A, B]

/** a handler of each shape, its value's clause first, its operations' after: `Handler.tail[Reader % String, R, A,
 * A](pure)([X] => (op: Reader[String, X]) => op match ...)`. Each gives a `Handler`, whatever its shape */
object Handler:
  /** `op` gets the operation and `k`, the rest of the body under this handler: resumed once, many times, or never */
  def general[E[_], R[_], A, B](ret: A => B ! R)(op: [X] => (E[X], X => B ! R) => B ! R): Handler[E, R, A, B] =
    General(ret, op)

  /** each operation answered in place: the body goes on at once with `answer`'s value, no `k` made */
  def tail[E[_], R[_], A, B](ret: A => B ! R)(answer: [X] => E[X] => X): Handler[E, R, A, B] = Tail(ret, answer)

  /** each operation aborts: `abort`'s program is the handler's result, the rest dropped, no `k` made */
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
