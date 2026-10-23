package okay.scala3

/** a program of the value `A` over the row `R`: `Int ! Reader % String + State % Int` */
infix type ![+A, R[_]] = Prog[R, A]

/**
 * A PROGRAM: data, run by the machine (`Cek`). Its own cases are the words below — `pure`, `flatMap`, `map`,
 * `defer`, `perform`, `handle`, `into`; `Resume` is a `k` resumed, made by the machine only.
 */
enum Prog[R[_], +A]:
  case Value[R[_], A](a: A) extends Prog[R, A]
  case FlatMap[R[_], X, A](p: Prog[R, X], f: X => Prog[R, A]) extends Prog[R, A]
  /** a value through `f`: one step, no program made of it */
  case Map[R[_], X, A](p: Prog[R, X], f: X => A) extends Prog[R, A]
  /** the program made when the run gets to it: a tail call, a loop in constant stack */
  case Defer[R[_], A](p: () => Prog[R, A]) extends Prog[R, A]
  /** an operation of `E`, at its place in `R` */
  case Perform[R[_], E[_], X](at: At[E, R, ?], op: E[X]) extends Prog[R, X]
  /** `p` with its `E`, at `at`, handled by `h`: the program outside at `Rest` */
  case Handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], at: At[E, R, Rest], h: Handler[E, Rest, A, B])
    extends Prog[Rest, B]
  /** `p` in another row, its places renamed */
  case Remap[R1[_], R2[_], A](p: Prog[R1, A], rename: Sub[R1, R2]) extends Prog[R2, A]
  /** `p`, and `release` when it ends, or when a handler aborting drops it (`ensuring`) */
  case Ensure[R[_], A](p: Prog[R, A], release: () => Unit) extends Prog[R, A]
  /** `k` resumed with `x`: the frames inside and the levels crossed put back, under the general handler `h` */
  case Resume[Rest[_], X, B, R0[_], V0, E[_], R[_], A](inner: Frames[R0, X, V0], crossed: Crossed[R0, V0, R, A],
                                                       at: At[E, R, Rest], h: Handler[E, Rest, A, B], x: X) extends Prog[Rest, B]

/** σ, THE PURE CONTINUATION: the binds from a value `X` to the value `V` of the nearest handler's body, at `R` */
enum Frames[R[_], -X, +V]:
  case End[R[_], V]() extends Frames[R, V, V]
  case Bind[R[_], X, Y, V](f: X => Prog[R, Y], rest: Frames[R, Y, V]) extends Frames[R, X, V]
  case Map[R[_], X, Y, V](f: X => Y, rest: Frames[R, Y, V]) extends Frames[R, X, V]
  /** `release` run as a value passes, or as the frames are dropped by a handler aborting */
  case Finally[R[_], X, V](release: () => Unit, rest: Frames[R, X, V]) extends Frames[R, X, V]

/** the levels a `k` took, from the innermost's body at `RIn` of `VIn` to the outermost's at `ROut` of `VOut`: data, put
 * back by `Cek.put` wherever `k` is resumed */
enum Crossed[RIn[_], VIn, ROut[_], VOut]:
  case Nothing[R[_], V]() extends Crossed[R, V, R, V]
  case Handled[RIn[_], VIn, E[_], R[_], Rest[_], A, B, W](inner: Crossed[RIn, VIn, R, A], at: At[E, R, Rest],
                                                          h: Handler[E, Rest, A, B], outside: Frames[Rest, B, W]) extends Crossed[RIn, VIn, Rest, W]
  case Renamed[RIn[_], VIn, R1[_], R2[_], A, W](inner: Crossed[RIn, VIn, R1, A], rename: Sub[R1, R2],
                                                outside: Frames[R2, A, W]) extends Crossed[RIn, VIn, R2, W]

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

// -- THE WORDS

def pure[A, R[_]](a: A): A ! R = Prog.Value(a)

/** the value `()` */
def unit[R[_]]: Unit ! R = Prog.Value(())

/** an operation of `E`: `perform[State % Int](State.Get())` — the effect said, the answer from the operation, the row
 * from what is expected or from the evidence in scope. (Scala gives all of a method's type arguments or none, so the
 * rest is `Performing`'s; it is nothing at run time.) */
inline def perform[E[_]]: Performing[E] = Performing[E]

opaque type Performing[E[_]] = Unit

object Performing:
  inline def apply[E[_]]: Performing[E] = ()
  extension [E[_]](p: Performing[E])
    inline def apply[R[_], X](op: E[X])(using at: In[E, R]): X ! R = Prog.Perform(at.place, op)

/** `p`, made when the run gets to it: a tail call — `even` calling `odd` calling `even` runs in constant stack */
def defer[A, R[_]](p: => A ! R): A ! R = Prog.Defer(() => p)

/** a loop: `f` from `a`, again from its `Left`, until its `Right` — in constant stack, whatever `f` performs */
def loop[A, B, R[_]](a: A)(f: A => Either[A, B] ! R): B ! R =
  Prog.FlatMap(f(a), {
    case Left(next) => loop(next)(f)
    case Right(b) => Prog.Value(b)
  })

extension [A, R[_]](p: A ! R)
  def flatMap[B](f: A => B ! R): B ! R = Prog.FlatMap(p, f)
  def map[B](f: A => B): B ! R = Prog.Map(p, f)
  def as[B](b: B): B ! R = Prog.Map(p, _ => b)
  def andThen[B](q: => B ! R): B ! R = Prog.FlatMap(p, _ => q)
  /** this program in a row of more effects */
  def into[S[_]](using s: Sub[R, S]): A ! S = Prog.Remap(p, s)

/** `E` handled by `h` from wherever it is in `p`'s row: the program over the rest */
def handle[E[_], R[_], Rest[_], A, B](p: A ! R, h: Handler[E, Rest, A, B])(using at: Split[E, R, Rest])
: B ! Rest = Prog.Handle(p, at.place, h)

/**
 * `E` INTERCEPTED in `p`: each operation of `E` given to `h` first, with `k` the rest of `p`, still intercepted. The
 * row stays `R`, `E` in it: the clause runs outside the interception, so an operation of `E` it performs — the one it
 * was given, passed on, or any other — goes to `E`'s handler further out. To observe, change, refuse, answer here:
 * control and audit of an effect, the program not changed
 */
def intercept[E[_], R[_], A, B](p: A ! R, h: Handler[E, R, A, B])(using at: In[E, R]): B ! R =
  Prog.Handle(Prog.Remap(p, Renamings.interposing(at.place)), Place.Last[R, E](), h)

/** the program `p` with `E`, from wherever it is, at the end of its row */
def split[E[_], R[_], Rest[_], A](p: A ! R)(using at: Split[E, R, Rest]): A ! (Rest + E) = (at.place: @unchecked) match
  case _: Place.Last[r, e] => p
  case _ => Prog.Remap(p, Renamings.toEnd(at.place))

/**
 * `p`, and `release` after it: when it ends, or when a handler that aborts (`Fail`, `Abort`) drops it — innermost
 * first. A general handler that drops the rest by not calling `k` drops it unseen, so its `release` does not run; one
 * that calls `k` twice runs it at each end.
 */
def ensuring[A, R[_]](p: A ! R)(release: => Unit): A ! R = Prog.Ensure(p, () => release)

/** `acquire`d, used, `release`d after the use: `ensuring` of the use */
def bracket[S, A, R[_]](acquire: S ! R)(use: S => A ! R)(release: S => Unit): A ! R =
  acquire.flatMap(s => ensuring(use(s))(release(s)))

/** each operation of `E` in `p` seen by `see` — a program of the row, run before the operation is passed on as it
 * is: an audit trail, a count, a trace */
def observe[E[_], R[_], A](p: A ! R)(see: [X] => E[X] => Unit ! R)(using at: In[E, R]): A ! R =
  intercept(p, Handler.general[E, R, A, A](pure)([X] => (op: E[X], k: X => A ! R) =>
    see[X](op).flatMap(_ => perform[E](op)(using at).flatMap(k))))

/** around each operation of `E` in `p`: given it and `proceed`, its way on as it is, the program in its place —
 * `proceed`, something before or after it, another operation, or an answer of its own and no `proceed` */
def around[E[_], R[_], A](p: A ! R)(f: [X] => (E[X], X ! R) => X ! R)(using at: In[E, R]): A ! R =
  intercept(p, Handler.general[E, R, A, A](pure)([X] => (op: E[X], k: X => A ! R) =>
    f[X](op, perform[E](op)(using at)).flatMap(k)))

/** a program of no effect, run */
def run[A](p: A ! Pure): A = Cek.run(p)
