package okay.scala3

/** a program of the value `A` over the row `R`: `Int ! Reader % String + State % Int` */
infix type ![+A, R[_]] = Prog[R, A]

/** a stateful handler's state, read and written by its clause in place: a final class, nothing virtual */
final class Cell[S](var state: S):
  def get: S = state
  def set(s: S): Unit = state = s

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
  /** `k` resumed with `x`: the frames inside and the levels crossed put back, under the general handler `h` */
  case Resume[Rest[_], X, B, R0[_], V0, E[_], R[_], A](inner: Frames[R0, X, V0], crossed: Crossed[R0, V0, R, A],
    at: At[E, R, Rest], h: Handler[E, Rest, A, B], x: X) extends Prog[Rest, B]

/**
 * A HANDLER OF `E`, the row outside it `R`, the body's value `A`, its own `B`: of one of four shapes, which the
 * machine runs each its own way. Each clause takes an operation `E[X]` as it is, and its match learns `X` from the
 * case — `case Get() => s` answers an `S`:
 * {{{
 * Handler.Tail(pure, [X] => (op: Reader[String, X]) => op match
 *   case Reader.Ask() => "Ann")
 * }}}
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

object Handler:
  /** a stateful handler from the state `init` */
  def stateful[E[_], R[_], A, B, S](init: S)(ret: (S, A) => B ! R)(answer: [X] => (E[X], Cell[S]) => X)
    : Handler[E, R, A, B] = Stateful(Cell(init), ret, answer)

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

/** a program of no effect, run */
def run[A](p: A ! Pure): A = Cek.run(p)
