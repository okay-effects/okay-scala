package okay.scala3

/*
 * THE STANDARD EFFECTS: each an enum of its operations, its words, its handler. A word finds its row from the
 * evidence in scope — `def tick[R[_]: Has[State % Int]]` lets `State.get` and `State.put(n)` say nothing more — or
 * from what is expected; with two states in one row, say which: `State.get[Int, R]`.
 */

/** a state of type `S`, read and written */
enum State[S, X]:
  case Get[S]() extends State[S, S]
  case Put[S](s: S) extends State[S, Unit]

object State:
  def get[S, R[_]](using In[State % S, R]): S ! R = perform[State % S](Get())
  def put[S, R[_]](s: S)(using In[State % S, R]): Unit ! R = perform[State % S](Put(s))
  def modify[S, R[_]](f: S => S)(using In[State % S, R]): Unit ! R = get[S, R].flatMap(s => put(f(s)))

  /** the program from `init`: the state it ends with, and its value */
  def run[S, R[_], Rest[_], A](init: S)(p: A ! R)(using Split[State % S, R, Rest]): (S, A) ! Rest =
    handle(p, Handler.stateful[State % S, Rest, A, (S, A), S](init)((s, a) => pure((s, a)))(
      [X] => (op: State[S, X], cell: Cell[S]) => op match
        case Get() => cell.get
        case Put(s) => cell.set(s)))

/** an environment of type `E`, read */
enum Reader[E, X]:
  case Ask[E]() extends Reader[E, E]

object Reader:
  def ask[E, R[_]](using In[Reader % E, R]): E ! R = perform[Reader % E](Ask())

  def run[E, R[_], Rest[_], A](env: E)(p: A ! R)(using Split[Reader % E, R, Rest]): A ! Rest =
    handle(p, Handler.tail[Reader % E, Rest, A, A](pure)([X] => (op: Reader[E, X]) => op match
      case Ask() => env))

/** an output of type `W`, written */
enum Writer[W, X]:
  case Tell[W](w: W) extends Writer[W, Unit]

object Writer:
  def tell[W, R[_]](w: W)(using In[Writer % W, R]): Unit ! R = perform[Writer % W](Tell(w))

  /** what the program wrote, in order, and its value */
  def run[W, R[_], Rest[_], A](p: A ! R)(using Split[Writer % W, R, Rest]): (List[W], A) ! Rest =
    handle(p, Handler.stateful[Writer % W, Rest, A, (List[W], A), List[W]](Nil)((ws, a) => pure((ws.reverse, a)))(
      [X] => (op: Writer[W, X], cell: Cell[List[W]]) => op match
        case Tell(w) => cell.set(w :: cell.get)))

/** failure with an error of type `E`: the rest of the program dropped */
enum Fail[E, X]:
  case Raise[E](e: E) extends Fail[E, Nothing]

object Fail:
  def raise[E, R[_]](e: E)(using In[Fail % E, R]): Nothing ! R = perform[Fail % E](Raise(e))

  /** the value, or the error raised */
  def run[E, R[_], Rest[_], A](p: A ! R)(using Split[Fail % E, R, Rest]): Either[E, A] ! Rest =
    handle(p, Handler.abort[Fail % E, Rest, A, Either[E, A]](a => pure(Right(a)))([X] => (op: Fail[E, X]) => op match
      case Raise(e) => pure(Left(e))))

/** a choice among values: its handler resumes the rest once for each — MULTI-SHOT */
enum Choose[X]:
  case Among[X](xs: List[X]) extends Choose[X]

object Choose:
  def among[X, R[_]](xs: List[X])(using In[Choose, R]): X ! R = perform[Choose](Among(xs))
  def flip[R[_]](using In[Choose, R]): Boolean ! R = among(List(true, false))
  /** no value: this way gives nothing */
  def none[X, R[_]](using In[Choose, R]): X ! R = among(Nil)

  /** every value of every way the choices can go, in order */
  def all[R[_], Rest[_], A](p: A ! R)(using Split[Choose, R, Rest]): List[A] ! Rest =
    handle(p, Handler.general[Choose, Rest, A, List[A]](a => pure(List(a)))([X] => (op: Choose[X], k: X => List[A] ! Rest) =>
      op match
        case Among(xs) => collect(xs, k, Nil)))

  /** `k` resumed for each value in turn, the lists joined: a loop of the program, no stack */
  private def collect[X, A, R[_]](xs: List[X], k: X => List[A] ! R, done: List[A]): List[A] ! R = xs match
    case Nil => pure(done)
    case x :: rest => k(x).flatMap(as => collect(rest, k, done ++ as))

/** a generator's values, each given out: the program goes on when it is taken */
enum Yield[O, X]:
  case Emit[O](o: O) extends Yield[O, Unit]

object Yield:
  def emit[O, R[_]](o: O)(using In[Yield % O, R]): Unit ! R = perform[Yield % O](Emit(o))

  /** every value emitted, in order, and the program's value */
  def collect[O, R[_], Rest[_], A](p: A ! R)(using Split[Yield % O, R, Rest]): (List[O], A) ! Rest =
    handle(p, Handler.stateful[Yield % O, Rest, A, (List[O], A), List[O]](Nil)((os, a) => pure((os.reverse, a)))(
      [X] => (op: Yield[O, X], cell: Cell[List[O]]) => op match
        case Emit(o) => cell.set(o :: cell.get)))

  /** each value given to `f`, a program of the row outside: the generator paused while it runs */
  def foreach[O, R[_], Rest[_], A](p: A ! R)(using Split[Yield % O, R, Rest])(f: O => Unit ! Rest): A ! Rest =
    handle(p, Handler.general[Yield % O, Rest, A, A](pure)([X] => (op: Yield[O, X], k: X => A ! Rest) => op match
      case Emit(o) => f(o).flatMap(k)))
