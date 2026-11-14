package okay.scala3.std

import okay.scala3.*
import scala.annotation.unused

/*
 * THE STANDARD EFFECTS: each an enum of its operations, its words, its handler. A word finds its row from the
 * evidence in scope — `def tick[R[_]: Has[State % Int]]` lets `State.get` and `State.put(n)` say nothing more — or
 * from what is expected; with two states in one row, say which: `State.get[Int, R]`.
 */

/** a state of type `S`, read and written */
enum State[S, X]:
  case Get[S]() extends State[S, S]
  case Put[S](s: S) extends State[S, Unit]

/** `State.get[Int]`: the word, not yet placed in a row — the row is the program's, known where it is used */
private[std] trait StateWords:
  def get[S]: Word[State % S, S] = word(State.Get())

object State extends StateWords:
  def get[S, R[_]](using In[State % S, R]): S ! R = perform[State % S](Get())
  def put[S, R[_]](s: S)(using In[State % S, R]): Unit ! R = perform[State % S](Put(s))
  def modify[S, R[_]](f: S => S)(using In[State % S, R]): Unit ! R = get[S, R].flatMap(s => put(f(s)))
  /** the state through `f` */
  def gets[S, A, R[_]](f: S => A)(using In[State % S, R]): A ! R = get[S, R].map(f)

  /** the program from `init`: the state it ends with, and its value */
  def run[S, R[_], Rest[_], A](init: S)(p: A ! R)(using Split[State % S, R, Rest]): (S, A) ! Rest =
    handle(p, Handler.stateful[State % S, Rest, A, (S, A), S](init)((s, a) => pure((s, a)))(
      [X] => (op: State[S, X], cell: Cell[S]) => op match
        case Get() => cell.get
        case Put(s) => cell.set(s)))

/** an environment of type `E`, read */
enum Reader[E, X]:
  case Ask[E]() extends Reader[E, E]

/** `Reader.ask[String]`: the word, not yet placed in a row */
private[std] trait ReaderWords:
  def ask[E]: Word[Reader % E, E] = word(Reader.Ask())

object Reader extends ReaderWords:
  def ask[E, R[_]](using In[Reader % E, R]): E ! R = perform[Reader % E](Ask())

  def run[E, R[_], Rest[_], A](env: E)(p: A ! R)(using Split[Reader % E, R, Rest]): A ! Rest =
    handle(p, Handler.tail[Reader % E, Rest, A, A](pure)([X] => (op: Reader[E, X]) => op match
      case Ask() => env))

  /** `p` with the environment through `f`: its asks answered here, from the environment outside; the row stays */
  def local[E, R[_], Rest[_], A](f: E => E)(p: A ! R)(using Split[Reader % E, R, Rest], Sub[Rest, R]): A ! R =
    ask[E, R].flatMap(e => run(f(e))(p).into[R])

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

  /** what `p` wrote, with its value — written outside too, when `p` ends */
  def listen[W, R[_], Rest[_], A](p: A ! R)(
    using split: Split[Writer % W, R, Rest], sub: Sub[Rest, R], @unused in: In[Writer % W, R])
    : (List[W], A) ! R =
    run[W, R, Rest, A](p).into[R].flatMap((ws, a) => tellAll[W, R](ws).as((ws, a)))

  /** what `p` wrote, through `f`, written outside when `p` ends */
  def censor[W, R[_], Rest[_], A](f: List[W] => List[W])(p: A ! R)(
    using split: Split[Writer % W, R, Rest], sub: Sub[Rest, R], @unused in: In[Writer % W, R]): A ! R =
    run[W, R, Rest, A](p).into[R].flatMap((ws, a) => tellAll[W, R](f(ws)).as(a))

  private def tellAll[W, R[_]](ws: List[W])(using In[Writer % W, R]): Unit ! R =
    ws.foldLeft(unit[R])((p, w) => p.andThen(tell[W, R](w)))

/** failure with an error of type `E`: the rest of the program left — unwound, its finalizers run (`ensuring`) */
enum Fail[E, X]:
  case Raise[E](e: E) extends Fail[E, Nothing]

object Fail:
  def raise[E, R[_]](e: E)(using In[Fail % E, R]): Nothing ! R = perform[Fail % E](Raise(e))

  /** the value, or the error raised */
  def run[E, R[_], Rest[_], A](p: A ! R)(using Split[Fail % E, R, Rest]): Either[E, A] ! Rest =
    handle(p, Handler.abort[Fail % E, Rest, A, Either[E, A]](a => pure(Right(a)))([X] => (op: Fail[E, X]) => op match
      case Raise(e) => pure(Left(e))))

  /** the value, or `h` of what was raised, in its place: the rest left, `h` in the row without this `Fail` — so
   * one raised there goes to the next handler of it. `h` is the handler's own clause: a failure of a finalizer,
   * raised while a handler outside unwinds this one, is answered by it there */
  def recover[E, R[_], Rest[_], A](p: A ! R)(h: E => A ! Rest)(using Split[Fail % E, R, Rest]): A ! Rest =
    handle(p, Handler.abort[Fail % E, Rest, A, A](pure)([X] => (op: Fail[E, X]) => op match
      case Raise(e) => h(e)))

/** an early end with no value: the rest left — unwound — `optional` giving `None` */
enum Abort[X]:
  case Exit[X]() extends Abort[X]

/** `Abort.exit[Int]`: the word, not yet placed in a row */
private[std] trait AbortWords:
  def exit[X]: Word[Abort, X] = word(Abort.Exit[X]())

object Abort extends AbortWords:
  /** the program ended here, with no value */
  def exit[X, R[_]](using In[Abort, R]): X ! R = perform[Abort](Exit[X]())

  /** the value, or `None` if the program ended early */
  def optional[R[_], Rest[_], A](p: A ! R)(using Split[Abort, R, Rest]): Option[A] ! Rest =
    handle(p, Handler.abort[Abort, Rest, A, Option[A]](a => pure(Some(a)))([X] => (_: Abort[X]) => pure(None)))

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

  /** the first `n` values, and the program's value if it ended by then: stopped at the value after them, and left
   * there — unwound (`ensuring`); what a finalizer emits then nobody takes, and it ends there */
  def take[O, R[_], Rest[_], A](n: Int)(p: A ! R)(using Split[Yield % O, R, Rest]): (List[O], Option[A]) ! Rest =
    // the handler's value: a program given how many are still to take, and those taken, reversed
    type Go = (Int, List[O]) => (List[O], Option[A]) ! Rest
    handle(p, Handler.control[Yield % O, Rest, A, Go](a => pure((_, taken) => pure((taken.reverse, Some(a)))))(
      [X] => (op: Yield[O, X], k: Continuation[X, Rest, Go]) => op match
        case Emit(o) => pure[Go, Rest]((left, taken) =>
          if left <= 0 then k.unwind.map(_ => (taken.reverse, None))
          else k(()).flatMap(go => go(left - 1, o :: taken)))))
      .flatMap(go => go(n, Nil))
