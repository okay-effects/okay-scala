package okay.scala3

import scala.annotation.targetName
import scala.language.implicitConversions

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

/** beside the conversion of a word, THE EVIDENCE A BIND ASKS FOR (`Fits`, `Known`): given search finds it here, and
 * nobody writes its names — so they are not beside `Prog`, where an `import okay.scala3.*` would bring them over a
 * name of one's own */
object Prog:
  /** a word is a program of any row that has its effect: its place is found where the row is known */
  implicit def placed[E[_], X, R[_]](w: Word[E, X])(using at: In[E, R]): X ! R = w.into

  /**
   * `P` IS A PROGRAM OF THE ROW `R`, to a `V`: as it is — and then this is nothing at run time; or a word, by its
   * effect's place; or a program of a smaller row, put into `R` as `into` does. What a bind asks of its program, and
   * a branch of each of its parts (`Branches`).
   */
  trait Fits[-P, R[_], +V]:
    def apply(p: P): V ! R

  object Fits extends FitsInto:
    given same[R[_], A]: Fits[A ! R, R, A] = p => p
    given word[E[_], X, R[_]](using at: In[E, R]): Fits[Word[E, X], R, X] = w => w.into

  trait FitsInto:
    given smaller[R[_], S[_], A](using sub: Sub[R, S]): Fits[A ! R, S, A] = p => Fits.embedded(p, sub)

    /** `p`, its places those of `S`: a value, or an operation alone, made anew at its place there; any other program
     * under a level that renames what passes it (`Remap`) */
    private[scala3] def embedded[R[_], S[_], A](p: A ! R, sub: Sub[R, S]): A ! S = p match
      case v: Prog.Value[r, a] => Prog.Value(v.a)
      case o: Prog.Perform[r, e, x] => Prog.Perform(sub(o.at), o.op)
      case _ => Prog.Remap(p, sub)

  /** THE EFFECT `E` AS THE EVIDENCE IN SCOPE HAS IT. A word bound in a program generic in its row — `State.get` under
   * `R[_]: Has[State % Int]` — takes its `Int` from here, before the function that follows it is read; with no
   * evidence in scope it is found anyway, and says nothing. Nothing at run time */
  opaque type Known[E[_]] = Unit

  object Known:
    inline given known[E[_], R[_]](using inline at: In[E, R]): Known[E] = ()

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
  /** A BIND. Its row `S` is the one expected of it — the declared type of the program it is in; with none expected,
   * the row of what `f` gives; or else this program's own. This program need not be of that row: one of a smaller
   * row is put into it, as `into` does (`Fits`) — under a level that renames its operations on their way out, which
   * a program of the row itself, or generic in its row, does not pay */
  def flatMap[S[_], B](f: A => B ! S)(using fits: Prog.Fits[A ! R, S, A]): B ! S = Prog.FlatMap(fits(p), f)
  /** a value through `f`; its row is the one expected of it, or this program's own */
  def map[S[_], B](f: A => B)(using fits: Prog.Fits[A ! R, S, A]): B ! S = Prog.Map(fits(p), f)
  def as[B](b: B): B ! R = Prog.Map(p, _ => b)
  def andThen[B](q: => B ! R): B ! R = Prog.FlatMap(p, _ => q)
  /** this program in a row of more effects */
  def into[S[_]](using s: Sub[R, S]): A ! S = Prog.Remap(p, s)

/**
 * THE BINDS OF 0.1.1, for what was compiled against it: there `flatMap` and `map` took the program and the function
 * and no evidence, the row one. They are kept under their names in the bytecode (`targetName`), and are the binds
 * above with the evidence of the same row. In source they have other names, and are the library's own: a program
 * written now gets the binds above.
 */
extension [A, R[_]](p: A ! R)
  @targetName("flatMap")
  private[scala3] def flatMapOfTheSameRow[B](f: A => B ! R): B ! R = p.flatMap(f)(using Prog.Fits.same)
  @targetName("map")
  private[scala3] def mapOfTheSameRow[B](f: A => B): B ! R = p.map(f)(using Prog.Fits.same)

/**
 * AN OPERATION, NOT YET PLACED: a word of an effect — `State.get[Int]`, `Choose.among(List(1, 2))`. It says no row:
 * the row is the program's, known where the word is used, and there the compiler finds the effect's place in it. So a
 * word is bound (`flatMap`, `map`: a `for`) into a program of the row expected, is a program where one is expected,
 * and `into[R]` says the row. At run time it is the operation itself: nothing is made for it.
 */
opaque type Word[E[_], X] <: Unplaced[X] = E[X]

/** what a word is as far as its answer goes: branches of words come to one type by it (`Branches`). The library's own
 * name: an `import okay.scala3.*` does not bring it */
private[scala3] opaque type Unplaced[+X] = Any

/** the word of an operation: `def deposit(n: Int) = word(Accounts.Deposit(n))` — no row, no evidence */
def word[E[_], X](op: E[X]): Word[E, X] = op

/** The order of a bind's clauses is the order the compiler learns in: the effect's own type from the evidence in
 * scope (`Prog.Known`), so that `f`'s argument has its type; then `f`, and with it or with what is expected the row;
 * then the effect's place in that row. */
extension [E[_], X](w: Word[E, X])
  def flatMap[R[_], B](using Prog.Known[E])(f: X => B ! R)(using at: In[E, R]): B ! R =
    Prog.FlatMap(Prog.Perform(at.place, w), f)
  def map[R[_], B](using Prog.Known[E])(f: X => B)(using at: In[E, R]): B ! R = Prog.Map(Prog.Perform(at.place, w), f)
  def as[R[_], B](b: B)(using at: In[E, R]): B ! R = Prog.Map(Prog.Perform(at.place, w), _ => b)
  def andThen[R[_], B](q: => B ! R)(using at: In[E, R]): B ! R = Prog.FlatMap(Prog.Perform(at.place, w), _ => q)
  /** this word as a program of the row `R` */
  def into[R[_]](using at: In[E, R]): X ! R = Prog.Perform(at.place, w)

/** a word or a program in every branch of an `if` or a `match` that is bound (`Branches`) */
extension [V](inline p: Unplaced[V] | Prog[?, V])
  inline def flatMap[R[_], B](f: V => B ! R): B ! R = ${ Branches.flatMap[R, V, B]('p, 'f) }
  inline def map[R[_], B](f: V => B): B ! R = ${ Branches.map[R, V, B]('p, 'f) }

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
