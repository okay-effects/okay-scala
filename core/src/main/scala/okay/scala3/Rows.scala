package okay.scala3

import scala.annotation.implicitNotFound

/*
 * THE ROWS, as okay-simple writes them — the same types: an effect is the GADT of its operations, a type
 * constructor (`Choose`, `State % Int`); a row is one too: an effect, `Pure`, or `R + E`, the effects summed left to
 * right. A program over the row `R` with the value `A` is `A ! R`:
 *
 *   Int ! Reader % String + State % Int
 *
 * (`!` binds loosest, `%` tightest). okay-simple's Scala 3 build takes `Sum`, `Void` and `Cell` from here, so a row
 * written there is the row written here, and its interface runs on this machine.
 */

/** the sum of the row `R` and the effect `E`: a phantom, of no operation of its own. The row `R + E` is `[X] Sum[R, E, X]` */
sealed trait Sum[R[_], E[_], X]

/** the empty effect: no operation — `Void` has no value, so an operation of it is absurd: it is anything */
sealed trait Void[X]:
  def absurd: Nothing

/** the row of no effect: no operation — `Void` has no value, so an operation of it is absurd */
type Pure[X] = Void[X]

/** the row `R` and the effect `E` after it */
infix type +[R[_], E[_]] = [X] =>> Sum[R, E, X]

/** the effect `F` with its parameter `S`: `State % Int`, its operations `State[Int, X]` */
infix type %[F[_, _], S] = [X] =>> F[S, X]

/** `E` is in `R`, somewhere: its place, found by given search; missing, a compile error naming both */
opaque type In[E[_], R[_]] = Place[E, R, ?]

/** `E` is in `R`, and `R` without it is `Rest` */
opaque type Split[E[_], R[_], Rest[_]] <: In[E, R] = Place[E, R, Rest]

object In extends InMissing:
  given found[E[_], R[_]](using at: Place[E, R, ?]): In[E, R] = at
  def apply[E[_], R[_]](at: Place[E, R, ?]): In[E, R] = at
  extension [E[_], R[_]](in: In[E, R]) def place: Place[E, R, ?] = in

trait InMissing:
  /** no place found: the error says which effect and which row, as written */
  inline given missing[E[_], R[_]]: In[E, R] = ${ Messages.notIn[E, R] }

object Split extends SplitFound:
  /** the row left as one would write it: `A + B` without `A` is `B` */
  given natural[E[_], R[_], Rest[_]](using at: Natural[E, R, Rest]): Split[E, R, Rest] = at
  def apply[E[_], R[_], Rest[_]](at: Place[E, R, Rest]): Split[E, R, Rest] = at
  extension [E[_], R[_], Rest[_]](s: Split[E, R, Rest]) def place: Place[E, R, Rest] = s

  /**
   * THE PLACE A HANDLER TAKES `E` FROM, the row left as one would write it: of `A + B` without `A`, `B` — not
   * `Pure + B`; of `A + B + C` without `A`, `B + C`. Found before the place as it is written (`Split.found`), which is
   * still what is found where that row is the one asked for. Evidence: nobody writes its name, so it is here and not
   * beside `Split`, where an `import okay.scala3.*` would bring it over a name of one's own.
   */
  opaque type Natural[E[_], R[_], Rest[_]] <: Place[E, R, Rest] = Place[E, R, Rest]

  /** the last effect before the first of two, so that an effect twice in a row is taken at its last place, as ever */
  object Natural extends NaturalFirst:
    given last[R[_], E[_]]: Natural[E, R + E, R] = Place.Last()
    def apply[E[_], R[_], Rest[_]](at: Place[E, R, Rest]): Natural[E, R, Rest] = at

  trait NaturalFirst extends NaturalInit:
    given first[E[_], G[_]]: Natural[E, E + G, G] = Natural(Place.First())

  trait NaturalInit:
    given init[E[_], R[_], G[_], Rest[_]](using at: Natural[E, R, Rest]): Natural[E, R + G, Rest + G] =
      Natural(Place.Init(at))

/** the layers of `Split`'s search are the library's own names: an `import okay.scala3.*` does not bring them */
private[scala3] trait SplitFound extends SplitMissing:
  /** the row left as it is written, the effect taken out — `Pure + B` of `A + B`, where that is what is asked */
  given found[E[_], R[_], Rest[_]](using at: Place[E, R, Rest]): Split[E, R, Rest] = Split(at)

trait SplitMissing:
  inline given missing[E[_], R[_], Rest[_]]: Split[E, R, Rest] = ${ Messages.notSplit[E, R, Rest] }

/** the row the compiler gives a program whose row nothing says */
private[scala3] type Unsaid[X] = Any

/** a context bound: `def tick[R[_]: Has[State % Int]]: Int ! R` */
type Has[E[_]] = [R[_]] =>> In[E, R]

/**
 * A PLACE OF `E` IN `R`, `R` without it `Rest`, of any representation: what the machine needs of one is `take` — an
 * operation's place compared with a handler's, the answer `Taken`, whose `Mine` proves by its type that the operation
 * is the handler's. This library's places are `Place`; another library's rows, of other classes, give the machine
 * places of their own (okay-simple's adapter does), each proving `Mine` where its own types show it: no cast.
 */
trait At[E[_], R[_], Rest[_]]:
  /** the operation's place `at`, in the same row, met by the handler of `E` here */
  def take[E2[_]](at: At[E2, R, ?]): Taken[E, E2, Rest]

  /** the operation's place outside this handler, if it is not the handler's; `null` if it is. What the machine asks
   * on its way out, level by level — no proof needed to go on, so a representation can answer making nothing; the
   * proof, `take`, it asks once, of the handler it stops at */
  def outside[E2[_]](at: At[E2, R, ?]): At[E2, Rest, ?] | Null = take(at) match
    case _: Taken.Mine[e, r] => null
    case t: Taken.Theirs[e, e2, r] => t.at

/** AN OPERATION PAST EVERY HANDLER, at the top of a run of the row `P`: it cannot be — a run's row has no effect, and
 * an operation of no effect has no value — and this says why, for the places of `P`'s representation */
trait Escape[P[_]]:
  def apply[E[_], X](at: At[E, P, ?], op: E[X]): Nothing

object Escape:
  /** this library's: past every handler, an operation is at `Pure`'s one place or the empty effect's — of `Void` */
  val pure: Escape[Pure] = new Escape[Pure]:
    def apply[E[_], X](at: At[E, Pure, ?], op: E[X]): Nothing = (at: @unchecked) match
      case _: Place.Only[e] => op.absurd
      case _: Place.Absurd[r] => op.absurd

/**
 * THE PLACE OF `E` IN `R`, `R` without it `Rest`: a path, typed by the row, found by given search — the evidence that
 * `E` is in `R`. The machine matches an operation's place against a handler's, and Scala's GADT inference learns from
 * the match that the operation is the handler's: no cast anywhere.
 */
@implicitNotFound("the effect ${E} is not in the row ${R}")
enum Place[E[_], R[_], Rest[_]] extends At[E, R, Rest]:
  /** the one effect of a row of one */
  case Only[E[_]]() extends Place[E, E, Pure]
  /** the last effect of `R + E` */
  case Last[R[_], E[_]]() extends Place[E, R + E, R]
  /** one further in than the last, `G` */
  case Init[E[_], R[_], G[_], Rest[_]](at: Place[E, R, Rest]) extends Place[E, R + G, Rest + G]
  /** the empty effect, in every row: `Void`, of no operation. Where `Pure`'s places go when a program of no effect
   * is put into a row (`into`); an operation is never there */
  case Absurd[R[_]]() extends Place[Void, R, R]
  /** the first effect of a row of two, the row without it the other effect, `G` — where `Init(Only())` leaves
   * `Pure + G`. A handler's place (`Natural`); an operation is found at `Init(Only())`, and the two are one place */
  case First[E[_], G[_]]() extends Place[E, E + G, G]

  def take[E2[_]](at: At[E2, R, ?]): Taken[E, E2, Rest] = Taken(this, at)
  override def outside[E2[_]](at: At[E2, R, ?]): At[E2, Rest, ?] | Null = Taken.outside(this, at)

/** the places found: a row of one, then the last effect, then one further in — in this order, so that an effect
 * twice in a row is found at its last place, nearest to the program */
object Place extends LastPlace:
  given only[E[_]]: Place[E, E, Pure] = Only()

trait LastPlace extends InitPlace:
  given last[R[_], E[_]]: Place[E, R + E, R] = Place.Last()

trait InitPlace:
  given init[E[_], R[_], G[_], Rest[_]](using at: Place[E, R, Rest]): Place[E, R + G, Rest + G] = Place.Init(at)

/**
 * EVERY EFFECT OF `R` IS IN `S`: a renaming of `R`'s places to `S`'s — what puts a program into a bigger row
 * (`into`), takes an effect to the end (`split`), or puts a handler in between (`intercept`). A function, not data.
 */
@implicitNotFound("the row ${R} is not in the row ${S}: an effect of it is missing there")
trait Sub[R[_], S[_]]:
  def apply[E[_]](at: At[E, R, ?]): At[E, S, ?]

object Sub extends SubMore:
  /** `Pure`'s one place — the empty effect's — to the empty effect's in `S` */
  given pure[S[_]]: Sub[Pure, S] = OfPure()

  /** the same row */
  def same[R[_]]: Sub[R, R] = Same()

  // THE RENAMINGS THE COMPILER FINDS, as data: so that one after another is one, no bigger than the row (`composed`)

  private[scala3] final class OfPure[S[_]] extends Sub[Pure, S]:
    def apply[E[_]](at: At[E, Pure, ?]): At[E, S, ?] = (at: @unchecked) match
      case _: Place.Only[e] => Place.Absurd[S]()
      case _: Place.Absurd[r] => Place.Absurd[S]()

  private[scala3] final class Same[R[_]] extends Sub[R, R]:
    def apply[E[_]](at: At[E, R, ?]): At[E, R, ?] = at

  private[scala3] final class OfUnsaid[S[_]] extends Sub[Unsaid, S]:
    def apply[E[_]](at: At[E, Unsaid, ?]): At[E, S, ?] =
      throw IllegalStateException("an operation of a row that nothing said")

  /** `R + E`: `R`'s places by `r`, and `E`'s, `e` */
  private[scala3] final class More[R[_], E[_], S[_]](val r: Sub[R, S], val e: At[E, S, ?]) extends Sub[R + E, S]:
    def apply[E2[_]](at: At[E2, R + E, ?]): At[E2, S, ?] = (at: @unchecked) match
      case _: Place.Last[r1, e1] => e
      case i: Place.Init[e2, r2, g, rest] => r(i.at)
      case _: Place.First[e2, g] => r(Place.Only[e2]())
      case _: Place.Absurd[r] => Place.Absurd[S]()

  /** a row of one: its effect's place, `e` */
  private[scala3] final class One[E[_], S[_]](val e: At[E, S, ?]) extends Sub[E, S]:
    def apply[E2[_]](at: At[E2, E, ?]): At[E2, S, ?] = (at: @unchecked) match
      case _: Place.Only[e] => e
      case _: Place.Absurd[r] => Place.Absurd[S]()

  /** a renaming, then another, kept as two: the first is of no shape known here — one's own, `split`'s */
  private[scala3] final class Both[R[_], S[_], T[_]](val first: Sub[R, S], val second: Sub[S, T]) extends Sub[R, T]:
    def apply[E[_]](at: At[E, R, ?]): At[E, T, ?] = second(first(at))

  /**
   * `first`, THEN `next`: one renaming for two, one directly inside the other — what a program put into a row in
   * tail position of one put into another runs under (`Cek`). Of the renamings found by the compiler it is as big
   * as the row `R`, however many were composed: each place of `R` is taken to its place in `T` once, now.
   */
  private[scala3] def composed[R[_], S[_], T[_]](first: Sub[R, S], next: Sub[S, T]): Sub[R, T] = first match
    case _: Same[r] => next
    case m: More[r, e, s] => More[r, e, T](composed[r, s, T](m.r, next), next[e](m.e))
    case o: One[e, s] => One[e, T](next[e](o.e))
    case _: OfPure[s] => OfPure[T]()
    case _: OfUnsaid[s] => OfUnsaid[T]()
    case b: Both[r, s0, s] => Both[r, s0, T](b.first, composed[s0, s, T](b.second, next))
    case _ => next match
      case _: Same[s] => first
      case _ => Both(first, next)

trait SubMore extends SubOne:
  /** the row of a `pure` or a `unit` that nothing said, in a branch beside words (`Branches`): no operation is of it */
  given unsaid[S[_]]: Sub[Unsaid, S] = Sub.OfUnsaid()

  /** `R + E`: `R`'s places and `E`'s */
  given more[R[_], E[_], S[_]](using r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = Sub.More(r, e.place)

trait SubOne:
  /** a row of one: its effect's place in `S` */
  given one[E[_], S[_]](using e: In[E, S]): Sub[E, S] = Sub.One(e.place)

/**
 * An operation at `at` in `R`, met by the handler of `E` at its place `mine` in `R`: the handler's own (`Mine`, so
 * `E2` is `E`), or another's (`Theirs`, its place in `Rest`, outside the handler).
 *
 * The matches over places are `@unchecked`: the checker cannot rule out an effect that is itself a row — an `E` that
 * is an `R + G` — as an effect is any constructor. No effect is a row, so the cases matched are all a place can be.
 */
enum Taken[E[_], E2[_], Rest[_]]:
  case Mine[E[_], Rest[_]]() extends Taken[E, E, Rest]
  case Theirs[E[_], E2[_], Rest[_]](at: At[E2, Rest, ?]) extends Taken[E, E2, Rest]

object Taken:
  /** the operation's place outside the handler at `mine`, or `null` if it is the handler's: nothing made on the way */
  def outside[E[_], R[_], Rest[_], E2[_]](mine: Place[E, R, Rest], at: At[E2, R, ?]): At[E2, Rest, ?] | Null =
    (mine: @unchecked) match
      case _: Place.Last[r, e] => (at: @unchecked) match
        case _: Place.Last[r2, e2] => null
        case i: Place.Init[e2, r2, g, rest] => i.at
        case _: Place.First[e2, g] => Place.Only[e2]()
        case _: Place.Absurd[r2] => Place.Absurd()
      case _: Place.Only[e] => (at: @unchecked) match
        case _: Place.Only[e2] => null
        case _: Place.Absurd[r2] => Place.Absurd()
      case i: Place.Init[e, r, g, rest] => (at: @unchecked) match
        case _: Place.Last[r2, e2] => Place.Last()
        case j: Place.Init[e2, r2, g2, rest2] => (outside(i.at, j.at): @unchecked) match
          case null => null
          case q: Place[E2, rest, ?] @unchecked => Place.Init(q)
        case _: Place.First[e2, g2] => (i.at: @unchecked) match
          case _: Place.Only[e3] => null
          case _: Place.Absurd[r3] => Place.First[e2, g2]()
        case _: Place.Absurd[r2] => Place.Absurd()
      case _: Place.First[e, g] => (at: @unchecked) match
        case _: Place.First[e2, g2] => null
        case _: Place.Last[r2, e2] => Place.Only[e2]()
        case j: Place.Init[e2, r2, g2, rest2] => (j.at: @unchecked) match
          case _: Place.Only[e3] => null
          case _: Place.Absurd[r3] => Place.Absurd()
        case _: Place.Absurd[r2] => Place.Absurd()
      case _: Place.Absurd[r] => at

  /** this library's places compared: the places of a row are all of one representation */
  def apply[E[_], R[_], Rest[_], E2[_]](mine: Place[E, R, Rest], at: At[E2, R, ?]): Taken[E, E2, Rest] =
    (mine: @unchecked) match
      case _: Place.Last[r, e] => (at: @unchecked) match
        case _: Place.Last[r2, e2] => Mine()
        case i: Place.Init[e2, r2, g, rest] => Theirs(i.at)
        case _: Place.First[e2, g] => Theirs(Place.Only[e2]())
        case _: Place.Absurd[r2] => Theirs(Place.Absurd())
      case _: Place.Only[e] => (at: @unchecked) match
        case _: Place.Only[e2] => Mine()
        case _: Place.Absurd[r2] => Theirs(Place.Absurd())
      case i: Place.Init[e, r, g, rest] => (at: @unchecked) match
        case _: Place.Last[r2, e2] => Theirs(Place.Last())
        case j: Place.Init[e2, r2, g2, rest2] => Taken(i.at, j.at) match
          case _: Mine[e3, r3] => Mine()
          case t: Theirs[e3, e4, r4] => (t.at: @unchecked) match
            case q: Place[e4, r4, ?] @unchecked => Theirs(Place.Init(q))
        case _: Place.First[e2, g2] => (i.at: @unchecked) match
          case _: Place.Only[e3] => Mine()
          case _: Place.Absurd[r3] => Theirs(Place.First[e2, g2]())
        case _: Place.Absurd[r2] => Theirs(Place.Absurd())
      case _: Place.First[e, g] => (at: @unchecked) match
        case _: Place.First[e2, g2] => Mine()
        case _: Place.Last[r2, e2] => Theirs(Place.Only[e2]())
        case j: Place.Init[e2, r2, g2, rest2] => (j.at: @unchecked) match
          case _: Place.Only[e3] => Mine()
          case _: Place.Absurd[r3] => Theirs(Place.Absurd())
        case _: Place.Absurd[r2] => Theirs(Place.Absurd())
      case _: Place.Absurd[r] => Theirs(at)

/** the renamings `split` and `intercept` make, each by one rule: the operation is `E`'s (`Taken.Mine`), or another's */
object Renamings:
  /** `R`'s places renamed to `Rest + E`'s: `E`'s from `at` to the end; another's to its place in `Rest`, one in */
  def toEnd[E[_], R[_], Rest[_]](at: Place[E, R, Rest]): Sub[R, Rest + E] = new Sub[R, Rest + E]:
    private val last = Place.Last[Rest, E]()
    def apply[E2[_]](p: At[E2, R, ?]): At[E2, Rest + E, ?] = Taken(at, p) match
      case _: Taken.Mine[e, r] => last
      case t: Taken.Theirs[e, e2, r] => further[E2, Rest, E](t.at)

  /** `R`'s places renamed to `R + E`'s: `E`'s at `at` to the new last place, another's where it is, one in — what
   * `intercept` handles, leaving `E`'s handler outside where it was */
  def interposing[E[_], R[_]](at: Place[E, R, ?]): Sub[R, R + E] = new Sub[R, R + E]:
    private val last = Place.Last[R, E]()
    def apply[E2[_]](p: At[E2, R, ?]): At[E2, R + E, ?] = Taken(at, p) match
      case _: Taken.Mine[e, r] => last
      case _: Taken.Theirs[e, e2, r] => further[E2, R, E](p)

  /** a place in `R` as a place in `R + G`: one further in — the empty effect's staying the empty effect's */
  private def further[E[_], R[_], G[_]](p: At[E, R, ?]): At[E, R + G, ?] = (p: @unchecked) match
    case _: Place.Absurd[r] => Place.Absurd[R + G]()
    case q: Place[E, R, ?] @unchecked => Place.Init(q)
