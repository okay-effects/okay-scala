package okay.scala3

import okay.effects.{Sum, Void}
import scala.annotation.implicitNotFound

/*
 * THE ROWS, as okay-simple writes them — the same types: an effect is the GADT of its operations, a type
 * constructor (`Choose`, `State % Int`); a row is one too: an effect, `Pure`, or `R + E`, the effects summed left to
 * right. A program over the row `R` with the value `A` is `A ! R`:
 *
 *   Int ! Reader % String + State % Int
 *
 * (`!` binds loosest, `%` tightest). `Sum` and `Void` are okay-simple's own classes, so a row written here is the row
 * written there, and a program of one is the program of the other on this machine (module `simple`).
 */

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

object Split extends SplitMissing:
  given found[E[_], R[_], Rest[_]](using at: Place[E, R, Rest]): Split[E, R, Rest] = at
  extension [E[_], R[_], Rest[_]](s: Split[E, R, Rest]) def place: Place[E, R, Rest] = s

trait SplitMissing:
  inline given missing[E[_], R[_], Rest[_]]: Split[E, R, Rest] = ${ Messages.notIn[E, R] }

/** a context bound: `def tick[R[_]: Has[State % Int]]: Int ! R` */
type Has[E[_]] = [R[_]] =>> In[E, R]

/**
 * THE PLACE OF `E` IN `R`, `R` without it `Rest`: a path, typed by the row, found by given search — the evidence that
 * `E` is in `R`. The machine matches an operation's place against a handler's, and Scala's GADT inference learns from
 * the match that the operation is the handler's: no cast anywhere.
 */
@implicitNotFound("the effect ${E} is not in the row ${R}")
enum Place[E[_], R[_], Rest[_]]:
  /** the one effect of a row of one */
  case Only[E[_]]() extends Place[E, E, Pure]
  /** the last effect of `R + E` */
  case Last[R[_], E[_]]() extends Place[E, R + E, R]
  /** one further in than the last, `G` */
  case Init[E[_], R[_], G[_], Rest[_]](at: Place[E, R, Rest]) extends Place[E, R + G, Rest + G]
  /** the empty effect, in every row: `Void`, of no operation. Where `Pure`'s places go when a program of no effect
   * is put into a row (`into`); an operation is never there */
  case Absurd[R[_]]() extends Place[Void, R, R]

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
  def apply[E[_]](at: Place[E, R, ?]): Place[E, S, ?]

object Sub extends SubMore:
  /** `Pure`'s one place — the empty effect's — to the empty effect's in `S` */
  given pure[S[_]]: Sub[Pure, S] = new Sub[Pure, S]:
    def apply[E[_]](at: Place[E, Pure, ?]): Place[E, S, ?] = (at: @unchecked) match
      case _: Place.Only[e] => Place.Absurd[S]()
      case _: Place.Absurd[r] => Place.Absurd[S]()

  /** the same row */
  def same[R[_]]: Sub[R, R] = new Sub[R, R]:
    def apply[E[_]](at: Place[E, R, ?]): Place[E, R, ?] = at

trait SubMore extends SubOne:
  /** `R + E`: `R`'s places and `E`'s */
  given more[R[_], E[_], S[_]](using r: Sub[R, S], e: In[E, S]): Sub[R + E, S] = new Sub[R + E, S]:
    def apply[E2[_]](at: Place[E2, R + E, ?]): Place[E2, S, ?] = (at: @unchecked) match
      case _: Place.Last[r1, e1] => e.place
      case i: Place.Init[e2, r2, g, rest] => r(i.at)
      case _: Place.Absurd[r] => Place.Absurd[S]()

trait SubOne:
  /** a row of one: its effect's place in `S` */
  given one[E[_], S[_]](using e: In[E, S]): Sub[E, S] = new Sub[E, S]:
    def apply[E2[_]](at: Place[E2, E, ?]): Place[E2, S, ?] = (at: @unchecked) match
      case _: Place.Only[e] => e.place
      case _: Place.Absurd[r] => Place.Absurd[S]()

/**
 * An operation at `at` in `R`, met by the handler of `E` at its place `mine` in `R`: the handler's own (`Mine`, so
 * `E2` is `E`), or another's (`Theirs`, its place in `Rest`, outside the handler).
 *
 * The matches over places are `@unchecked`: the checker cannot rule out an effect that is itself a row — an `E` that
 * is an `R + G` — as an effect is any constructor. No effect is a row, so the cases matched are all a place can be.
 */
enum Taken[E[_], E2[_], Rest[_]]:
  case Mine[E[_], Rest[_]]() extends Taken[E, E, Rest]
  case Theirs[E[_], E2[_], Rest[_]](at: Place[E2, Rest, ?]) extends Taken[E, E2, Rest]

object Taken:
  def apply[E[_], R[_], Rest[_], E2[_]](mine: Place[E, R, Rest], at: Place[E2, R, ?]): Taken[E, E2, Rest] =
    (mine: @unchecked) match
      case _: Place.Last[r, e] => (at: @unchecked) match
          case _: Place.Last[r2, e2] => Mine()
          case i: Place.Init[e2, r2, g, rest] => Theirs(i.at)
          case _: Place.Absurd[r2] => Theirs(Place.Absurd())
      case _: Place.Only[e] => (at: @unchecked) match
          case _: Place.Only[e2] => Mine()
          case _: Place.Absurd[r2] => Theirs(Place.Absurd())
      case i: Place.Init[e, r, g, rest] => (at: @unchecked) match
          case _: Place.Last[r2, e2] => Theirs(Place.Last())
          case j: Place.Init[e2, r2, g2, rest2] => Taken(i.at, j.at) match
              case _: Mine[e3, r3] => Mine()
              case t: Theirs[e3, e4, r4] => Theirs(Place.Init(t.at))
          case _: Place.Absurd[r2] => Theirs(Place.Absurd())
      case _: Place.Absurd[r] => Theirs(at)

/** renamings the machine and `simple` make: `E` taken to the end; `E` put at a new last place */
object Renamings:
  /** `R`'s places renamed to `Rest + E`'s: `E` from `at` to the end, the others in their order */
  def toEnd[E[_], R[_], Rest[_]](at: Place[E, R, Rest]): Sub[R, Rest + E] = (at: @unchecked) match
    case _: Place.Only[e] => onlyToEnd[E]
    case _: Place.Last[r, e] => Sub.same[R]
    case i: Place.Init[e, r, g, rest] => initToEnd[E, r, g, rest](i.at)
    case _: Place.Absurd[r] => further[R]

  /** `R`'s places renamed to `R + E`'s: the operations of `E` at `at` to the new last place, the others where they
   * are — what `intercept` handles, leaving `E`'s handler outside where it was */
  def interposing[E[_], R[_]](at: Place[E, R, ?]): Sub[R, R + E] = new Sub[R, R + E]:
    private val last = Place.Last[R, E]()
    def apply[E2[_]](p: Place[E2, R, ?]): Place[E2, R + E, ?] = Taken(at, p) match
      case _: Taken.Mine[e, r] => last
      case _: Taken.Theirs[e, e2, r] => (p: @unchecked) match
          case _: Place.Absurd[r2] => Place.Absurd[R + E]()
          case _ => Place.Init(p)

  private def further[R[_]]: Sub[R, R + Void] = new Sub[R, R + Void]:
    def apply[E[_]](at: Place[E, R, ?]): Place[E, R + Void, ?] = at match
      case _: Place.Absurd[r] => Place.Absurd[R + Void]()
      case _ => Place.Init(at)

  private def onlyToEnd[E[_]]: Sub[E, Pure + E] = new Sub[E, Pure + E]:
    private val last = Place.Last[Pure, E]()
    def apply[E2[_]](at: Place[E2, E, ?]): Place[E2, Pure + E, ?] = (at: @unchecked) match
      case _: Place.Only[e] => last
      case _: Place.Absurd[r] => Place.Absurd[Pure + E]()

  private def initToEnd[E[_], R[_], G[_], Rest[_]](at: Place[E, R, Rest]): Sub[R + G, Rest + G + E] =
    new Sub[R + G, Rest + G + E]:
      private val gNow = Place.Init[G, Rest + G, E, Rest](Place.Last[Rest, G]())
      private val eNow = Place.Last[Rest + G, E]()
      private val inner = toEnd(at)
      def apply[E2[_]](p: Place[E2, R + G, ?]): Place[E2, Rest + G + E, ?] = (p: @unchecked) match
        case _: Place.Last[r, g] => gNow
        case _: Place.Absurd[r] => Place.Absurd[Rest + G + E]()
        case i: Place.Init[e2, r, g, rest] => (inner(i.at): @unchecked) match
            case _: Place.Last[r3, e3] => eNow
            case j: Place.Init[e4, r4, g4, rest4] => Place.Init(Place.Init[E2, Rest, G, rest4](j.at))
            case _: Place.Absurd[r5] => Place.Absurd[Rest + G + E]()
