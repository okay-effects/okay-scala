package okay.scala3.std

import okay.scala3.*

import scala.annotation.targetName
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** A PROGRAM OF ANOTHER MONAD as an effect: `Of(ga)`, a `G[X]` performed as it is, its value the answer — an
 * `Option`, an `Either`, a `Future`, an `IO`: anything `lower` can bind. In a row, `Lifted[G]` */
enum Lift[G[_], X]:
  case Of[G[_], X](ga: G[X]) extends Lift[G, X]

/** the effect of lifting `G`'s programs, in a row */
type Lifted[G[_]] = [X] =>> Lift[G, X]

/** what `lower` needs of `G`: its `pure` and its bind — a monad, of a library or one's own */
trait Lowering[G[_]]:
  def pure[A](a: A): G[A]
  def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B]

object Lowering:
  given Lowering[Option] with
    def pure[A](a: A): Option[A] = Some(a)
    def flatMap[A, B](ga: Option[A])(f: A => Option[B]): Option[B] = ga.flatMap(f)

  given either[E]: Lowering[[A] =>> Either[E, A]] with
    def pure[A](a: A): Either[E, A] = Right(a)
    def flatMap[A, B](ga: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ga.flatMap(f)

  given Lowering[Try] with
    def pure[A](a: A): Try[A] = scala.util.Success(a)
    def flatMap[A, B](ga: Try[A])(f: A => Try[B]): Try[B] = ga.flatMap(f)

  given future(using ExecutionContext): Lowering[Future] with
    def pure[A](a: A): Future[A] = Future.successful(a)
    def flatMap[A, B](ga: Future[A])(f: A => Future[B]): Future[B] = ga.flatMap(f)

/** A MONAD MORPHISM, `G`'s programs as `H`'s: `[X] => (t: Try[X]) => Future.fromTry(t)` */
type ~>[G[_], H[_]] = [X] => G[X] => H[X]

/**
 * A DISTRIBUTIVE LAW OF `G` OVER ANY ROW: a `G[A]` bound to a continuation that is a program, `G` coming out on the
 * other side, inside the program — what `Lift.handle` needs to handle `Lifted[G]` in place, the rest of the row kept.
 *
 * Only a TRAVERSABLE monad has one: its values gone through, each given to `k`, in order. `Option`, `Either`, `Try`
 * give `k` their one value or none; `List` and `Vector` traverse `k` over their elements in order, one after
 * another, then concatenate what each gave. `Future`, `IO` have none, by design: what they will hold cannot be gone
 * through now — one of them is the last, lowered by `Lift.lower`.
 *
 * The laws, in the row: `flatMapIn(pure(a))(k) == k(a)`; `flatMapIn(ga)(a => pure(pure(a))) == pure(ga)`;
 * associativity, for `Option`, `Either`, `Try` with any effects, for `List` and `Vector` with effects that commute
 */
trait Distributes[G[_]]:
  def pure[A](a: A): G[A]
  def flatMapIn[A, B, R[_]](ga: G[A])(k: A => G[B] ! R): G[B] ! R

object Distributes:
  given Distributes[Option] with
    def pure[A](a: A): Option[A] = Some(a)
    def flatMapIn[A, B, R[_]](ga: Option[A])(k: A => Option[B] ! R): Option[B] ! R = ga match
      case Some(a) => k(a)
      case None => okay.scala3.pure(None)

  given either[E]: Distributes[[A] =>> Either[E, A]] with
    def pure[A](a: A): Either[E, A] = Right(a)
    def flatMapIn[A, B, R[_]](ga: Either[E, A])(k: A => Either[E, B] ! R): Either[E, B] ! R = ga match
      case Right(a) => k(a)
      case Left(e) => okay.scala3.pure(Left(e))

  given Distributes[Try] with
    def pure[A](a: A): Try[A] = Success(a)
    def flatMapIn[A, B, R[_]](ga: Try[A])(k: A => Try[B] ! R): Try[B] ! R = ga match
      case Success(a) => k(a)
      case Failure(e) => okay.scala3.pure(Failure(e))

  /** `k` over the elements in order, in a loop — constant stack — what each gave concatenated; immutable, so a `k`
   * resumed twice by a handler outside (a `Choose`) goes on from the same elements */
  given Distributes[List] with
    def pure[A](a: A): List[A] = List(a)
    def flatMapIn[A, B, R[_]](ga: List[A])(k: A => List[B] ! R): List[B] ! R =
      traverse(ga, k).map(_.toList)

  given Distributes[Vector] with
    def pure[A](a: A): Vector[A] = Vector(a)
    def flatMapIn[A, B, R[_]](ga: Vector[A])(k: A => Vector[B] ! R): Vector[B] ! R = traverse(ga.toList, k)

  private def traverse[A, B, R[_]](as: List[A], k: A => IterableOnce[B] ! R): Vector[B] ! R =
    loop[(List[A], Vector[B]), Vector[B], R]((as, Vector.empty)):
      case (Nil, done) => okay.scala3.pure(Right(done))
      case (a :: more, done) => k(a).map(bs => Left((more, done ++ bs)))

object Lift:
  def lift[G[_], X, R[_]](ga: G[X])(using In[Lifted[G], R]): X ! R = perform[Lifted[G]](Of(ga))

  /** a program whose one effect is lifting `G`'s, as a `G`: each lifted `G[X]` bound in `G`, the rest run inside the
   * bind */
  def lower[G[_], A](p: A ! Lifted[G])(using G: Lowering[G]): G[A] = lowered(p)

  /** the same of a row whose one effect left is `Lifted[G]`, of any shape: `Pure + Lifted[G]`, what is left when the
   * data monads before it were handled in place (`handle`) */
  @targetName("lowerLast")
  def lower[G[_], R[_], A](p: A ! R)(using at: Split[Lifted[G], R, Pure], G: Lowering[G]): G[A] = lowered(p)

  private def lowered[G[_], R[_], A](p: A ! R)(using at: Split[Lifted[G], R, Pure], G: Lowering[G]): G[A] =
    run(okay.scala3.handle(p, Handler.general[Lifted[G], Pure, A, G[A]](a => pure(G.pure(a)))(
      [X] => (op: Lift[G, X], k: X => G[A] ! Pure) => op match
        case Of(ga) => pure(G.flatMap(ga)(x => run(k(x)))))))

  /** `Lifted[G]` HANDLED IN PLACE, the rest of the row kept: each lifted `G[X]` bound by `G`'s distributive law to the
   * rest of the program — so any number of data monads are handled one after another, the last, a `Future` or an
   * `IO`, lowered by `lower`. `List`'s depth first: the whole rest for each element, in order. `Lift.handle[List](p)` */
  def handle[G[_]]: Handling[G] = Handling[G]()

  final class Handling[G[_]]:
    def apply[R[_], Rest[_], A](p: A ! R)(using at: Split[Lifted[G], R, Rest], G: Distributes[G]): G[A] ! Rest =
      okay.scala3.handle(p, Handler.general[Lifted[G], Rest, A, G[A]](a => pure(G.pure(a)))(
        [X] => (op: Lift[G, X], k: X => G[A] ! Rest) => op match
          case Of(ga) => G.flatMapIn(ga)(k)))

  /** each `Lifted[G]` operation REPLACED by the `Lifted[H]` one `nt` makes, `Lifted[H]` in the row already: several
   * base monads brought to one, to lower — `Lift.via[Try, Future]([X] => (t: Try[X]) => Future.fromTry(t))(p)` */
  def via[G[_], H[_]](nt: G ~> H): Via[G, H] = Via(nt)

  final class Via[G[_], H[_]](nt: G ~> H):
    def apply[R[_], Rest[_], A](p: A ! R)(using at: Split[Lifted[G], R, Rest], h: In[Lifted[H], Rest]): A ! Rest =
      okay.scala3.handle(p, Handler.general[Lifted[G], Rest, A, A](a => pure(a))(
        [X] => (op: Lift[G, X], k: X => A ! Rest) => op match
          case Of(ga) => perform[Lifted[H]](Of(nt(ga))).flatMap(k)))
