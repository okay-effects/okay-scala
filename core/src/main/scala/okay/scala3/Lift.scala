package okay.scala3

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

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

object Lift:
  def lift[G[_], X, R[_]](ga: G[X])(using In[Lifted[G], R]): X ! R = perform[Lifted[G]](Of(ga))

  /** a program whose one effect is lifting `G`'s, as a `G`: each lifted `G[X]` bound in `G`, the rest run inside the
   * bind */
  def lower[G[_], A](p: A ! Lifted[G])(using G: Lowering[G]): G[A] =
    run(handle(p, Handler.general[Lifted[G], Pure, A, G[A]](a => pure(G.pure(a)))(
      [X] => (op: Lift[G, X], k: X => G[A] ! Pure) => op match
        case Of(ga) => pure(G.flatMap(ga)(x => run(k(x)))))))
