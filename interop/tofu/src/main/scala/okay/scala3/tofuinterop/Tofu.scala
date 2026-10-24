package okay.scala3.tofuinterop

import cats.Functor
import okay.scala3.*
import okay.scala3.catsinterop.{Program, monad}
import okay.scala3.std.*
import tofu.{Errors, WithLocal}

/** tofu's `WithLocal` of a `Reader` anywhere in `R`: its context asked; `local` its programs asking it changed — the
 * `Reader` handled again from its place by `project` of the context asked once, the rest put back in `R` */
given withLocal[R[_], Rest[_], C](using Split[Reader % C, R, Rest], Sub[Rest, R]): WithLocal[Program[R], C] with
  def functor: Functor[Program[R]] = monad[R]
  def context: C ! R = Reader.ask[C, R]
  def local[A](fa: A ! R)(project: C => C): A ! R = Reader.local[C, R, Rest, A](project)(fa)

/** tofu's `Errors` of a `Fail` anywhere in `R`: raised; handled — the `Fail` run from its place, its program put back
 * in `R`, the recovery in `R`, so a raise in it goes out to the handler outside */
given errors[R[_], Rest[_], E](using Split[Fail % E, R, Rest], Sub[Rest, R]): Errors[Program[R], E] with
  def raise[A](err: E): A ! R = Fail.raise[E, R](err)
  def tryHandleWith[A](fa: A ! R)(f: E => Option[A ! R]): A ! R =
    Fail.run[E, R, Rest, A](fa).into[R].flatMap {
      case Right(a) => pure(a)
      case Left(e) => f(e).getOrElse(raise[A](e))
    }
  def restore[A](fa: A ! R): Option[A] ! R = Fail.run[E, R, Rest, A](fa).into[R].map(_.toOption)
  def lift[A](fa: A ! R): A ! R = fa
