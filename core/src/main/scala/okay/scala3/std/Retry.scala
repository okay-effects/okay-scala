package okay.scala3.std

import okay.scala3.*

/** A PROGRAM RUN AGAIN when it fails: a program is data, so running it again is running the same value. Its effects
 * done before the failure are done again — a retry is for what is safe to repeat */
object Retry:
  /** `p`, run again up to `times` more times while it raises an error `retryOn` accepts: the last error raised on */
  def times[E, R[_], Rest[_], A](times: Int, retryOn: E => Boolean = (_: E) => true)(p: A ! R)(
    using Split[Fail % E, R, Rest], Sub[Rest, R], In[Fail % E, R]): A ! R =
    attempts[E, R, Rest, A](times, retryOn, _ => unit[R])(p)

  /** as `times`, waiting before each new run: `firstMs`, then each wait `factor` times the one before */
  def backoff[E, R[_], Rest[_], A](times: Int, firstMs: Long, factor: Double = 2.0,
                                   retryOn: E => Boolean = (_: E) => true)(p: A ! R)(
    using Split[Fail % E, R, Rest], Sub[Rest, R], In[Fail % E, R], In[Async, R]): A ! R =
    attempts[E, R, Rest, A](times, retryOn, n => Async.sleep[R]((firstMs * math.pow(factor, n)).toLong))(p)

  private def attempts[E, R[_], Rest[_], A](times: Int, retryOn: E => Boolean, pause: Int => Unit ! R)(p: A ! R)(
    using Split[Fail % E, R, Rest], Sub[Rest, R], In[Fail % E, R]): A ! R =
    loop[Int, A, R](0) { n =>
      Fail.run[E, R, Rest, A](p).into[R].flatMap {
        case Right(a) => pure(Right(a))
        case Left(e) if n < times && retryOn(e) => pause(n).as(Left(n + 1))
        case Left(e) => Fail.raise[E, R](e)
      }
    }
