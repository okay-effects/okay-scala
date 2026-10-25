package okay.scala3.std

import okay.scala3.*

import scala.concurrent.{ExecutionContext, Future}

/** one step of a stream: done, or a value and the rest */
enum Step[+O, R[_]]:
  case Done[R[_]]() extends Step[Nothing, R]
  case Emit[O, R[_]](o: O, rest: Stream[O, R]) extends Step[O, R]

/**
 * A STREAM, pulled: `pull` a program of the row `R` giving its next step — nothing is run until it is pulled, and only
 * as far as it is pulled, so `take(3)` of an endless stream runs three steps. Its effects are any effects: a stream of
 * `Async` waits on callbacks, one of `Fail` may fail, one of `Sync` reads a file. A generator written with
 * `Yield.emit` is a stream (`Stream.generated`). Resources: acquired in a `Scope`, released when the scope ends —
 * a stream stopped early by `take` drops its rest unseen.
 */
final class Stream[+O, R[_]](val pull: Step[O, R] ! R):
  import Step.*

  def map[B](f: O => B): Stream[B, R] = Stream(pull.map {
    case Done() => Done()
    case Emit(o, rest) => Emit(f(o), rest.map(f))
  })

  /** each value through a program of the row */
  def evalMap[B](f: O => B ! R): Stream[B, R] = Stream(pull.flatMap {
    case Done() => pure(Done())
    case Emit(o, rest) => f(o).map(b => Emit(b, rest.evalMap(f)))
  })

  /** a program of the row run for each value, the value kept */
  def evalTap(f: O => Unit ! R): Stream[O, R] = evalMap(o => f(o).as(o))

  def filter(p: O => Boolean): Stream[O, R] = Stream(loop[Stream[O, R], Step[O, R], R](this) { s =>
    s.pull.map {
      case Done() => Right(Done())
      case Emit(o, rest) => if p(o) then Right(Emit(o, rest.filter(p))) else Left(rest)
    }
  })

  def collect[B](pf: PartialFunction[O, B]): Stream[B, R] = filter(pf.isDefinedAt).map(pf)

  /** the first `n` values: the rest never pulled */
  def take(n: Long): Stream[O, R] =
    if n <= 0 then Stream.empty else Stream(pull.map {
      case Done() => Done()
      case Emit(o, rest) => Emit(o, rest.take(n - 1))
    })

  def takeWhile(p: O => Boolean): Stream[O, R] = Stream(pull.map {
    case Emit(o, rest) if p(o) => Emit(o, rest.takeWhile(p))
    case _ => Done()
  })

  def drop(n: Long): Stream[O, R] = Stream(loop[(Long, Stream[O, R]), Step[O, R], R]((n, this)) { (left, s) =>
    if left <= 0 then s.pull.map(Right(_))
    else s.pull.map {
      case Done() => Right(Done())
      case Emit(_, rest) => Left((left - 1, rest))
    }
  })

  def dropWhile(p: O => Boolean): Stream[O, R] = Stream(loop[Stream[O, R], Step[O, R], R](this) { s =>
    s.pull.map {
      case Emit(o, rest) if p(o) => Left(rest)
      case step => Right(step)
    }
  })

  /** one of this, one of that, in turn; the rest of the longer at the end */
  def interleave[O2 >: O](that: Stream[O2, R]): Stream[O2, R] = Stream(pull.flatMap {
    case Done() => that.pull
    case Emit(o, rest) => pure(Emit(o, that.interleave(rest)))
  })

  /** this stream, then `that` */
  def ++[O2 >: O](that: => Stream[O2, R]): Stream[O2, R] = Stream(pull.flatMap {
    case Done() => that.pull
    case Emit(o, rest) => pure(Emit(o, rest ++ that))
  })

  /** each value's stream, one after another */
  def flatMap[B](f: O => Stream[B, R]): Stream[B, R] = Stream(pull.flatMap {
    case Done() => pure(Done())
    case Emit(o, rest) => (f(o) ++ rest.flatMap(f)).pull
  })

  /** pairs, as long as the shorter */
  def zip[B](that: Stream[B, R]): Stream[(O, B), R] = Stream(pull.flatMap {
    case Done() => pure(Done())
    case Emit(o, rest) => that.pull.map {
      case Done() => Done()
      case Emit(b, more) => Emit((o, b), rest.zip(more))
    }
  })

  def zipWithIndex: Stream[(O, Long), R] = zip(Stream.iterate[Long, R](0L)(_ + 1))

  /** the running folds, `z` first */
  def scan[B](z: B)(f: (B, O) => B): Stream[B, R] = Stream(pure(Emit(z, Stream(pull.flatMap {
    case Done() => pure(Done())
    case Emit(o, rest) => rest.scan(f(z, o))(f).pull
  }))))

  /** the values in groups of `n`, the last maybe fewer */
  def grouped(n: Int): Stream[List[O], R] = Stream(
    take(n).toList.flatMap(group => if group.isEmpty then pure(Done()) else pure(Emit(group, drop(n).grouped(n)))))

  /** all of it pulled: the fold of its values, in constant stack */
  def fold[B](z: B)(f: (B, O) => B): B ! R = loop[(B, Stream[O, R]), B, R]((z, this)) { (b, s) =>
    s.pull.map {
      case Done() => Right(b)
      case Emit(o, rest) => Left((f(b, o), rest))
    }
  }

  def toList: List[O] ! R = fold(List.empty[O])((os, o) => o :: os).map(_.reverse)

  /** all of it pulled, each value given to a program of the row */
  def foreach(f: O => Unit ! R): Unit ! R = evalMap(f).drain

  /** all of it pulled, its values dropped */
  def drain: Unit ! R = fold(())((_, _) => ())

  def headOption: Option[O] ! R = pull.map {
    case Done() => None
    case Emit(o, _) => Some(o)
  }

  def last: Option[O] ! R = fold(Option.empty[O])((_, o) => Some(o))

object Stream:
  import Step.*

  def empty[O, R[_]]: Stream[O, R] = Stream(pure(Done()))

  def emit[O, R[_]](o: O): Stream[O, R] = Stream(pure(Emit(o, empty)))

  def emits[O, R[_]](os: Iterable[O]): Stream[O, R] = fromIterator(os.iterator)

  /** an iterator's values: pulled once */
  def fromIterator[O, R[_]](it: Iterator[O]): Stream[O, R] =
    Stream(defer(pure(if it.hasNext then Emit(it.next(), fromIterator(it)) else Done())))

  /** a program's value, as a stream of one */
  def eval[O, R[_]](p: O ! R): Stream[O, R] = Stream(p.map(o => Emit(o, empty)))

  /** a value made by `p` at each pull: endless */
  def repeatEval[O, R[_]](p: O ! R): Stream[O, R] = Stream(p.map(o => Emit(o, repeatEval(p))))

  /** from `s`, each step's value and the next state, until `None` */
  def unfold[S, O, R[_]](s: S)(f: S => Option[(O, S)] ! R): Stream[O, R] = Stream(f(s).map {
    case None => Done()
    case Some((o, next)) => Emit(o, unfold(next)(f))
  })

  /** `a`, `f(a)`, `f(f(a))`, ...: endless */
  def iterate[O, R[_]](a: O)(f: O => O): Stream[O, R] = Stream(defer(pure(Emit(a, iterate(f(a))(f)))))

  /** from `from` up to `until`, not including it */
  def range[R[_]](from: Long, until: Long): Stream[Long, R] =
    unfold[Long, Long, R](from)(n => pure(Option.when(n < until)((n, n + 1))))

  /** a generator, written with `Yield.emit`, as a stream: it runs to its next `emit` when pulled, and stops there */
  def generated[O, R[_], Rest[_]](p: Unit ! R)(using Split[Yield % O, R, Rest]): Stream[O, Rest] =
    Stream(handle(p, Handler.general[Yield % O, Rest, Unit, Step[O, Rest]](_ => pure(Done()))(
      [X] => (op: Yield[O, X], k: X => Step[O, Rest] ! Rest) => op match
        case Yield.Emit(o) => pure(Emit(o, Stream(k(())))))))

  /** a queue's values, each taken when pulled: endless, until `None` is offered */
  def fromQueue[O, R[_]](q: Queue[Option[O]])(using In[Async, R]): Stream[O, R] = Stream(q.take[R].map {
    case None => Done()
    case Some(o) => Emit(o, fromQueue(q))
  })

  /** THE STREAMS MERGED, IN TURN: one value of each in turn, a stream that has ended left out — of any row, no buffer,
   * no fiber: each pull the next stream's own */
  def merge[O, R[_]](ss: List[Stream[O, R]]): Stream[O, R] = inTurn(scala.collection.immutable.Queue.from(ss))

  /** the same, the streams given one by one: `merge(a, b, c)` */
  def merge[O, R[_]](first: Stream[O, R], rest: Stream[O, R]*): Stream[O, R] = merge(first :: rest.toList)

  private def inTurn[O, R[_]](q: scala.collection.immutable.Queue[Stream[O, R]]): Stream[O, R] =
    q.dequeueOption match
      case None => empty
      case Some((s, others)) => Stream(s.pull.flatMap {
        case Done() => inTurn(others).pull
        case Emit(o, rest) => pure(Emit(o, inTurn(others.enqueue(rest))))
      })

  /** STREAMS OF `Async` MERGED AS THEIR VALUES COME: each stream's next pull started at once, all at the same time;
   * the first to come given out, and the next of its stream started. A stream that ends is left out; one that fails
   * fails the merged stream. Pulled a step ahead: while a value is used, each stream's next is on its way */
  def mergeAsync[O](ss: List[Stream[O, Async]])(using ExecutionContext): Stream[O, Async] =
    Stream(defer(racing(ss.zipWithIndex.map((s, i) => (i, Async.start(s.pull)))).pull))

  /** the same, the streams given one by one: `mergeAsync(a, b, c)` */
  def mergeAsync[O](first: Stream[O, Async], rest: Stream[O, Async]*)(using ExecutionContext): Stream[O, Async] =
    mergeAsync(first :: rest.toList)

  /** the pulls on their way, each with its stream's number: the first to come given out, its stream's next pulled */
  private def racing[O](pending: List[(Int, Running[Step[O, Async]])])(using ec: ExecutionContext)
    : Stream[O, Async] =
    if pending.isEmpty then empty
    else Stream(Async.fromFuture[(Int, Step[O, Async]), Async](
      Future.firstCompletedOf(pending.map((i, r) => r.future.map(step => (i, step))))).flatMap { (i, step) =>
      val others = pending.filterNot(_._1 == i)
      step match
        case Done() => racing(others).pull
        case Emit(o, rest) => pure(Emit(o, racing(others :+ ((i, Async.start(rest.pull))))))
    })

  extension [O](s: Stream[O, Async])
    /** each value through `f`, up to `n` at once, the results in order */
    def parEvalMap[B](n: Int)(f: O => B ! Async)(using ExecutionContext): Stream[B, Async] =
      s.grouped(n).flatMap(group => Stream.eval[List[B], Async](Async.parTraverse(group)(f))
        .flatMap(bs => emits[B, Async](bs)))
