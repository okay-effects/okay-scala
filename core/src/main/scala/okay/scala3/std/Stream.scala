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
 * `Yield.emit` is a stream (`Stream.generated`).
 *
 * RESOURCES: `release`, what to do if the stream is left unpulled — a file, a cursor, a Java stream closed. A stream
 * holding one (`Stream.bracket`) closes it when it ends, when a value's program fails, and when it is left: `take`,
 * `takeWhile`, `headOption`, `zip`'s longer, a merge stopped, call `release` of what they leave. A stream given to
 * `++` by name is not opened until it is reached, so has nothing to release before.
 */
final class Stream[+O, R[_]](val pull: Step[O, R] ! R, val release: () => Unit = Stream.nothing):
  import Step.*
  import Stream.{nothing, onAbort}

  def map[B](f: O => B): Stream[B, R] = Stream(pull.map {
    case Done() => Done()
    case Emit(o, rest) => Emit(f(o), rest.map(f))
  }, release)

  /** each value through a program of the row: it failing, the rest released */
  def evalMap[B](f: O => B ! R): Stream[B, R] = Stream(pull.flatMap {
    case Done() => pure(Done())
    case Emit(o, rest) => onAbort(f(o))(rest.release).map(b => Emit(b, rest.evalMap(f)))
  }, release)

  /** a program of the row run for each value, the value kept */
  def evalTap(f: O => Unit ! R): Stream[O, R] = evalMap(o => f(o).as(o))

  def filter(p: O => Boolean): Stream[O, R] = Stream(loop[Stream[O, R], Step[O, R], R](this) { s =>
    s.pull.map {
      case Done() => Right(Done())
      case Emit(o, rest) => if p(o) then Right(Emit(o, rest.filter(p))) else Left(rest)
    }
  }, release)

  def collect[B](pf: PartialFunction[O, B]): Stream[B, R] = filter(pf.isDefinedAt).map(pf)

  /** the first `n` values: the rest never pulled, released */
  def take(n: Long): Stream[O, R] =
    if n <= 0 then Stream(defer {
      release()
      pure(Done())
    }, release)
    else Stream(pull.map {
      case Done() => Done()
      case Emit(o, rest) => Emit(o, rest.take(n - 1))
    }, release)

  /** the values while `p` holds: the rest released */
  def takeWhile(p: O => Boolean): Stream[O, R] = Stream(pull.map {
    case Emit(o, rest) if p(o) => Emit(o, rest.takeWhile(p))
    case Emit(_, rest) =>
      rest.release()
      Done()
    case Done() => Done()
  }, release)

  def drop(n: Long): Stream[O, R] = Stream(loop[(Long, Stream[O, R]), Step[O, R], R]((n, this)) { (left, s) =>
    if left <= 0 then s.pull.map(Right(_))
    else s.pull.map {
      case Done() => Right(Done())
      case Emit(_, rest) => Left((left - 1, rest))
    }
  }, release)

  def dropWhile(p: O => Boolean): Stream[O, R] = Stream(loop[Stream[O, R], Step[O, R], R](this) { s =>
    s.pull.map {
      case Emit(o, rest) if p(o) => Left(rest)
      case step => Right(step)
    }
  }, release)

  /** one of this, one of that, in turn; the rest of the longer at the end */
  def interleave[O2 >: O](that: Stream[O2, R]): Stream[O2, R] = Stream(onAbort(pull)(that.release).flatMap {
    case Done() => that.pull
    case Emit(o, rest) => pure(Emit(o, that.interleave(rest)))
  }, both(that))

  /** this stream, then `that`: made, and opened, when this one ends */
  def ++[O2 >: O](that: => Stream[O2, R]): Stream[O2, R] = Stream(pull.flatMap {
    case Done() => that.pull
    case Emit(o, rest) => pure(Emit(o, rest ++ that))
  }, release)

  /** this stream, then `that`, a stream already held: both released if left */
  private def concat[O2 >: O](that: Stream[O2, R]): Stream[O2, R] = Stream(onAbort(pull)(that.release).flatMap {
    case Done() => that.pull
    case Emit(o, rest) => pure(Emit(o, rest.concat(that)))
  }, both(that))

  /** each value's stream, one after another: left in the middle, the inner stream and the outer released */
  def flatMap[B](f: O => Stream[B, R]): Stream[B, R] = Stream(pull.flatMap {
    case Done() => pure(Done())
    case Emit(o, rest) => f(o).concat(rest.flatMap(f)).pull
  }, release)

  /** pairs, as long as the shorter: the longer's rest released */
  def zip[B](that: Stream[B, R]): Stream[(O, B), R] = Stream(onAbort(pull)(that.release).flatMap {
    case Done() =>
      that.release()
      pure(Done())
    case Emit(o, rest) => onAbort(that.pull)(rest.release).map {
      case Done() =>
        rest.release()
        Done()
      case Emit(b, more) => Emit((o, b), rest.zip(more))
    }
  }, both(that))

  def zipWithIndex: Stream[(O, Long), R] = zip(Stream.iterate[Long, R](0L)(_ + 1))

  /** the running folds, `z` first */
  def scan[B](z: B)(f: (B, O) => B): Stream[B, R] = Stream(pure(Emit(z, Stream(pull.flatMap {
    case Done() => pure(Done())
    case Emit(o, rest) => rest.scan(f(z, o))(f).pull
  }, release))), release)

  /** the values in groups of `n`, the last maybe fewer */
  def grouped(n: Int): Stream[List[O], R] =
    require(n > 0, "a group of at least one")
    Stream(loop[(Int, List[O], Stream[O, R]), Step[List[O], R], R]((0, Nil, this)) { (size, group, s) =>
      if size == n then pure(Right(Emit(group.reverse, s.grouped(n))))
      else s.pull.map {
        case Done() => Right(if group.isEmpty then Done() else Emit(group.reverse, Stream.empty))
        case Emit(o, rest) => Left((size + 1, o :: group, rest))
      }
    }, release)

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

  /** the first value: the rest released */
  def headOption: Option[O] ! R = pull.map {
    case Done() => None
    case Emit(o, rest) =>
      rest.release()
      Some(o)
  }

  def last: Option[O] ! R = fold(Option.empty[O])((_, o) => Some(o))

  /** at its end, or left, `fin` — once */
  private[std] def finishing(fin: () => Unit): Stream[O, R] = Stream(onAbort(pull)(fin).map {
    case Done() =>
      fin()
      Done()
    case Emit(o, rest) => Emit(o, rest.finishing(fin))
  }, () => {
    release()
    fin()
  })

  private def both(that: Stream[?, R]): () => Unit =
    if (release eq nothing) && (that.release eq nothing) then nothing
    else () =>
      try release()
      finally that.release()

object Stream:
  import Step.*

  /** nothing to release */
  val nothing: () => Unit = () => ()

  /** `p`, and `fin` if it does not end with a value: a failure, an abort */
  private[std] def onAbort[A, R[_]](p: A ! R)(fin: () => Unit): A ! R =
    if fin eq nothing then p
    else okay.scala3.defer {
      var ended = false
      ensuring(p.map { a =>
        ended = true
        a
      })(if !ended then fin())
    }

  /** `f` run at most once */
  private def once(f: () => Unit): () => Unit =
    val done = java.util.concurrent.atomic.AtomicBoolean(false)
    () => if done.compareAndSet(false, true) then f()

  /** A STREAM OF A RESOURCE: `acquire`d at the first pull, `use`d, and `release`d once — when the stream ends, when a
   * value's program fails, or when it is left (`take`, `headOption`, a merge stopped) */
  def bracket[S, O, R[_]](acquire: S ! R)(release: S => Unit)(use: S => Stream[O, R]): Stream[O, R] =
    Stream(acquire.flatMap(s => use(s).finishing(once(() => release(s))).pull))

  def empty[O, R[_]]: Stream[O, R] = Stream(pure(Done()))

  def emit[O, R[_]](o: O): Stream[O, R] = Stream(pure(Emit(o, empty)))

  /** an iterable's values — a `List`, a `Vector`, a `Range`, a `Set`: its iterator's */
  def emits[O, R[_]](os: Iterable[O]): Stream[O, R] = fromIterator(os.iterator)

  /** an iterator's values: the iterator made at the first pull, its values pulled one at a time, as far as asked;
   * each step remembered, so the stream pulled twice gives the same values */
  def fromIterator[O, R[_]](it: => Iterator[O]): Stream[O, R] = Stream(defer(stepping[O, R](it).pull))

  /** a lazy list's values, endless or not, as it makes them */
  def fromLazyList[O, R[_]](ll: LazyList[O]): Stream[O, R] =
    Stream(defer(pure(if ll.isEmpty then Done() else Emit(ll.head, fromLazyList(ll.tail)))))

  /** a step of the iterator, taken once, when first pulled */
  private def stepping[O, R[_]](it: Iterator[O]): Stream[O, R] =
    lazy val step: Step[O, R] = if it.hasNext then
      val o = it.next()
      Emit(o, stepping(it))
    else Done()
    Stream(defer(pure(step)))

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
      case Some((s, others)) => Stream(onAbort(s.pull)(releaseAll(others)).flatMap {
        case Done() => inTurn(others).pull
        case Emit(o, rest) => pure(Emit(o, inTurn(others.enqueue(rest))))
      }, releaseAll(q))

  private def releaseAll[O, R[_]](ss: Iterable[Stream[O, R]]): () => Unit =
    if ss.forall(_.release eq nothing) then nothing else () => ss.foreach(_.release())

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
    else Stream(onAbort(Async.fromFuture[(Int, Step[O, Async]), Async](
      Future.firstCompletedOf(pending.map((i, r) => r.future.map(step => (i, step))))))(releasing(pending)).flatMap {
      (i, step) =>
        val others = pending.filterNot(_._1 == i)
        step match
          case Done() => racing(others).pull
          case Emit(o, rest) => pure(Emit(o, racing(others :+ ((i, Async.start(rest.pull))))))
    }, releasing(pending))

  /** the pulls on their way, left: each stream's rest released when its pull comes */
  private def releasing[O](pending: List[(Int, Running[Step[O, Async]])]): () => Unit = () =>
    pending.foreach((_, r) => r.future.foreach {
      case Emit(_, rest) => rest.release()
      case _ => ()
    }(using ExecutionContext.parasitic))

  extension [O](s: Stream[O, Pure])
    /** a stream of no effect as a lazy list: each value pulled when the list is read that far — its other effects
     * handled first. A lazy list left unread cannot say so: a stream of a resource, read to its end */
    def toLazyList: LazyList[O] = LazyList.unfold(s)(pulled)

    /** a stream of no effect as an iterator: each value pulled by `next`; `close` releases what is left */
    def iterator: Pulling[O] = Pulling(s)

  private def pulled[O](s: Stream[O, Pure]): Option[(O, Stream[O, Pure])] = run(s.pull) match
    case Done() => None
    case Emit(o, rest) => Some((o, rest))

  /** an iterator pulling a stream of no effect, one value ahead at most; `close`, the rest released */
  final class Pulling[O](private var rest: Stream[O, Pure]) extends Iterator[O] with AutoCloseable:
    private var ahead: Option[O] = None
    private var ended = false
    def hasNext: Boolean =
      if ahead.isEmpty && !ended then pulled(rest) match
        case None => ended = true
        case Some((o, more)) =>
          ahead = Some(o)
          rest = more
      ahead.isDefined
    def next(): O =
      if !hasNext then throw java.util.NoSuchElementException("the stream has ended")
      val o = ahead.get
      ahead = None
      o
    def close(): Unit = if !ended then
      ended = true
      ahead = None
      rest.release()

  extension [O](s: Stream[O, Async])
    /** each value through `f`, up to `n` at once, the results in order */
    def parEvalMap[B](n: Int)(f: O => B ! Async)(using ExecutionContext): Stream[B, Async] =
      s.grouped(n).flatMap(group => Stream.eval[List[B], Async](Async.parTraverse(group)(f))
        .flatMap(bs => emits[B, Async](bs)))
