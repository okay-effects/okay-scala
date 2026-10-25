package okay.scala3.std

import okay.scala3.*

import scala.collection.immutable.ArraySeq
import scala.concurrent.{ExecutionContext, Future}

/** values pulled together: a stream's step gives a chunk of them, never empty */
type Chunk[+O] = ArraySeq[O]

object Chunk:
  def apply[O](os: O*): Chunk[O] = ArraySeq.untagged.from(os)
  /** one value: no varargs, no builder */
  def single[O](o: O): Chunk[O] = ArraySeq.untagged.fill(1)(o)
  def from[O](os: IterableOnce[O]): Chunk[O] = ArraySeq.untagged.from(os)

/** one step of a stream: done, or a chunk of values, never empty, and the rest */
enum Step[+O, R[_]]:
  case Done[R[_]]() extends Step[Nothing, R]
  case Emit[O, R[_]](chunk: Chunk[O], rest: Stream[O, R]) extends Step[O, R]

/**
 * A STREAM, pulled: `pull` a program of the row `R` giving its next step — nothing is run until it is pulled, and only
 * as far as it is pulled, so `take(3)` of an endless stream runs three values' programs. Its effects are any effects:
 * a stream of `Async` waits on callbacks, one of `Fail` may fail, one of `Sync` reads a file. A generator written
 * with `Yield.emit` is a stream (`Stream.generated`).
 *
 * CHUNKS: a step gives its values in a chunk — a collection's, a range's, a Java stream's, read many at a time, and
 * `map`, `filter`, `fold` go through a chunk in a loop, not a step of the machine per value. A program per value
 * (`evalMap`, `foreach`) runs per value: as far as pulled, no further. `chunks` and `unchunks` to see them.
 *
 * RESOURCES: `release`, what to do if the stream is left unpulled — a file, a cursor, a Java stream closed. A stream
 * holding one (`Stream.bracket`) closes it when it ends, when a value's program fails, and when it is left: `take`,
 * `takeWhile`, `headOption`, `zip`'s longer, a merge stopped, call `release` of what they leave. A stream given to
 * `++` by name is not opened until it is reached, so has nothing to release before.
 */
final class Stream[+O, R[_]](val pull: Step[O, R] ! R, val release: () => Unit = Stream.nothing):
  import Step.*
  import Stream.{cons, nothing, onAbort}

  /** the steps through `f` */
  private def steps[B](f: Step[O, R] => Step[B, R]): Stream[B, R] = Stream(pull.map(f), release)

  def map[B](f: O => B): Stream[B, R] = mapChunks(_.map(f))

  /** each chunk through `f`: what it gives, the stream's chunk, an empty one left out */
  def mapChunks[B](f: Chunk[O] => Chunk[B]): Stream[B, R] = Stream(loop[Stream[O, R], Step[B, R], R](this) { s =>
    s.pull.map {
      case Done() => Right(Done())
      case Emit(c, rest) =>
        val b = f(c)
        if b.isEmpty then Left(rest) else Right(Emit(b, rest.mapChunks(f)))
    }
  }, release)

  /** each value through a program of the row, one at a time, as far as pulled: it failing, the rest released */
  def evalMap[B](f: O => B ! R): Stream[B, R] = Stream(pull.flatMap {
    case Done() => pure(Done())
    case Emit(c, rest) => Stream.evalAt(c, 0, rest, f).pull
  }, release)

  /** a program of the row run for each value, the value kept */
  def evalTap(f: O => Unit ! R): Stream[O, R] = evalMap(o => f(o).as(o))

  def filter(p: O => Boolean): Stream[O, R] = mapChunks(_.filter(p))

  def collect[B](pf: PartialFunction[O, B]): Stream[B, R] = mapChunks(_.collect(pf))

  /** the first `n` values: the rest never pulled, released */
  def take(n: Long): Stream[O, R] =
    if n <= 0 then Stream(defer {
      release()
      pure(Done())
    }, release)
    else steps {
      case Done() => Done()
      case Emit(c, rest) =>
        if c.size < n then Emit(c, rest.take(n - c.size))
        else Emit(c.take(n.toInt), rest.take(0))
    }

  /** the values while `p` holds: the rest released */
  def takeWhile(p: O => Boolean): Stream[O, R] = steps {
    case Done() => Done()
    case Emit(c, rest) =>
      val kept = c.takeWhile(p)
      if kept.size == c.size then Emit(c, rest.takeWhile(p))
      else
        rest.release()
        if kept.isEmpty then Done() else Emit(kept, Stream.empty)
  }

  def drop(n: Long): Stream[O, R] = Stream(loop[(Long, Stream[O, R]), Step[O, R], R]((n, this)) { (left, s) =>
    if left <= 0 then s.pull.map(Right(_))
    else s.pull.map {
      case Done() => Right(Done())
      case Emit(c, rest) =>
        if c.size <= left then Left((left - c.size, rest)) else Right(Emit(c.drop(left.toInt), rest))
    }
  }, release)

  def dropWhile(p: O => Boolean): Stream[O, R] = Stream(loop[Stream[O, R], Step[O, R], R](this) { s =>
    s.pull.map {
      case Done() => Right(Done())
      case Emit(c, rest) =>
        val left = c.dropWhile(p)
        if left.isEmpty then Left(rest) else Right(Emit(left, rest))
    }
  }, release)

  /** one value of this, one of that, in turn; the rest of the longer at the end */
  def interleave[O2 >: O](that: Stream[O2, R]): Stream[O2, R] = Stream(onAbort(pull)(that.release).flatMap {
    case Done() => that.pull
    case Emit(c, rest) => pure(Emit(Chunk.single(c.head), that.interleave(Stream.from(c, 1, rest))))
  }, both(that))

  /** this stream, then `that`: made, and opened, when this one ends */
  def ++[O2 >: O](that: => Stream[O2, R]): Stream[O2, R] = Stream(pull.flatMap {
    case Done() => that.pull
    case Emit(c, rest) => pure(Emit(c, rest ++ that))
  }, release)

  /** this stream, then `that`, a stream already held: both released if left */
  private[std] def concat[O2 >: O](that: Stream[O2, R]): Stream[O2, R] =
    Stream(onAbort(pull)(that.release).flatMap {
      case Done() => that.pull
      case Emit(c, rest) => pure(Emit(c, rest.concat(that)))
    }, both(that))

  /** each value's stream, one after another: left in the middle, the inner stream and the outer released */
  def flatMap[B](f: O => Stream[B, R]): Stream[B, R] = Stream(pull.flatMap {
    case Done() => pure(Done())
    case Emit(c, rest) => Stream.flatAt(c, 0, rest, f).pull
  }, release)

  /** pairs, as long as the shorter: the longer's rest released */
  def zip[B](that: Stream[B, R]): Stream[(O, B), R] = Stream(onAbort(pull)(that.release).flatMap {
    case Done() =>
      that.release()
      pure(Done())
    case Emit(c, rest) => onAbort(that.pull)(rest.release).map {
      case Done() =>
        rest.release()
        Done()
      case Emit(d, more) =>
        val n = math.min(c.size, d.size)
        Emit(c.zip(d), cons(c.drop(n), rest).zip(cons(d.drop(n), more)))
    }
  }, both(that))

  def zipWithIndex: Stream[(O, Long), R] = Stream(loop[(Long, Stream[O, R]), Step[(O, Long), R], R]((0L, this)) {
    (i, s) => s.pull.map {
      case Done() => Right(Done())
      case Emit(c, rest) => Right(Emit(c.zipWithIndex.map((o, j) => (o, i + j)), Stream.indexed(rest, i + c.size)))
    }
  }, release)

  /** the running folds, `z` first */
  def scan[B](z: B)(f: (B, O) => B): Stream[B, R] = Stream(pure(Emit(Chunk.single(z), scanning(z)(f))), release)

  private def scanning[B](z: B)(f: (B, O) => B): Stream[B, R] = steps {
    case Done() => Done()
    case Emit(c, rest) =>
      val bs = c.scanLeft(z)(f).tail
      Emit(bs, rest.scanning(bs.last)(f))
  }

  /** the values in groups of `n`, the last maybe fewer */
  def grouped(n: Int): Stream[List[O], R] =
    require(n > 0, "a group of at least one")
    Stream(loop[(Vector[O], Stream[O, R]), Step[List[O], R], R]((Vector.empty, this)) { (held, s) =>
      if held.size >= n then
        val (full, left) = held.splitAt(held.size / n * n)
        pure(Right(Emit(Chunk.from(full.grouped(n).map(_.toList)), cons(Chunk.from(left), s).grouped(n))))
      else s.pull.map {
        case Done() => Right(if held.isEmpty then Done() else Emit(Chunk.single(held.toList), Stream.empty))
        case Emit(c, rest) => Left((held ++ c, rest))
      }
    }, release)

  /** the chunks as values */
  def chunks: Stream[Chunk[O], R] = steps {
    case Done() => Done()
    case Emit(c, rest) => Emit(Chunk.single(c), rest.chunks)
  }

  /** the values in chunks of at most `n`: a chunk split, never joined */
  def chunkLimit(n: Int): Stream[O, R] =
    require(n > 0, "a chunk of at least one")
    steps {
      case Done() => Done()
      case Emit(c, rest) =>
        if c.size <= n then Emit(c, rest.chunkLimit(n)) else Emit(c.take(n), cons(c.drop(n), rest).chunkLimit(n))
    }

  /** all of it pulled: the fold of its values, in constant stack */
  def fold[B](z: B)(f: (B, O) => B): B ! R = loop[(B, Stream[O, R]), B, R]((z, this)) { (b, s) =>
    s.pull.map {
      case Done() => Right(b)
      case Emit(c, rest) => Left((c.foldLeft(b)(f), rest))
    }
  }

  def toList: List[O] ! R = fold(List.empty[O])((os, o) => o :: os).map(_.reverse)

  /** all of it pulled, each value given to a program of the row */
  def foreach(f: O => Unit ! R): Unit ! R = evalMap(f).drain

  /** all of it pulled, its values dropped */
  def drain: Unit ! R = loop[Stream[O, R], Unit, R](this)(s => s.pull.map {
    case Done() => Right(())
    case Emit(_, rest) => Left(rest)
  })

  /** the first value: the rest released */
  def headOption: Option[O] ! R = pull.map {
    case Done() => None
    case Emit(c, rest) =>
      rest.release()
      Some(c.head)
  }

  def last: Option[O] ! R = loop[(Option[O], Stream[O, R]), Option[O], R]((None, this)) { (last, s) =>
    s.pull.map {
      case Done() => Right(last)
      case Emit(c, rest) => Left((Some(c.last), rest))
    }
  }

  /** at its end, or left, `fin` — once */
  private[std] def finishing(fin: () => Unit): Stream[O, R] = Stream(onAbort(pull)(fin).map {
    case Done() =>
      fin()
      Done()
    case Emit(c, rest) => Emit(c, rest.finishing(fin))
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

  /** how many values of a collection, an iterator, a range a step gives */
  val chunkSize = 256

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

  /** `c`'s values, then `rest`: `rest` if `c` is empty */
  private[std] def cons[O, R[_]](c: Chunk[O], rest: Stream[O, R]): Stream[O, R] =
    if c.isEmpty then rest else Stream(pure(Emit(c, rest)), rest.release)

  /** the values of `c` from `i` on, one a step, then `rest`: no copy of the chunk */
  private[std] def from[O, R[_]](c: Chunk[O], i: Int, rest: Stream[O, R]): Stream[O, R] =
    if i >= c.size then rest else Stream(pure(Emit(Chunk.single(c(i)), from(c, i + 1, rest))), rest.release)

  /** `evalMap` through a chunk, from `i`, by its index: no copy of the chunk */
  private[std] def evalAt[O, B, R[_]](c: Chunk[O], i: Int, rest: Stream[O, R], f: O => B ! R): Stream[B, R] =
    Stream(onAbort(f(c(i)))(rest.release).map(b =>
      Emit(Chunk.single(b), if i + 1 < c.size then evalAt(c, i + 1, rest, f) else rest.evalMap(f))), rest.release)

  /** `flatMap` through a chunk, from `i`, by its index: the next made when this value's stream ends */
  private[std] def flatAt[O, B, R[_]](c: Chunk[O], i: Int, rest: Stream[O, R], f: O => Stream[B, R]): Stream[B, R] =
    Stream(defer(f(c(i)).concat(if i + 1 < c.size then flatAt(c, i + 1, rest, f) else rest.flatMap(f)).pull),
      rest.release)

  private def indexed[O, R[_]](s: Stream[O, R], from: Long): Stream[(O, Long), R] =
    Stream(loop[(Long, Stream[O, R]), Step[(O, Long), R], R]((from, s)) { (i, s) =>
      s.pull.map {
        case Done() => Right(Done())
        case Emit(c, rest) => Right(Emit(c.zipWithIndex.map((o, j) => (o, i + j)), indexed(rest, i + c.size)))
      }
    }, s.release)

  /** `f` run at most once */
  private def once(f: () => Unit): () => Unit =
    val done = java.util.concurrent.atomic.AtomicBoolean(false)
    () => if done.compareAndSet(false, true) then f()

  /** A STREAM OF A RESOURCE: `acquire`d at the first pull, `use`d, and `release`d once — when the stream ends, when a
   * value's program fails, or when it is left (`take`, `headOption`, a merge stopped) */
  def bracket[S, O, R[_]](acquire: S ! R)(release: S => Unit)(use: S => Stream[O, R]): Stream[O, R] =
    Stream(acquire.flatMap(s => use(s).finishing(once(() => release(s))).pull))

  def empty[O, R[_]]: Stream[O, R] = Stream(pure(Done()))

  def emit[O, R[_]](o: O): Stream[O, R] = Stream(pure(Emit(Chunk.single(o), empty)))

  /** a chunk's values, as one step */
  def chunk[O, R[_]](c: Chunk[O]): Stream[O, R] = cons(c, empty)

  /** an iterable's values — a `List`, a `Vector`, a `Range`, a `Set`: its iterator's */
  def emits[O, R[_]](os: Iterable[O]): Stream[O, R] = fromIterator(os.iterator)

  /** an iterator's values: the iterator made at the first pull, read `chunk` values at a time, as far as asked;
   * each step remembered, so the stream pulled twice gives the same values */
  def fromIterator[O, R[_]](it: => Iterator[O], chunk: Int = chunkSize): Stream[O, R] =
    Stream(defer(stepping[O, R](it, chunk).pull))

  /** a lazy list's values, endless or not, as it makes them: `chunk` at a time, one unless said */
  def fromLazyList[O, R[_]](ll: LazyList[O], chunk: Int = 1): Stream[O, R] = Stream(defer(pure {
    val (c, more) = ll.splitAt(chunk)
    if c.isEmpty then Done() else Emit(Chunk.from(c), fromLazyList(more, chunk))
  }))

  /** a step of the iterator, taken once, when first pulled */
  private def stepping[O, R[_]](it: Iterator[O], chunk: Int): Stream[O, R] =
    lazy val step: Step[O, R] =
      // read by hand: an iterator's `take` may leave the iterator itself unusable (a `Vector`'s does)
      val b = ArraySeq.untagged.newBuilder[O]
      var n = 0
      while n < chunk && it.hasNext do
        b += it.next()
        n += 1
      if n == 0 then Done() else Emit(b.result(), stepping(it, chunk))
    Stream(defer(pure(step)))

  /** a program's value, as a stream of one */
  def eval[O, R[_]](p: O ! R): Stream[O, R] = Stream(p.map(o => Emit(Chunk.single(o), empty)))

  /** a value made by `p` at each pull: endless */
  def repeatEval[O, R[_]](p: O ! R): Stream[O, R] = Stream(p.map(o => Emit(Chunk.single(o), repeatEval(p))))

  /** from `s`, each step's value and the next state, until `None` */
  def unfold[S, O, R[_]](s: S)(f: S => Option[(O, S)] ! R): Stream[O, R] = Stream(f(s).map {
    case None => Done()
    case Some((o, next)) => Emit(Chunk.single(o), unfold(next)(f))
  })

  /** `a`, `f(a)`, `f(f(a))`, ...: endless, `chunk` values a step — made a chunk ahead, `f` being pure */
  def iterate[O, R[_]](a: O, chunk: Int = chunkSize)(f: O => O): Stream[O, R] = Stream(defer {
    val b = ArraySeq.untagged.newBuilder[O]
    var x = a
    var n = 0
    while n < chunk do
      b += x
      x = f(x)
      n += 1
    pure(Emit(b.result(), iterate(x, chunk)(f)))
  })

  /** from `from` up to `until`, not including it, `chunk` values a step */
  def range[R[_]](from: Long, until: Long, chunk: Int = chunkSize): Stream[Long, R] =
    if from >= until then empty
    else Stream(defer {
      val to = math.min(until, from + chunk)
      pure(Emit(Chunk.from(from until to), range(to, until, chunk)))
    })

  /** a generator, written with `Yield.emit`, as a stream: it runs to its next `emit` when pulled, and stops there */
  def generated[O, R[_], Rest[_]](p: Unit ! R)(using Split[Yield % O, R, Rest]): Stream[O, Rest] =
    Stream(handle(p, Handler.general[Yield % O, Rest, Unit, Step[O, Rest]](_ => pure(Done()))(
      [X] => (op: Yield[O, X], k: X => Step[O, Rest] ! Rest) => op match
        case Yield.Emit(o) => pure(Emit(Chunk.single(o), Stream(k(())))))))

  /** a queue's values, each taken when pulled: endless, until `None` is offered */
  def fromQueue[O, R[_]](q: Queue[Option[O]])(using In[Async, R]): Stream[O, R] = Stream(q.take[R].map {
    case None => Done()
    case Some(o) => Emit(Chunk.single(o), fromQueue(q))
  })

  extension [O, R[_]](s: Stream[Chunk[O], R])
    /** chunks given as values, as the stream's chunks */
    def unchunks: Stream[O, R] = Stream(loop[Stream[Chunk[O], R], Step[O, R], R](s) { s =>
      s.pull.map {
        case Done() => Right(Done())
        case Emit(cs, rest) =>
          val c = cs.flatten
          if c.isEmpty then Left(rest) else Right(Emit(c, rest.unchunks))
      }
    }, s.release)

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
        case Emit(c, rest) => pure(Emit(Chunk.single(c.head), inTurn(others.enqueue(from(c, 1, rest)))))
      }, releaseAll(q))

  private def releaseAll[O, R[_]](ss: Iterable[Stream[O, R]]): () => Unit =
    if ss.forall(_.release eq nothing) then nothing else () => ss.foreach(_.release())

  /** STREAMS OF `Async` MERGED AS THEIR VALUES COME: each stream's next pull started at once, all at the same time;
   * the first to come given out, and the next of its stream started. A stream that ends is left out; one that fails
   * fails the merged stream. Pulled a step ahead: while a chunk is used, each stream's next is on its way */
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
          case Emit(c, rest) => pure(Emit(c, racing(others :+ ((i, Async.start(rest.pull))))))
    }, releasing(pending))

  /** the pulls on their way, left: each stream's rest released when its pull comes */
  private def releasing[O](pending: List[(Int, Running[Step[O, Async]])]): () => Unit = () =>
    pending.foreach((_, r) => r.future.foreach {
      case Emit(_, rest) => rest.release()
      case _ => ()
    }(using ExecutionContext.parasitic))

  extension [O](s: Stream[O, Pure])
    /** a stream of no effect as a lazy list: each chunk pulled when the list is read that far — its other effects
     * handled first. A lazy list left unread cannot say so: a stream of a resource, read to its end */
    def toLazyList: LazyList[O] = LazyList.unfold(s)(pulled).flatMap(identity)

    /** a stream of no effect as an iterator: each chunk pulled when reached; `close` releases what is left */
    def iterator: Pulling[O] = Pulling(s)

  private def pulled[O](s: Stream[O, Pure]): Option[(Chunk[O], Stream[O, Pure])] = run(s.pull) match
    case Done() => None
    case Emit(c, rest) => Some((c, rest))

  /** an iterator pulling a stream of no effect, a chunk at a time; `close`, the rest released */
  final class Pulling[O](private var rest: Stream[O, Pure]) extends Iterator[O] with AutoCloseable:
    private var chunk: Chunk[O] = ArraySeq.untagged.empty
    private var at = 0
    private var ended = false
    def hasNext: Boolean =
      while at == chunk.size && !ended do pulled(rest) match
        case None => ended = true
        case Some((c, more)) =>
          chunk = c
          at = 0
          rest = more
      at < chunk.size
    def next(): O =
      if !hasNext then throw java.util.NoSuchElementException("the stream has ended")
      at += 1
      chunk(at - 1)
    def close(): Unit = if !ended then
      ended = true
      chunk = ArraySeq.untagged.empty
      at = 0
      rest.release()

  extension [O](s: Stream[O, Async])
    /** each value through `f`, up to `n` at once, the results in order */
    def parEvalMap[B](n: Int)(f: O => B ! Async)(using ExecutionContext): Stream[B, Async] =
      s.grouped(n).flatMap(group => Stream.eval[List[B], Async](Async.parTraverse(group)(f))
        .flatMap(bs => emits[B, Async](bs)))
