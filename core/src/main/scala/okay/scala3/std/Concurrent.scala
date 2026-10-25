package okay.scala3.std

import okay.scala3.*

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

/*
 * SHARED STATE FOR FIBERS: a reference changed atomically, a value given once, permits, a queue — each wait an `Async`,
 * cancellable, no thread blocked. Each made by a program (`Ref.make`), so a program run twice makes its own.
 */

/** a value shared by fibers, changed atomically */
final class Ref[A] private (init: A):
  private val cell = AtomicReference[A](init)

  def get[R[_]]: A ! R = defer(pure(cell.get))
  def set[R[_]](a: A): Unit ! R = defer(pure(cell.set(a)))
  def update[R[_]](f: A => A): Unit ! R = modify(a => (f(a), ()))
  def getAndUpdate[R[_]](f: A => A): A ! R = modify(a => (f(a), a))
  def updateAndGet[R[_]](f: A => A): A ! R = modify { a => val b = f(a); (b, b) }

  /** the value through `f`, atomically: its new value, and what is given back */
  def modify[B, R[_]](f: A => (A, B)): B ! R = defer(pure(modifyNow(f)))

  @tailrec private def modifyNow[B](f: A => (A, B)): B =
    val a = cell.get
    val (next, b) = f(a)
    if cell.compareAndSet(a, next) then b else modifyNow(f)

object Ref:
  def make[A, R[_]](a: A): Ref[A] ! R = defer(pure(new Ref(a)))

/** a value given once, waited for by any number of fibers */
final class Deferred[A] private ():
  private var value: Option[A] = None
  private var waiting = Vector.empty[Either[Throwable, A] => Unit]

  /** the value given, the waiting resumed: `false` if it was given before, this one dropped */
  def complete[R[_]](a: A): Boolean ! R = defer(pure(completeNow(a)))

  /** the value, waited for if not given yet; the wait cancelled, it waits no more */
  def get[R[_]](using In[Async, R]): A ! R = Async.cancellable[A, R] { cb =>
    val now = synchronized:
      if value.isEmpty then waiting :+= cb
      value
    now match
      case Some(a) =>
        cb(Right(a))
        Async.noCancel
      case None => () => synchronized { waiting = waiting.filterNot(_ eq cb) }
  }

  /** the value if given */
  def tryGet[R[_]]: Option[A] ! R = defer(pure(synchronized(value)))

  private def completeNow(a: A): Boolean =
    val resumed = synchronized:
      if value.isDefined then None
      else
        value = Some(a)
        val ws = waiting
        waiting = Vector.empty
        Some(ws)
    resumed.foreach(_.foreach(_(Right(a))))
    resumed.isDefined

object Deferred:
  def make[A, R[_]]: Deferred[A] ! R = defer(pure(new Deferred[A]))

/** permits, at most `n` held at once: `withPermit` around what must not run more than `n` times at once */
final class Semaphore private[std] (private var free: Long):
  /** one wait for a permit: given it, the program not yet resumed, a cancel gives it back */
  private final class Grant(val resume: Either[Throwable, Grant] => Unit)
  private val waiting = scala.collection.mutable.Queue.empty[Grant]
  private val granted = scala.collection.mutable.Set.empty[Grant]

  /** a permit, waited for if none is free; the wait cancelled, no permit taken */
  def acquire[R[_]](using In[Async, R]): Unit ! R =
    Async.cancellable[Grant, R] { cb =>
      val g = Grant(cb)
      val now = synchronized:
        if free > 0 then
          free -= 1
          true
        else
          waiting.enqueue(g)
          false
      if now then
        cb(Right(g))
        Async.noCancel
      else () =>
        val giveBack = synchronized:
          waiting.removeFirst(_ eq g)
          granted.remove(g)
        if giveBack then releaseNow()
    }.flatMap(g => defer(pure(synchronized { granted.remove(g); () })))

  /** a permit given back: to the first waiting, or free */
  def release[R[_]]: Unit ! R = defer(pure(releaseNow()))

  /** `p` with a permit: given back when `p` ends, fails or is dropped */
  def withPermit[A, R[_]](p: A ! R)(using In[Async, R]): A ! R =
    acquire[R].flatMap(_ => ensuring(p)(releaseNow()))

  /** the permits free now */
  def available[R[_]]: Long ! R = defer(pure(synchronized(free)))

  private def releaseNow(): Unit =
    val next = synchronized:
      if waiting.isEmpty then
        free += 1
        None
      else
        val g = waiting.dequeue()
        granted += g
        Some(g)
    next.foreach(g => g.resume(Right(g)))

object Semaphore:
  def make[R[_]](permits: Long): Semaphore ! R = defer(pure(new Semaphore(permits)))

/** a queue between fibers: `take` waits while it is empty, `offer` while it is full */
final class Queue[A] private (capacity: Long):
  private val items = scala.collection.mutable.Queue.empty[A]
  private val filled = new Semaphore(0)
  private val space = new Semaphore(capacity)

  /** `a` put at the end, waiting while the queue is full */
  def offer[R[_]](a: A)(using In[Async, R]): Unit ! R =
    space.acquire[R].flatMap(_ => defer { synchronized(items.enqueue(a)); filled.release[R] })

  /** the first value, waiting while the queue is empty */
  def take[R[_]](using In[Async, R]): A ! R =
    filled.acquire[R].flatMap(_ => defer { val a = synchronized(items.dequeue()); space.release[R].as(a) })

  /** the values in the queue now */
  def size[R[_]]: Int ! R = defer(pure(synchronized(items.size)))

object Queue:
  /** a queue of at most `capacity` values */
  def bounded[A, R[_]](capacity: Long): Queue[A] ! R = defer(pure(new Queue[A](capacity)))
  /** a queue with no bound: `offer` never waits */
  def unbounded[A, R[_]]: Queue[A] ! R = bounded(Long.MaxValue)
