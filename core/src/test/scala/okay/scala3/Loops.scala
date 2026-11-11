package okay.scala3

import okay.scala3.std.*

/** LOOPS THAT MUST GATHER NOTHING: programs resumed turn after turn, and calls in tail position from row to row —
 * each with the most levels and frames a `k` held on the way (`Depth`) */
object Loops:
  import Depth.{here, probed}

  type Forth[X] = (State % Int + Probe)[X]
  type Back[X] = (Probe + State % Int)[X]

  /** probed, and one added to the state */
  def tick[R[_]](using In[Probe, R], In[State % Int, R]): Unit ! R = here[R].flatMap(_ => State.modify[Int, R](_ + 1))

  // calls in tail position between two orders of one row: each puts the program called into the caller's row
  def ping(n: Int): Unit ! Forth = if n == 0 then unit else tick[Forth].flatMap(_ => pong(n - 1).into[Forth])
  def pong(n: Int): Unit ! Back = if n == 0 then unit else tick[Back].flatMap(_ => ping(n - 1).into[Back])
  def pingPong(n: Int): (Int, (Int, Int)) = run(State.run(0)(probed(ping(n)))) match
    case (s, (_, most)) => (s, most)

  type One[X] = (State % Int + Probe + Reader % Int)[X]
  type Two[X] = (Probe + Reader % Int + State % Int)[X]
  type Three[X] = (Reader % Int + State % Int + Probe)[X]
  def one(n: Int): Unit ! One = if n == 0 then unit else tick[One].flatMap(_ => two(n - 1).into[One])
  def two(n: Int): Unit ! Two = if n == 0 then unit else tick[Two].flatMap(_ => three(n - 1).into[Two])
  def three(n: Int): Unit ! Three = if n == 0 then unit else tick[Three].flatMap(_ => one(n - 1).into[Three])
  def round(n: Int): (Int, (Int, Int)) = run(Reader.run(2)(State.run(0)(probed(one(n))))) match
    case (s, (_, most)) => (s, most)

  /** a call in tail position into the same row, said by `into` */
  def same(n: Int): Unit ! Forth = if n == 0 then unit else tick[Forth].flatMap(_ => same(n - 1).into[Forth])
  def sameRow(n: Int): (Int, (Int, Int)) = run(State.run(0)(probed(same(n)))) match
    case (s, (_, most)) => (s, most)

  // a program of a smaller row bound at every turn of a loop: put into the row at the bind
  val small: Unit ! Probe = here[Probe].flatMap(_ => here[Probe])
  def bound(n: Int): Unit ! Forth = if n == 0 then unit else small.flatMap(_ => bound(n - 1))
  def boundLoop(n: Int): (Int, Int) = run(State.run(0)(probed(bound(n))))._2._2

  // a general handler that resumes in a loop
  def numbers[R[_]](n: Int)(using In[Probe, R]): Unit ! R =
    if n == 0 then unit else here[R].flatMap(_ => numbers(n - 1))
  def resumed(n: Int): (Int, Int) = run(probed(numbers[Probe](n)))._2

  // a generator, each value probed: looped over, cut, pulled as a stream
  type Gen[X] = (Probe + Yield % Int)[X]
  def gen(n: Int): Unit ! Gen =
    if n == 0 then unit else here[Gen].flatMap(_ => Yield.emit[Int, Gen](n)).flatMap(_ => gen(n - 1))
  def foreach(n: Int): (Int, (Int, Int)) = defer {
    var sum = 0
    probed(Yield.foreach[Int, Gen, Probe, Unit](gen(n))(o => defer(pure { sum += o }))).map((_, most) => (sum, most))
  }.pipe(run)
  def taken(n: Int): (Int, (Int, Int)) = run(probed(Yield.take[Int, Gen, Probe, Unit](n)(gen(n + 1)))) match
    case ((os, _), most) => (os.size, most)
  def pulled(n: Int): (Int, (Int, Int)) = run(probed(Stream.generated[Int, Gen, Probe](gen(n)).fold(0)(_ + _)))
  def pulledMapped(n: Int): (Int, (Int, Int)) =
    run(probed(Stream.generated[Int, Gen, Probe](gen(n)).map(_ + 1).filter(_ % 2 == 0).evalMap(o => pure[Int, Probe](o))
      .fold(0)(_ + _)))

  extension [A](a: A) def pipe[B](f: A => B): B = f(a)

  // the asynchronous driver's way: run to a wait, the rest resumed with the answer, again and again
  enum Waiting[A]:
    case Done(a: A)
    case Suspended[A, X](register: (Either[Throwable, X] => Unit) => (() => Unit), k: X => Waiting[A] ! Pure)
      extends Waiting[A]

  def waits(n: Int): Int ! Async =
    if n == 0 then pure(0) else Async.async[Int, Async](cb => cb(Right(1))).flatMap(x => waits(n - 1).map(_ + x))
  def waitsInTail(n: Int, sum: Int = 0): Int ! Async =
    if n == 0 then pure(sum) else Async.async[Int, Async](cb => cb(Right(1))).flatMap(x => waitsInTail(n - 1, sum + x))

  /** `p` driven as `Async` drives it, by hand: its value, and the most levels and frames a rest held */
  def driven[A](p: A ! Async): (A, (Int, Int)) =
    var most = (0, 0)
    var next: Waiting[A] ! Pure = handle(p, Handler.general[Async, Pure, A, Waiting[A]](a => pure(Waiting.Done(a)))(
      [X] => (op: Async[X], k: X => Waiting[A] ! Pure) => op match
        case Async.Suspend(register) => pure(Waiting.Suspended(register, k))))
    var result = Option.empty[A]
    while result.isEmpty do run(next) match
      case Waiting.Done(a) => result = Some(a)
      case s: Waiting.Suspended[a, x] =>
        var got = Option.empty[Either[Throwable, x]]
        s.register(r => got = Some(r))
        got match
          case Some(Right(x)) =>
            next = s.k(x)
            val (levels, frames) = Depth.of(next)
            most = (most._1.max(levels), most._2.max(frames))
          case other => sys.error(s"an answer at once was expected: $other")
    (result.get, most)

  // a program of another monad lowered: each lifted value bound there
  def lifts(n: Int, sum: Int = 0): Int ! Lifted[Option] =
    if n == 0 then pure(sum) else Lift.lift[Option, Int, Lifted[Option]](Some(1)).flatMap(x => lifts(n - 1, sum + x))
