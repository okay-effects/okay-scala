package okay.scala3.bench

import okay.scala3.*
import okay.scala3.std.*
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope as JScope, State as JState}

import java.util.concurrent.TimeUnit

/** the handlers that leave the rest of a program, or keep it: a failure, a choice of nothing, a generator looped
 * over, cut and pulled, an interceptor, a finalizer — `sbt "bench/Jmh/run -prof gc HandlersBenchmark"` */
@JState(JScope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class HandlersBenchmark:
  val n = 1000

  type F[X] = (Fail % String)[X]
  def deep(i: Int): Int ! F = if i == 0 then Fail.raise[String, F]("no") else deep(i - 1).map(_ + 1)
  /** a failure under a thousand binds */
  @Benchmark def failDeep: Any = run(Fail.run(deep(n)))

  def attempts(i: Int): Int ! Pure =
    if i == 0 then pure(0)
    else Fail.run[String, F, Pure, Int](Fail.raise[String, F]("no")).flatMap(_ => attempts(i - 1))
  /** a thousand failures, each under its own handler */
  @Benchmark def failLoop: Any = run(attempts(n))

  type C[X] = Choose[X]
  def queens(size: Int): List[Int] ! C =
    def place(row: Int, placed: List[Int]): List[Int] ! C =
      if row == size then pure(placed)
      else Choose.among[Int, C]((0 until size).toList).flatMap { col =>
        val safe = placed.zipWithIndex.forall((c, i) => c != col && (c - col).abs != i + 1)
        Choose.guard[C](safe).flatMap(_ => place(row + 1, col :: placed))
      }
    place(0, Nil)
  @Benchmark def queensAll: Any = run(Choose.all(queens(6)))
  @Benchmark def queensFirst: Any = run(Choose.first(queens(8)))

  type S[X] = (State % Int)[X]
  var released = 0
  def held(i: Int): Unit ! S =
    if i == 0 then unit else ensuring(State.modify[Int, S](_ + 1))(released += 1).flatMap(_ => held(i - 1))
  /** a thousand finalizers, each a thunk, at a value */
  @Benchmark def ensuringThunk: Any = run(State.run(0)(held(n)))

  type Y[X] = (Yield % Int)[X]
  def gen(i: Int): Unit ! Y = if i == 0 then unit else Yield.emit[Int, Y](i).flatMap(_ => gen(i - 1))
  @Benchmark def foreach: Any = run(Yield.foreach[Int, Y, Pure, Unit](gen(n))(_ => unit))
  @Benchmark def take: Any = run(Yield.take[Int, Y, Pure, Unit](n / 2)(gen(n)))
  @Benchmark def generated: Any = run(Stream.generated[Int, Y, Pure](gen(n)).fold(0)(_ + _))

  def counting(i: Int): Unit ! S =
    if i == 0 then unit else State.get[Int, S].flatMap(s => State.put[Int, S](s + 1)).andThen(counting(i - 1))
  /** two thousand operations, each seen by an interceptor */
  @Benchmark def observed: Any =
    run(State.run(0)(observe[State % Int, S, Unit](counting(n))([X] => (_: State[Int, X]) => unit[S])))
