package okay.scala3.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, State as JmhState}
import okay.scala3.*
import okay.scala3.std.*

/** A PROGRAM OF A SMALLER ROW BOUND IN A BIGGER ONE: what each operation costs as it passes the renaming. `sameRow`
 * asks with no renaming, the baseline; `embedded` binds a program of `State % Int` in `State % Int + Reader % Int`
 * (the row at the start: `Sub.Prefix`, a place made per operation); `embeddedAbstract` the same, the program's row
 * an abstract `R` */
@JmhState(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class EmbedBenchmark:
  type S[X] = (State % Int)[X]
  type SR[X] = (S + Reader % Int)[X]
  val n = 1000

  def tickIn: Unit ! SR = State.get[Int, SR].flatMap(s => State.put[Int, SR](s + 1))
  def sameLoop(i: Int): Unit ! SR = if i == 0 then unit else tickIn.flatMap(_ => sameLoop(i - 1))

  val tick: Unit ! S = State.get[Int, S].flatMap(s => State.put[Int, S](s + 1))
  def embeddedLoop(i: Int): Unit ! SR = if i == 0 then unit else tick.flatMap(_ => embeddedLoop(i - 1))

  def tickOf[R[_]: Has[State % Int]]: Unit ! R = State.get[Int, R].flatMap(s => State.put[Int, R](s + 1))
  def abstractLoop[R[_]: Has[State % Int]](i: Int): Unit ! R + Reader % Int =
    if i == 0 then unit else tickOf[R].flatMap(_ => abstractLoop[R](i - 1))

  @Benchmark def sameRow: Int = run(Reader.run(0)(State.run(0)(sameLoop(n))))._1
  @Benchmark def embedded: Int = run(Reader.run(0)(State.run(0)(embeddedLoop(n))))._1
  @Benchmark def embeddedAbstract: Int = run(Reader.run(0)(State.run(0)(abstractLoop[S](n))))._1
