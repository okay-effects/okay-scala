package okay.scala3.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, State}
import okay.scala3.*

/** the machine on okay-simple's benchmark programs, in this library's words. okay-simple measures its own machine
 * against this one on the same programs, in its own benchmarks */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class CekBenchmark:
  val lane = NativeLanes()

  @Benchmark def binds: Int = lane.binds
  @Benchmark def askNear: Int = lane.askNear
  @Benchmark def askAcross4: Int = lane.askAcross4
  @Benchmark def stateLoop: Int = lane.stateLoop
  @Benchmark def askNearIn: Int = lane.askNearIn
  @Benchmark def askAcross4In: Int = lane.askAcross4In
  @Benchmark def stateLoopIn: Int = lane.stateLoopIn
  /** a run of one step: what a run costs beside its steps — the nested-run mark set and cleared (`Cek.run`) */
  @Benchmark def oneStepRun: Int = run(pure[Int, Pure](1).map(_ + 1))
