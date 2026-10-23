package okay.scala3.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope, State}

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
