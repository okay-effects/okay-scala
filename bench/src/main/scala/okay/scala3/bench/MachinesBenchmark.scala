package okay.scala3.bench

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Param, Scope, Setup, State}

/** okay-simple's machine; okay-simple's programs on this machine; this library's own programs on it */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class MachinesBenchmark:
  @Param(Array("simple", "simpleOnCek", "cek"))
  var impl: String = ""
  var lane: Lane = null

  @Setup def setup(): Unit = lane = impl match
    case "simple" => Lanes(okay.effects.machine.Machine)
    case "simpleOnCek" => Lanes(okay.effects.cek.OnCek)
    case "cek" => NativeLanes()

  @Benchmark def binds: Int = lane.binds
  @Benchmark def askNear: Int = lane.askNear
  @Benchmark def askAcross4: Int = lane.askAcross4
  @Benchmark def stateLoop: Int = lane.stateLoop
  @Benchmark def askNearIn: Int = lane.askNearIn
  @Benchmark def askAcross4In: Int = lane.askAcross4In
  @Benchmark def stateLoopIn: Int = lane.stateLoopIn
