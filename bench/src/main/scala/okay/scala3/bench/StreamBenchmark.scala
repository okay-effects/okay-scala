package okay.scala3.bench

import okay.scala3.*
import okay.scala3.std.*
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Mode, OutputTimeUnit, Scope as JScope, State as JState}

import java.util.concurrent.TimeUnit

/** streams pulled to their end: pure pipelines, from a collection, nested, with an effect per value, zipped */
@JState(JScope.Benchmark)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class StreamBenchmark:
  val n = 100_000
  val vector: Vector[Long] = Vector.range(0L, n.toLong)

  type S[X] = (State % Long)[X]

  @Benchmark def rangeMapFilterFold: Long =
    run(Stream.range[Pure](0, n).map(_ * 2).filter(_ % 3 == 0).fold(0L)(_ + _))

  @Benchmark def emitsFold: Long = run(Stream.emits[Long, Pure](vector).fold(0L)(_ + _))

  @Benchmark def flatMapPairs: Long =
    run(Stream.range[Pure](0, n / 10).flatMap(i => Stream.emits(List(i, i))).fold(0L)(_ + _))

  @Benchmark def evalMapState: Long =
    run(State.run(0L)(Stream.range[S](0, n / 10).evalTap(i => State.modify[Long, S](_ + i)).drain))._1

  @Benchmark def iterateTake: Long = run(Stream.iterate[Long, Pure](0L)(_ + 1).take(n).fold(0L)(_ + _))

  @Benchmark def zipFold: Long =
    run(Stream.range[Pure](0, n / 2).zip(Stream.range[Pure](0, n / 2)).fold(0L)((s, p) => s + p._1 + p._2))
