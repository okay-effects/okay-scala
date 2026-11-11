package okay.scala3

import okay.scala3.std.*

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration.*

/** `Loops` by the clock, beside the counts `LevelsSuite` checks: `sbt "coreJVM/Test/runMain okay.scala3.LevelsProbe"`
 * prints, for each loop, its levels and frames at two lengths and the time per element as the length doubles; with
 * words after it, the loops whose names have one of them */
object LevelsProbe:
  import Loops.*

  /** the least of five runs, in nanoseconds per element */
  def perElement(n: Int)(run: Int => Any): Double =
    (1 to 5).map { _ =>
      val t0 = System.nanoTime()
      run(n)
      (System.nanoTime() - t0).toDouble / n
    }.min

  /** the loops named, or every one */
  private var asked = Set.empty[String]
  private def wanted(name: String): Boolean = asked.isEmpty || asked.exists(name.contains)

  def doubling(name: String, from: Int, times: Int = 4)(run: Int => Any): Unit = if wanted(name) then
    run(from) // warm
    val lengths = (0 until times).map(from << _)
    println(f"$name%-28s ns/element at ${lengths.mkString(", ")}: " +
      lengths.map(n => f"${perElement(n)(run)}%.0f").mkString(", "))

  def depths(name: String)(at: Int => Any): Unit = if wanted(name) then
    println(f"$name%-28s (levels, frames) at 1000, 2000: ${at(1000)}, ${at(2000)}")

  def future(n: Int): Int =
    given ExecutionContext = Async.defaultContext
    Await.result(Async.toFuture(waitsInTail(n)), 60.seconds)

  def main(args: Array[String]): Unit =
    asked = args.toSet
    depths("ping-pong, into")(n => pingPong(n)._2)
    depths("round three rows")(n => round(n)._2)
    depths("the same row, into")(n => sameRow(n)._2)
    depths("a smaller row bound")(boundLoop)
    depths("a handler that resumes")(resumed)
    depths("Yield.foreach")(n => foreach(n)._2)
    depths("Yield.take")(n => taken(n)._2)
    depths("Stream.generated, fold")(n => pulled(n)._2)
    depths("Stream.generated, mapped")(n => pulledMapped(n)._2)
    depths("driven as Async, binds")(n => driven(waits(n))._2)
    depths("driven as Async, tail")(n => driven(waitsInTail(n))._2)
    doubling("ping-pong, into", 2000)(pingPong)
    doubling("round three rows", 2000)(round)
    doubling("the same row, into", 2000)(sameRow)
    doubling("a smaller row bound", 20000)(boundLoop)
    doubling("a handler that resumes", 20000)(resumed)
    doubling("Yield.foreach", 20000)(foreach)
    doubling("Yield.take", 20000)(taken)
    doubling("Stream.generated, fold", 20000)(pulled)
    doubling("Stream.generated, mapped", 20000)(pulledMapped)
    doubling("driven as Async, tail", 20000)(n => driven(waitsInTail(n)))
    doubling("Async.toFuture", 20000)(future)
    doubling("Lift.lower, Option", 20000)(n => Lift.lower(lifts(n)))
