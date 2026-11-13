package okay.scala3

import okay.scala3.std.*

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.*

/** the mark is the thread's: a run begun on another thread while a program runs here — a platform thread, a virtual
 * one, a pool's — is a run of its own, not refused; and a program resumed on another thread runs there */
class NestedRunThreadsSuite extends munit.FunSuite:
  /** `body` on a thread made by `start`, joined: its value, or what it threw */
  def on(start: Runnable => Thread)(body: => Int): Int =
    val got = AtomicReference[Either[Throwable, Int]]()
    start(() => got.set(try Right(body) catch case e: Throwable => Left(e))).join()
    got.get.fold(throw _, identity)

  test("a run on another thread, begun by a step of a run here: not refused") {
    val platform = run(pure[Int, Pure](1).map(x => x + on(r => Thread.ofPlatform().start(r))(run(pure(41)))))
    assertEquals(platform, 42)
    val virtual = run(pure[Int, Pure](1).map(x => x + on(r => Thread.ofVirtual().start(r))(run(pure(41)))))
    assertEquals(virtual, 42)
  }

  test("a run nested on a virtual thread's own stack: refused there as anywhere") {
    intercept[NestedRun](on(r => Thread.ofVirtual().start(r))(run(pure[Int, Pure](1).map(x => x + run(pure(1))))))
  }

  test("a program waiting on futures, resumed on the pool's threads, while runs go on here") {
    given ExecutionContext = ExecutionContext.global
    def steps(i: Int): Int ! Async =
      if i == 0 then pure(0) else Async.fromFuture[Int, Async](Future(1)).flatMap(x => steps(i - 1).map(_ + x))
    val running = Async.toFuture(steps(1000))
    var here = 0
    while !running.isCompleted do here += run(pure[Int, Pure](1))
    assertEquals(Await.result(running, 10.seconds), 1000)
    assert(here > 0)
  }
