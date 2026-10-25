package okay.scala3

import okay.scala3.std.*

import scala.concurrent.ExecutionContext

/** streams, pulled: as far as pulled, of any effects, from generators, merged — on every platform */
class StreamSuite extends munit.FunSuite:
  given ExecutionContext = Async.defaultContext

  test("pure combinators") {
    val s = Stream.range[Pure](0, 10).filter(_ % 2 == 0).map(_ * 10)
    assertEquals(run(s.toList), List(0L, 20L, 40L, 60L, 80L))
    assertEquals(run(s.take(2).toList), List(0L, 20L))
    assertEquals(run(s.drop(3).toList), List(60L, 80L))
    assertEquals(run(s.zipWithIndex.map(_._2).toList), List(0L, 1L, 2L, 3L, 4L))
    assertEquals(run(Stream.emits[Int, Pure](List(1, 2, 3)).scan(0)(_ + _).toList), List(0, 1, 3, 6))
    assertEquals(run(Stream.range[Pure](0, 5).grouped(2).toList), List(List(0L, 1L), List(2L, 3L), List(4L)))
    assertEquals(run(Stream.emits[Int, Pure](List(1, 2)).flatMap(i => Stream.emits(List(i, i))).toList),
      List(1, 1, 2, 2))
    assertEquals(run((Stream.emit[Int, Pure](1) ++ Stream.emit(2)).takeWhile(_ < 2).toList), List(1))
  }

  test("endless, pulled only as far as taken; long, in constant stack") {
    assertEquals(run(Stream.iterate[Int, Pure](1)(_ * 2).take(5).toList), List(1, 2, 4, 8, 16))
    assertEquals(run(Stream.range[Pure](0, 1_000_000).filter(_ % 1000 == 0).fold(0L)(_ + _)), 499500000L)
    assertEquals(run(Stream.range[Pure](0, 100_000).flatMap(i => Stream.emits(List(i, i))).fold(0L)((n, _) => n + 1)),
      200000L)
  }

  test("effects in a stream: state counted, a failure stops it") {
    type R[X] = (State % Int + Fail % String)[X]
    val s = Stream.range[R](1, 10).evalTap(_ => State.modify[Int, R](_ + 1))
      .evalMap(n => if n == 4 then Fail.raise[String, R]("four") else pure[Long, R](n))
    assertEquals(run(Fail.run(State.run(0)(s.take(3).toList))), Right((3, List(1L, 2L, 3L))))
    assertEquals(run(Fail.run(State.run(0)(s.toList))), Left("four"))
  }

  test("a generator written with emit is a stream: it runs only as far as pulled") {
    var made = 0
    def gen[R[_]: Has[Yield % Int]]: Unit ! R =
      loop[Int, Unit, R](0)(i => defer { made += 1; Yield.emit[Int, R](i) }.as(Left(i + 1)))
    assertEquals(run(Stream.generated[Int, Yield % Int, Pure](gen).take(3).toList), List(0, 1, 2))
    assertEquals(made, 3)
  }

  test("asynchronous: merged as they come, evaluated n at once") {
    def ticks(name: String, ms: Long): Stream[String, Async] =
      Stream.range[Async](0, 3).evalMap(i => Async.sleep[Async](ms).as(s"$name$i"))
    val merged = Stream.mergeAsync(ticks("a", 10), ticks("b", 15)).toList
    val t0 = System.nanoTime()
    val par = Stream.range[Async](0, 8).parEvalMap(4)(i => Async.sleep[Async](50).as(i * 2)).toList
    for
      m <- Async.toFuture(merged)
      p <- Async.toFuture(par)
    yield
      assertEquals(m.sorted, List("a0", "a1", "a2", "b0", "b1", "b2"))
      assertEquals(m.filter(_.startsWith("a")), List("a0", "a1", "a2"))
      assertEquals(p, List(0L, 2L, 4L, 6L, 8L, 10L, 12L, 14L))
      assert((System.nanoTime() - t0) / 1000000 < 380, "not 4 at once")
  }

  test("merge: in turn, of any row, an ended stream left out; interleave") {
    val merged = Stream.merge(Stream.range[Pure](0, 3), Stream.emits[Long, Pure](List(10L, 11L)), Stream.empty)
    assertEquals(run(merged.toList), List(0L, 10L, 1L, 11L, 2L))
    assertEquals(run(Stream.emits[Int, Pure](List(1, 2, 3)).interleave(Stream.emit(9)).toList), List(1, 9, 2, 3))
    type R[X] = (State % Int)[X]
    val counted = Stream.repeatEval[Int, R](State.get[Int, R].flatMap(n => State.put[Int, R](n + 1).as(n)))
    assertEquals(run(State.run(0)(Stream.merge(counted, counted.map(-_)).take(4).toList)), (4, List(0, -1, 2, -3)))
  }

  test("mergeAsync: one failing fails the merged stream") {
    val bad: Stream[Int, Async] = Stream.eval[Int, Async](Async.sleep[Async](20)
      .andThen(Async.async[Int, Async](cb => cb(Left(IllegalStateException("bad"))))))
    val slow: Stream[Int, Async] = Stream.iterate[Int, Async](0)(_ + 1).evalMap(i => Async.sleep[Async](10).as(i))
    Async.toFuture(Stream.mergeAsync(bad, slow).toList).failed.map(e => assertEquals(e.getMessage, "bad"))
  }

  test("from a queue until None") {
    val p: List[Int] ! Async =
      for
        q <- Queue.unbounded[Option[Int], Async]
        _ <- Async.fork[Unit, Async](Stream.emits[Int, Async](List(1, 2, 3)).foreach(i => q.offer[Async](Some(i)))
               .andThen(q.offer[Async](None)))
        vs <- Stream.fromQueue[Int, Async](q).toList
      yield vs
    Async.toFuture(p).map(vs => assertEquals(vs, List(1, 2, 3)))
  }
