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

  test("iterators, iterables, lazy lists: both ways; pulled twice, the same values") {
    val once = Stream.fromIterator[Int, Pure](Iterator(1, 2, 3))
    assertEquals(run(once.toList), List(1, 2, 3))
    val twice = Stream.emits[Int, Pure](Vector(1, 2))
    assertEquals(run(twice.toList) ++ run(twice.toList), List(1, 2, 1, 2))
    var made = 0
    val ll = LazyList.iterate(0)(i => { made += 1; i + 1 })
    assertEquals(run(Stream.fromLazyList[Int, Pure](ll).take(3).toList), List(0, 1, 2))
    assert(made <= 3, s"made $made")
    var pulled = 0
    val counted = Stream.iterate[Int, Pure](0)(_ + 1).evalMap(i => defer { pulled += 1; pure(i) })
    val lazyList = counted.toLazyList
    assertEquals(pulled, 0)
    assertEquals(lazyList.take(4).toList, List(0, 1, 2, 3))
    assertEquals(counted.iterator.drop(2).next(), 2)
    type R[X] = (State % Int)[X]
    val effectful = Stream.range[R](0, 3).evalTap(_ => State.modify[Int, R](_ + 1))
    assertEquals(Stream.fromIterator[Long, Pure](
      run(State.run(0)(effectful.toList))._2.iterator).toLazyList.toList, List(0L, 1L, 2L))
  }

  /** a resource counted: opened, closed */
  final class Counted:
    var opened, closed = 0
    def stream[R[_]](n: Long): Stream[Long, R] =
      Stream.bracket[Unit, Long, R](defer(pure(opened += 1)))(_ => closed += 1)(_ => Stream.range[R](0, n))
    def balanced = opened == closed

  test("a resource closed once at its end, and when left: take, takeWhile, headOption, zip, flatMap, merge") {
    val c = Counted()
    assertEquals(run(c.stream[Pure](3).toList), List(0L, 1L, 2L))
    assertEquals(run(c.stream[Pure](100).take(2).toList), List(0L, 1L))
    assertEquals(run(c.stream[Pure](100).takeWhile(_ < 2).toList), List(0L, 1L))
    assertEquals(run(c.stream[Pure](100).headOption), Some(0L))
    assertEquals(run(c.stream[Pure](100).zip(c.stream[Pure](2)).toList), List((0L, 0L), (1L, 1L)))
    assertEquals(run(c.stream[Pure](2).zip(c.stream[Pure](100)).toList), List((0L, 0L), (1L, 1L)))
    assertEquals(run(c.stream[Pure](100).flatMap(i => c.stream[Pure](10).map(_ + i * 10)).take(13).toList).last, 12L)
    assertEquals(run(Stream.merge(c.stream[Pure](100), c.stream[Pure](100)).take(3).toList), List(0L, 0L, 1L))
    assertEquals(run(c.stream[Pure](100).map(_ + 1).filter(_ > 3).drop(1).take(1).toList), List(5L))
    assertEquals(run(c.stream[Pure](100).grouped(3).take(2).toList).flatten.size, 6)
    assertEquals(run(c.stream[Pure](100).interleave(c.stream[Pure](100)).take(3).toList), List(0L, 0L, 1L))
    assertEquals(run((c.stream[Pure](2) ++ c.stream[Pure](100)).take(3).toList), List(0L, 1L, 0L))
    assert(c.balanced, s"opened ${c.opened}, closed ${c.closed}")
    assert(c.opened >= 15, s"opened ${c.opened}")
  }

  test("a resource closed when a value's program fails, or the stream's own pull") {
    type F[X] = (Fail % String)[X]
    val c = Counted()
    val failing = c.stream[F](100).evalMap(i => if i == 2 then Fail.raise[String, F]("two") else pure[Long, F](i))
    assertEquals(run(Fail.run(failing.toList)), Left("two"))
    val inner = c.stream[F](100).flatMap(i => c.stream[F](3).evalMap(j =>
      if i == 1 && j == 1 then Fail.raise[String, F]("inner") else pure[Long, F](j)))
    assertEquals(run(Fail.run(inner.toList)), Left("inner"))
    val zipped = c.stream[F](100).zip(c.stream[F](100).evalMap(j =>
      if j == 3 then Fail.raise[String, F]("zip") else pure[Long, F](j)))
    assertEquals(run(Fail.run(zipped.toList)), Left("zip"))
    assert(c.balanced, s"opened ${c.opened}, closed ${c.closed}")
    assertEquals(c.opened, 6)
  }

  test("an iterator closed, the rest released; mergeAsync stopped, its streams released") {
    val c = Counted()
    val it = c.stream[Pure](100).iterator
    assertEquals(it.take(2).toList, List(0L, 1L))
    it.close()
    assert(c.balanced, s"opened ${c.opened}, closed ${c.closed}")
    val merged = Stream.mergeAsync(c.stream[Async](100), c.stream[Async](100)).take(3).toList
    Async.toFuture(merged).flatMap(vs => Async.toFuture(Async.sleep[Async](50)).map { _ =>
      assertEquals(vs.size, 3)
      assert(c.balanced, s"opened ${c.opened}, closed ${c.closed}")
    })
  }

  test("chunks: a range, a collection read many at a time; a program per value still as far as pulled") {
    assertEquals(run(Stream.range[Pure](0, 600).chunks.map(_.size).toList), List(256, 256, 88))
    for big <- List[Iterable[Long]](Vector.range(0L, 1000L), List.range(0L, 1000L), (0L until 1000L), Array.range(0, 1000)
        .map(_.toLong).toSeq) do
      assertEquals(run(Stream.emits[Long, Pure](big).fold(0L)(_ + _)), 499500L, big.getClass.getName)
    assertEquals(run(Stream.range[Pure](0, 10, chunk = 4).chunks.map(_.toList).toList),
      List(List(0L, 1L, 2L, 3L), List(4L, 5L, 6L, 7L), List(8L, 9L)))
    assertEquals(run(Stream.range[Pure](0, 10).chunkLimit(3).chunks.map(_.size).toList), List(3, 3, 3, 1))
    assertEquals(run(Stream.range[Pure](0, 10).chunks.unchunks.toList), (0L until 10L).toList)
    assertEquals(run(Stream.range[Pure](0, 10).take(3).chunks.map(_.size).toList), List(3))
    var ran = 0
    val effects = Stream.range[Pure](0, 1000).evalMap(i => defer { ran += 1; pure[Long, Pure](i) })
    assertEquals(run(effects.take(3).toList), List(0L, 1L, 2L))
    assertEquals(ran, 3)
    assertEquals(run(Stream.range[Pure](0, 7).zip(Stream.range[Pure](0, 7, chunk = 2)).map((a, b) => a - b).toList),
      List.fill(7)(0L))
    assertEquals(run(Stream.range[Pure](0, 7).grouped(3).toList).map(_.size), List(3, 3, 1))
    assertEquals(run(Stream.range[Pure](0, 600).scan(0L)(_ + _).last), Some((0L until 600L).sum))
  }
