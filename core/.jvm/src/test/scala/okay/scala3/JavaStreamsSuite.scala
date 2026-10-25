package okay.scala3

import okay.scala3.std.*
import okay.scala3.std.JavaStreams.*

/** Java's streams and iterators, both ways */
class JavaStreamsSuite extends munit.FunSuite:
  test("a Java stream as a stream: pulled lazily, closed at its end and when left") {
    var closed = false
    var made = 0
    def js = java.util.stream.Stream.iterate(1, (i: Int) => { made += 1; i + 1 }).limit(100)
      .onClose(() => closed = true)
    assertEquals(run(JavaStreams.from[Int, Pure](js, chunk = 1).take(3).toList), List(1, 2, 3))
    assert(made < 10, s"pulled $made ahead")
    made = 0
    closed = false
    assertEquals(run(JavaStreams.from[Int, Pure](js, chunk = 10).take(3).toList), List(1, 2, 3))
    assert(made < 20, s"pulled $made ahead, not a chunk")
    assert(closed, "left by take, not closed")
    closed = false
    assertEquals(run(JavaStreams.from[Int, Pure](js).fold(0)(_ + _)), 5050)
    assert(closed)
    assertEquals(run(JavaStreams.from[Integer, Pure](java.util.stream.IntStream.range(0, 4)).map(_.toInt).toList),
      List(0, 1, 2, 3))
  }

  test("a Java iterator; a stream as a Java stream and iterator") {
    val list = java.util.List.of("a", "b", "c")
    assertEquals(run(JavaStreams.fromIterator[String, Pure](list.iterator()).toList), List("a", "b", "c"))
    val s = Stream.iterate[Int, Pure](1)(_ * 2)
    assertEquals(s.toJavaStream.limit(5).map(_ + 1).toList.toString, "[2, 3, 5, 9, 17]")
    val it = s.take(2).javaIterator
    assertEquals(List(it.next(), it.next(), it.hasNext), List(1, 2, false))
    var closed = false
    val resource = Stream.bracket[Int, Int, Pure](pure(0))(_ => closed = true)(_ => Stream.iterate(0)(_ + 1))
    val js = resource.toJavaStream
    try assertEquals(js.limit(3).toList.toString, "[0, 1, 2]")
    finally js.close()
    assert(closed, "the Java stream closed, the stream's rest not released")
  }
