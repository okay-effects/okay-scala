package okay.scala3.std

import okay.scala3.*

import java.util.Spliterators
import java.util.stream.{BaseStream, StreamSupport}

/** JAVA'S STREAMS AND ITERATORS, both ways (the JVM's): a Java stream's values as a stream, pulled one at a time,
 * the Java stream closed when it ends; a stream of no effect as a Java stream, lazy, sequential */
object JavaStreams:
  /** a Java stream's values — a `Stream`, an `IntStream` (its values boxed), `Files.lines`, a collection's `stream()`
   * — made at the first pull, closed at its end. Stopped early, it is not closed: `Scope.defer(js.close())` for that */
  def from[O, R[_]](js: => BaseStream[O, ?]): Stream[O, R] = Stream(defer {
    val opened = js
    val it = opened.iterator()
    Stream.fromIterator[O, R](new Iterator[O]:
      def hasNext: Boolean =
        val more = it.hasNext
        if !more then opened.close()
        more
      def next(): O = it.next()
    ).pull
  })

  /** a Java iterator's values */
  def fromIterator[O, R[_]](it: => java.util.Iterator[O]): Stream[O, R] =
    Stream.fromIterator(new Iterator[O]:
      private lazy val java = it
      def hasNext: Boolean = java.hasNext
      def next(): O = java.next())

  extension [O](s: Stream[O, Pure])
    /** a stream of no effect as a Java stream: lazy, sequential, each value pulled as the Java stream asks */
    def toJavaStream: java.util.stream.Stream[O] =
      StreamSupport.stream(Spliterators.spliteratorUnknownSize(s.javaIterator, java.util.Spliterator.ORDERED), false)

    /** a stream of no effect as a Java iterator */
    def javaIterator: java.util.Iterator[O] =
      val it = s.iterator
      new java.util.Iterator[O]:
        def hasNext: Boolean = it.hasNext
        def next(): O = it.next()
