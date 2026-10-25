package okay.scala3.std

import okay.scala3.*

import java.util.Spliterators
import java.util.stream.{BaseStream, StreamSupport}

/** JAVA'S STREAMS AND ITERATORS, both ways (the JVM's): a Java stream's values as a stream, pulled one at a time,
 * the Java stream closed when the stream ends, fails, or is left; a stream of no effect as a Java stream, lazy,
 * sequential, closing it releasing the stream's rest */
object JavaStreams:
  /** a Java stream's values — a `Stream`, an `IntStream` (its values boxed), `Files.lines`, a collection's `stream()`
   * — opened at the first pull, closed once: at its end, a value's program failing, or the stream left */
  def from[O, R[_]](js: => BaseStream[O, ?]): Stream[O, R] =
    Stream.bracket[BaseStream[O, ?], O, R](okay.scala3.defer(pure(js)))(_.close())(opened =>
      fromIterator[O, R](opened.iterator()))

  /** a Java iterator's values */
  def fromIterator[O, R[_]](it: => java.util.Iterator[O]): Stream[O, R] =
    Stream.fromIterator(new Iterator[O]:
      private lazy val java = it
      def hasNext: Boolean = java.hasNext
      def next(): O = java.next())

  extension [O](s: Stream[O, Pure])
    /** a stream of no effect as a Java stream: lazy, sequential, each value pulled as the Java stream asks; closed
     * (`close`, try-with-resources), the stream's rest released */
    def toJavaStream: java.util.stream.Stream[O] =
      val it = s.iterator
      StreamSupport.stream(Spliterators.spliteratorUnknownSize(javaOf(it), java.util.Spliterator.ORDERED), false)
        .onClose(() => it.close())

    /** a stream of no effect as a Java iterator */
    def javaIterator: java.util.Iterator[O] = javaOf(s.iterator)

  private def javaOf[O](it: Iterator[O]): java.util.Iterator[O] = new java.util.Iterator[O]:
    def hasNext: Boolean = it.hasNext
    def next(): O = it.next()
