package okay.lifting

import okay.scala3.*, okay.scala3.std.*

/** a `Has` of an abstract row lifts to the row with effects added after it */
class LiftSuite extends munit.FunSuite:
  def step[R[_]: Has[Yield % Int]](x: Int): Unit ! R + State % Int =
    for
      s <- State.get[Int]
      _ <- State.put(s + x)
      _ <- Yield.emit[Int, R + State % Int](s + x)
    yield ()

  def sums[R[_]: Has[Yield % Int]](xs: List[Int]): Unit ! R =
    val each: Unit ! R + State % Int = xs.foldLeft(unit[R + State % Int])((p, x) => p.flatMap(_ => step[R](x)))
    State.run(0)(each).map(_ => ())

  test("an effect of an abstract row is in it with effects added: R + State % Int") {
    assertEquals(run(Yield.collect(sums[Yield % Int](List(1, 2, 3, 4)))), (List(1, 3, 6, 10), ()))
    type App[X] = (Yield % Int + Reader % Int)[X]
    assertEquals(run(Reader.run(0)(Yield.collect(sums[App](List(5, 5))))), (List(5, 10), ()))
  }

  test("two effects added: still found, one further in each") {
    def deep[R[_]: Has[Yield % Int]]: Unit ! R + State % Int + Reader % Int = Yield.emit(1)
    assertEquals(run(Yield.collect(Reader.run(0)(State.run(0)(deep[Yield % Int])))), (List(1), (0, ())))
  }

  test("missing, the error names the whole row, abstract or written out") {
    val lifted = compileErrors("""
      def g[R[_]: Has[Yield % Int]]: Unit ! R + State % Int = Writer.tell("x")""")
    assert(lifted.contains("the effect Writer % String is not in the row R + State % Int"), lifted)
    val known = compileErrors("""
      val k: Unit ! Yield % Int + State % Int = Writer.tell("x")""")
    assert(known.contains("the effect Writer % String is not in the row Yield % Int + State % Int"), known)
  }
