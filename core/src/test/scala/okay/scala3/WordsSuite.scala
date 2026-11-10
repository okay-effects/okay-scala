package okay.scala3

import okay.scala3.std.*

/** a word says no row: the program it is used in does; a handler leaves the row as one would write it */
class WordsSuite extends munit.FunSuite:
  type App[X] = (State % Int + Choose)[X]
  type Three[X] = (Reader % Int + State % Int + Writer % String)[X]

  test("a handler leaves the natural row: of `A + B` without `A`, `B`; and `Pure + B` where that is asked") {
    val p: Int ! App = State.put[Int, App](1).flatMap(_ => Choose.among[Int, App](List(1, 2)))
    val chosen: (Int, Int) ! Choose = State.run(0)(p)
    assertEquals(run(Choose.all(chosen)), List((1, 1), (1, 2)))
    // as before: the row as it is written, the effect taken out
    val written: (Int, Int) ! (Pure + Choose) = State.run(0)(p)
    assertEquals(run(Choose.all(written)), List((1, 1), (1, 2)))
    assertEquals(run(Choose.all(State.run[Int, App, Pure + Choose, Int](0)(p))), List((1, 1), (1, 2)))
    // of three, the first taken, or the one between; the handlers in any order
    val q: Int ! Three =
      for
        r <- Reader.ask[Int, Three]
        s <- State.get[Int, Three]
        _ <- Writer.tell[String, Three](s"$r $s")
        _ <- State.put[Int, Three](s + r)
      yield r * s
    val asked: Int ! (State % Int + Writer % String) = Reader.run(2)(q)
    val told: (List[String], Int) ! (State % Int) = Writer.run(asked)
    assertEquals(run(State.run(3)(told)), (5, (List("2 3"), 6)))
    val between: (Int, Int) ! (Reader % Int + Writer % String) = State.run(3)(q)
    val last: (Int, Int) ! (Writer % String) = Reader.run(2)(between)
    assertEquals(run(Writer.run(last)), (List("2 3"), (5, 6)))
    // an effect twice in a row is taken at its last place, as ever
    type Twice[X] = (State % Int + State % Int)[X]
    val twice: Int ! Twice = State.put[Int, Twice](7).flatMap(_ => State.get[Int, Twice])
    assertEquals(run(State.run(1)(State.run(2)(twice))), (1, (7, 7)))
  }
