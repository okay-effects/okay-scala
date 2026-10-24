package okay.scala3

import okay.scala3.std.*

/** the README's example, as it is written there */
class ReadmeSuite extends munit.FunSuite:
  def greet[R[_]: Has[Reader % String] : Has[State % Int] : Has[Writer % String]]: Int ! R =
    for
      name <- Reader.ask
      n    <- State.get
      _    <- State.put(n + 1)
      _    <- Writer.tell(s"hello, $name")
    yield n

  type Greeting[X] = (Reader % String + State % Int + Writer % String)[X]

  test("the README's greeting, handled in two orders") {
    assertEquals(run(State.run(12)(Reader.run("Ann")(Writer.run(greet[Greeting])))), (13, (List("hello, Ann"), 12)))
    assertEquals(run(Writer.run(State.run(12)(Reader.run("Ann")(greet[Greeting])))), (List("hello, Ann"), (13, 12)))
  }

  test("the README's missing effect") {
    val error = compileErrors("""
      def tick[R[_]: Has[State % Int]]: Int ! R = State.get
      tick[Reader % String]""")
    assert(error.contains("the effect State % Int is not in the row Reader % String"), error)
  }
