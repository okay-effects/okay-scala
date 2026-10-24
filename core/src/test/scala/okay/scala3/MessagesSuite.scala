package okay.scala3

/** the compile errors of rows, as they read */
class MessagesSuite extends munit.FunSuite:
  test("a handler whose rest is not the row without its effect") {
    val error = compileErrors("""
      type R[X] = (State % Int + Async)[X]
      def p: Int ! R = State.get[Int, R]
      State.run[Int, R, Async, Int](1)(p)""")
    assert(error.contains("the row State % Int + Async without the effect State % Int is not Async"), error)
  }

  test("a missing effect in a row of three") {
    val error = compileErrors("""
      def tick[R[_]: Has[State % Int]]: Int ! R = State.get
      tick[Reader % String + Writer % String + Async]""")
    assert(error.contains("the effect State % Int is not in the row Reader % String + Writer % String + Async"), error)
  }
