package okay.scala3

/** programs written once, their rows by context bounds */
object Programs:
  def tick[R[_]: Has[State % Int]]: Int ! R =
    for
      n <- State.get
      _ <- State.put(n + 1)
    yield n

  def greet[R[_]: Has[Reader % String] : Has[State % Int] : Has[Writer % String]]: Int ! R =
    for
      name <- Reader.ask
      n <- tick
      _ <- Writer.tell(s"hello, $name")
    yield n + 10

  def divide[R[_]: Has[Fail % String] : Has[State % Int]](a: Int, b: Int): Int ! R =
    for
      _ <- State.put(1)
      q <- if b == 0 then Fail.raise("div by zero") else pure(a / b)
    yield q

  def even[R[_]](n: Int): Boolean ! R = if n == 0 then pure(true) else defer(odd(n - 1))
  def odd[R[_]](n: Int): Boolean ! R = if n == 0 then pure(false) else defer(even(n - 1))

  type Greeting[X] = (Reader % String + State % Int + Writer % String)[X]

class CoreSuite extends munit.FunSuite:
  import Programs.*

  test("a program's effects handled in the row's order") {
    val p = greet[Greeting]
    assertEquals(run(State.run(12)(Reader.run("Ann")(Writer.run(p)))), (13, (List("hello, Ann"), 22)))
  }

  test("handled out of order: a handler takes its effect from any place") {
    val p = greet[Greeting]
    assertEquals(run(Writer.run(State.run(12)(Reader.run("Ann")(p)))), (List("hello, Ann"), (13, 22)))
  }

  test("an effect twice in a row: the nearer handled first") {
    def both[R[_]](using In[State % Int, R]): Int ! R = State.modify[Int, R](_ * 10).andThen(State.get)
    type Twice[X] = (State % Int + State % Int)[X]
    assertEquals(run(State.run(1)(State.run(2)(both[Twice]))), (1, (20, 20)))
  }

  test("a failure: the value or the error, the state kept or dropped by the order") {
    type Div[X] = (State % Int + Fail % String)[X]
    type DivOther[X] = (Fail % String + State % Int)[X]
    assertEquals(run(State.run(0)(Fail.run(divide[Div](10, 2)))), (1, Right(5)))
    assertEquals(run(State.run(0)(Fail.run(divide[Div](10, 0)))), (1, Left("div by zero")))
    assertEquals(run(Fail.run(State.run(0)(divide[DivOther](10, 0)))), Left("div by zero"))
  }

  test("multi-shot k: a state outside the choice threads through the branches, inside each has its own") {
    def chosen[R[_]: Has[Choose] : Has[State % Int]]: Int ! R =
      for
        x <- Choose.among(List(1, 2, 3))
        _ <- State.modify[Int, R](_ + x)
        s <- State.get[Int, R]
      yield s * 10
    type Outer[X] = (State % Int + Choose)[X]
    type Inner[X] = (Choose + State % Int)[X]
    assertEquals(run(State.run(0)(Choose.all(chosen[Outer]))), (6, List(10, 30, 60)))
    assertEquals(run(Choose.all(State.run(0)(chosen[Inner]))), List((1, 10), (2, 20), (3, 30)))
  }

  test("choices in choices, and none") {
    def triples[R[_]: Has[Choose]](n: Int): (Int, Int, Int) ! R =
      for
        a <- Choose.among((1 to n).toList)
        b <- Choose.among((a to n).toList)
        c <- Choose.among((b to n).toList)
        t <- if a * a + b * b == c * c then pure((a, b, c)) else Choose.none
      yield t
    assertEquals(run(Choose.all(triples[Choose](15))), List((3, 4, 5), (5, 12, 13), (6, 8, 10), (9, 12, 15)))
  }

  test("a generator: collected, and each value to a consumer with effects of its own") {
    def count[R[_]: Has[Yield % Int]](n: Int): Unit ! R =
      loop(1)(i => if i > n then pure(Right(())) else Yield.emit(i).as(Left(i + 1)))
    assertEquals(run(Yield.collect(count[Yield % Int](3))), (List(1, 2, 3), ()))
    type Sum[X] = (State % Int + Yield % Int)[X]
    val summed = Yield.foreach(count[Sum](100))(i => State.modify[Int, State % Int](_ + i))
    assertEquals(run(State.run(0)(summed)), (5050, ()))
  }

  test("intercepted: each operation observed, changed or refused, then passed on to its handler outside") {
    type Counting[X] = (Writer % String + State % Int)[X]
    def logged[A](p: A ! Counting): A ! Counting =
      okay.scala3.intercept(p, Handler.General[State % Int, Counting, A, A](pure, [X] => (op: State[Int, X], k: X => A ! Counting) =>
        op match
          case State.Get() => Writer.tell[String, Counting]("get").andThen(State.get[Int, Counting]).flatMap(k)
          case State.Put(n) => Writer.tell[String, Counting](s"put $n").andThen(State.put[Int, Counting](n * 2)).flatMap(k)))
    val p = logged(tick[Counting].andThen(tick[Counting]))
    assertEquals(run(Writer.run(State.run(5)(p))), (List("get", "put 6", "get", "put 13"), (26, 12)))
  }

  test("a program in a bigger row: into") {
    val small: Int ! State % Int = tick
    type Big[X] = (Reader % String + State % Int + Writer % String)[X]
    val big: Int ! Big = small.into[Big].flatMap(n => Reader.ask[String, Big].map(s => n + s.length))
    assertEquals(run(Writer.run(Reader.run("abc")(State.run(1)(big)))), (Nil, (2, 4)))
    val none: Int ! Pure = pure(7)
    assertEquals(run(State.run(0)(none.into[State % Int])), (0, 7))
  }

  test("split: an effect taken to the end of the row") {
    type Row[X] = (State % Int + Reader % String)[X]
    val p: Int ! Row = Reader.ask[String, Row].flatMap(s => State.get[Int, Row].map(_ + s.length))
    val moved: Int ! (Pure + Reader % String + State % Int) = split[State % Int, Row, Pure + Reader % String, Int](p)
    assertEquals(run(State.run(1)(Reader.run("ab")(moved))), (1, 3))
  }

  test("a million binds, left and right, in constant stack") {
    val right = loop(0)(i => pure[Either[Int, Int], Pure](if i == 1000000 then Right(i) else Left(i + 1)))
    assertEquals(run(right), 1000000)
    val left = (1 to 1000000).foldLeft(pure[Int, Pure](0))((p, _) => p.map(_ + 1))
    assertEquals(run(left), 1000000)
    val nested = (1 to 1000000).foldLeft(pure[Int, Pure](0))((p, _) => p.flatMap(n => pure(n + 1)))
    assertEquals(run(nested), 1000000)
  }

  test("a million operations, tail calls and resumptions in constant stack") {
    def count[R[_]: Has[State % Int]](n: Int): Unit ! R =
      loop(n)(i => if i == 0 then pure(Right(())) else State.modify[Int, R](_ + 1).as(Left(i - 1)))
    assertEquals(run(State.run(0)(count[State % Int](1000000))), (1000000, ()))
    assertEquals(run(even[Pure](1000001)), false)
    def resumed[R[_]: Has[Yield % Int]](n: Int): Unit ! R =
      loop(n)(i => if i == 0 then pure(Right(())) else Yield.emit(i).as(Left(i - 1)))
    type Row[X] = (State % Int + Yield % Int)[X]
    val p = Yield.foreach(resumed[Row](1000000))(i => State.modify[Int, State % Int](_ + 1))
    assertEquals(run(State.run(0)(p)), (1000000, ()))
  }

  test("ten thousand handlers deep") {
    // the innermost asks, through ten thousand writers, the reader outside them all
    def deep(n: Int): Int ! Reader % Int =
      if n == 0 then Reader.ask
      else Writer.run[String, Reader % Int + Writer % String, Reader % Int, Int](
        defer(deep(n - 1)).into[Reader % Int + Writer % String]).map(_._2 + 1) // built as the run gets to it
    assertEquals(run(Reader.run(5)(deep(10000))), 10005)
  }

  test("an effect missing from the row does not compile, and says which") {
    val error = compileErrors("Programs.tick[Reader % String]")
    assert(error.contains("the effect State % Int is not in the row Reader % String"), error)
    val twice = compileErrors("State.run(0)(Programs.greet[Reader % String + State % Int])")
    assert(twice.contains("the effect Writer % String is not in the row Reader % String + State % Int"), twice)
  }
