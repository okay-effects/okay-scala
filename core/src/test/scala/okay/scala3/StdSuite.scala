package okay.scala3

import okay.scala3.std.*

/** the standard effects beyond the first six, their words and handlers; resources released */
class StdSuite extends munit.FunSuite:

  test("State.gets: the state through a function") {
    type R[X] = (State % (Int, String))[X]
    assertEquals(run(State.run((1, "a"))(State.gets[(Int, String), String, R](_._2))), ((1, "a"), "a"))
  }

  test("Reader.local: a part of the program sees the environment changed, the rest as it was") {
    type R[X] = (Reader % Int)[X]
    val p: (Int, Int, Int) ! R =
      for
        a <- Reader.ask[Int, R]
        b <- Reader.local[Int, R, Pure, Int](_ * 2)(Reader.ask[Int, R])
        c <- Reader.ask[Int, R]
      yield (a, b, c)
    assertEquals(run(Reader.run(10)(p)), (10, 20, 10))
  }

  test("Writer.listen and censor: what a part wrote, seen and changed, written outside too") {
    type R[X] = (Writer % String)[X]
    val part: Int ! R = Writer.tell[String, R]("a").andThen(Writer.tell[String, R]("b")).as(7)
    val listened = Writer.listen[String, R, Pure, Int](part)
    assertEquals(run(Writer.run(listened)), (List("a", "b"), (List("a", "b"), 7)))
    val censored = Writer.censor[String, R, Pure, Int](_.map(_.toUpperCase))(part)
    assertEquals(run(Writer.run(censored)), (List("A", "B"), 7))
  }

  test("Fail.recover: a value in place of the error") {
    type R[X] = (Fail % String)[X]
    val failing: Int ! R = Fail.raise[String, R]("four").map(_ => 0)
    assertEquals(run(Fail.recover[String, R, Pure, Int](failing)(e => pure(e.length))), 4)
  }

  test("Abort: the value, or None, the rest dropped") {
    val some: Int ! Abort = pure(1)
    val none: Int ! Abort = Abort.exit[Int, Abort].map(_ + 1)
    assertEquals(run(Abort.optional(some)), Some(1))
    assertEquals(run(Abort.optional(none)), None)
  }

  test("Yield.take: the first values, the program's value if it ended by then") {
    type R[X] = (Yield % Int)[X]
    def count(n: Int): Unit ! R = loop(1)(i => if i > n then pure(Right(())) else Yield.emit[Int, R](i).as(Left(i + 1)))
    assertEquals(run(Yield.take(2)(count(5))), (List(1, 2), None))
    assertEquals(run(Yield.take(9)(count(3))), (List(1, 2, 3), Some(())))
  }

  test("Choose: first, guard, once, mark and cut") {
    val square: Int ! Choose =
      for
        x <- Choose.among[Int, Choose]((1 to 10).toList)
        _ <- Choose.guard[Choose](x * x > 30)
      yield x
    assertEquals(run(Choose.first(square)), Some(6))
    assertEquals(run(Choose.all(square)), List(6, 7, 8, 9, 10))
    val once: Int ! Choose =
      for
        x <- Choose.once(Choose.among[Int, Choose](List(1, 2, 3)))
        y <- Choose.among[Int, Choose](List(10, 20))
      yield x + y
    assertEquals(run(Choose.all(once)), List(11, 21))
    val cut: Int ! Choose =
      for
        m <- Choose.mark[Choose]
        x <- Choose.among[Int, Choose](List(1, 2, 3))
        _ <- if x == 2 then Choose.cut[Choose](m) else unit[Choose]
      yield x
    assertEquals(run(Choose.all(cut)), List(1, 2))
    assertEquals(run(Choose.first(Choose.none[Int, Choose])), None)
  }

  test("Fresh: numbers never given before") {
    val p: (Long, Long) ! Fresh = Fresh.next[Fresh].flatMap(a => Fresh.next[Fresh].map(b => (a, b)))
    assertEquals(run(Fresh.from(100)(p)), (100L, 101L))
  }

  test("Random: from a seed, the same numbers each run; bounded") {
    val p: List[Int] ! Random = loop((0, List.empty[Int]))((i, ns) =>
      if i == 5 then pure(Right(ns.reverse)) else Random.int[Random](10).map(n => Left((i + 1, n :: ns))))
    val first = run(Random.seeded(42)(p))
    assertEquals(run(Random.seeded(42)(p)), first)
    assert(first.forall(n => n >= 0 && n < 10), first)
    assertNotEquals(run(Random.seeded(43)(p)), first)
    val d = run(Random.seeded(1)(Random.double[Random]))
    assert(d >= 0 && d < 1, d)
  }

  test("Store: saved, loaded, removed, in memory") {
    type S[X] = Store.Of[String, Int][X]
    val p: (Option[Int], Option[Int]) ! S =
      for
        _ <- Store.save[String, Int, S]("a", 1)
        _ <- Store.save[String, Int, S]("b", 2)
        a <- Store.load[String, Int, S]("a")
        _ <- Store.remove[String, Int, S]("a")
        gone <- Store.load[String, Int, S]("a")
      yield (a, gone)
    assertEquals(run(Store.inMemory(Map.empty[String, Int])(p)), (Map("b" -> 2), (Some(1), None)))
  }

  test("Env: variables and secrets, asked apart, by the handler given") {
    val p: (Option[String], Option[String], Option[String]) ! Env =
      for
        region <- Env.variable[Env]("REGION")
        key <- Env.secret[Env]("API_KEY")
        asVar <- Env.variable[Env]("API_KEY")
      yield (region, key, asVar)
    assertEquals(run(Env.of(Map("REGION" -> "eu"), Map("API_KEY" -> "k"))(p)), (Some("eu"), Some("k"), None))
  }

  test("Clock: virtual time, a day of sleeps in no time") {
    val p: (Long, Long) ! Clock =
      for
        start <- Clock.now[Clock]
        _ <- Clock.sleep[Clock](86400000L)
        end <- Clock.now[Clock]
      yield (start, end)
    assertEquals(run(Clock.virtual(1000)(p)), (86401000L, (1000L, 86401000L)))
  }

  test("Log: collected from a level up, or each to a sink") {
    val p: Int ! Log = Log.debug[Log]("d").andThen(Log.info[Log]("i")).andThen(Log.error[Log]("e")).as(1)
    assertEquals(run(Log.collect(Level.Info)(p)), (List(Level.Info -> "i", Level.Error -> "e"), 1))
    val seen = collection.mutable.ListBuffer.empty[String]
    assertEquals(run(Log.to((level, m) => seen += s"$level $m", Level.Warn)(p)), 1)
    assertEquals(seen.toList, List("Error e"))
  }

  test("Control: shift and reset, k resumed twice") {
    type R[X] = (Pure + Control % (Int ! Pure))[X]
    val p: Int ! R = Control.shift[Int, Int, Pure, R](k => k(1).flatMap(a => k(2).map(b => a + b))).map(_ * 10)
    assertEquals(run(Control.reset[Int, Pure](p)), 30)
  }

  test("Lift: another monad's programs, lowered to it") {
    val sum: Int ! Lifted[Option] = Lift.lift[Option, Int, Lifted[Option]](Some(1))
      .flatMap(a => Lift.lift[Option, Int, Lifted[Option]](Some(2)).map(a + _))
    assertEquals(Lift.lower(sum), Some(3))
    val stopped: Int ! Lifted[Option] = Lift.lift[Option, Int, Lifted[Option]](None).map(_ + 1)
    assertEquals(Lift.lower(stopped), None)
    type E[X] = Either[String, X]
    val failed: Int ! Lifted[E] = Lift.lift[E, Int, Lifted[E]](Left("no"))
    assertEquals(Lift.lower(failed), Left("no"))
  }

  test("observe and around: each operation seen, or changed, then passed on") {
    type R[X] = (Writer % String + State % Int)[X]
    val tick: Int ! R = State.get[Int, R].flatMap(n => State.put[Int, R](n + 1).as(n))
    val seen = observe[State % Int, R, Int](tick)([X] => (op: State[Int, X]) => Writer.tell[String, R](op.toString))
    assertEquals(run(Writer.run(State.run(5)(seen))), (List("Get()", "Put(6)"), (6, 5)))
    val doubled = around[State % Int, R, Int](tick)([X] => (op: State[Int, X], proceed: X ! R) => op match
      case State.Get() => proceed.map(_ * 2)
      case State.Put(_) => proceed)
    assertEquals(run(Writer.run(State.run(5)(doubled))), (Nil, (11, 10)))
  }

  test("ensuring and bracket: released when the program ends, and when a failure drops it") {
    val log = collection.mutable.ListBuffer.empty[String]
    type R[X] = (Fail % String)[X]
    val ok: Int ! R = bracket[String, Int, R](pure("file"))(f => pure(f.length))(f => log += s"close $f")
    assertEquals(run(Fail.run(ok)), Right(4))
    assertEquals(log.toList, List("close file"))
    log.clear()
    val failing: Int ! R = ensuring(ensuring(Fail.raise[String, R]("boom").map(_ => 1))(log += "inner"))(log += "outer")
    assertEquals(run(Fail.run(failing)), Left("boom"))
    assertEquals(log.toList, List("inner", "outer"))
  }

  test("released through the handlers between the failure and its handler") {
    val log = collection.mutable.ListBuffer.empty[String]
    type R[X] = (Fail % String + State % Int)[X]
    val p: Int ! R =
      ensuring(State.put[Int, R](1).andThen(Fail.raise[String, R]("boom")).map(_ => 2))(log += "released")
    assertEquals(run(Fail.run(State.run(0)(p))), Left("boom"))
    assertEquals(log.toList, List("released"))
  }
