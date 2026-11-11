package okay.scala3

import okay.scala3.std.*

/** WHAT A LOOP MUST NOT GATHER: the levels and frames its continuations hold, counted at two lengths — whatever the
 * clock says (`Loops`, `Depth`; by the clock, `LevelsProbe`) */
class LevelsSuite extends munit.FunSuite:
  import Depth.probed
  import Loops.*

  test("calls in tail position between two orders of one row keep one level, however many: ping and pong") {
    // the handler's level, and the one that renames — for every call
    assertEquals((pingPong(1000), pingPong(2000)), ((1000, (2, 2)), (2000, (2, 2))))
    assertEquals(pingPong(1_000_000), (1_000_000, (2, 2)))
  }

  test("calls in tail position round three rows, and into the same row, keep one level, however many") {
    assertEquals((round(1000), round(2000)), ((1000, (2, 2)), (2000, (2, 2))))
    assertEquals(round(1_000_000), (1_000_000, (2, 2)))
    assertEquals((sameRow(1000), sameRow(2000)), ((1000, (2, 2)), (2000, (2, 2))))
  }

  test("calls in tail position between two orders of one row, a state's handler inside: nothing passes it") {
    /** the state's handler inside the one that counts: a level of every continuation */
    def inside(n: Int): (Int, (Int, Int)) = run(probed(State.run(0)(ping(n)))) match
      case ((s, _), most) => (s, most)
    assertEquals((inside(1000), inside(2000)), ((1000, (3, 2)), (2000, (3, 2))))
    // a handler entered between two calls: its level stays between their two, and each renames its own row
    type Read[X] = (Probe + Reader % Int + State % Int)[X]
    def read(n: Int): Unit ! Read = Reader.ask[Int, Read].flatMap(_ => tick[Read]).flatMap(_ => pong(n - 1).into[Read])
    def answered(n: Int): Unit ! Back = Reader.run(10)(read(n))
    def under(n: Int): Unit ! Forth = tick[Forth].flatMap(_ => answered(n - 1).into[Forth])
    def outside(n: Int): (Int, (Int, Int)) = run(State.run(0)(probed(under(n)))) match
      case (s, (_, most)) => (s, most)
    assertEquals((outside(1000), outside(2000)), ((1000, (4, 2)), (2000, (4, 2))))
  }

  test("a continuation taken in the middle of calls from row to row is resumed twice: each path goes on") {
    type Chosen[X] = (State % Int + Choose)[X]
    type Swapped[X] = (Choose + State % Int)[X]
    def add[R[_]](n: Int)(using In[Choose, R], In[State % Int, R]): Unit ! R =
      if n == 500 then Choose.among[Int, R](List(10, 20)).flatMap(x => State.modify[Int, R](_ + x))
      else State.modify[Int, R](_ + 1)
    def flip(n: Int): Int ! Chosen =
      if n == 0 then State.get[Int, Chosen] else add[Chosen](n).flatMap(_ => flop(n - 1).into[Chosen])
    def flop(n: Int): Int ! Swapped =
      if n == 0 then State.get[Int, Swapped] else add[Swapped](n).flatMap(_ => flip(n - 1).into[Swapped])
    // each path its own state; or one state through both: the second goes on from where the first ended
    assertEquals(run(Choose.all(State.run(0)(flip(1000)))), List((1009, 1009), (1019, 1019)))
    assertEquals(run(State.run(0)(Choose.all(flip(1000)))), (1528, List(1009, 1528)))
    assertEquals(run(Choose.all(State.run(0)(flop(999)))), List((1008, 1008), (1018, 1018)))
    assertEquals(run(State.run(0)(Choose.all(flop(1001)))), (1529, List(1010, 1529)))
  }

  test("ensuring in calls from row to row: every finalizer runs once, in order; at a failure too") {
    type Failed[X] = (Probe + Fail % String + State % Int)[X]
    type Turned[X] = (State % Int + Fail % String + Probe)[X]
    var log = List.empty[Int]
    def said[R[_]](n: Int): Unit ! R = defer(pure { log ::= n })
    def here(n: Int): Unit ! Failed =
      if n == 0 then Fail.raise[String, Failed]("end")
      else ensuring(tick[Failed])(said(n)).flatMap(_ => there(n - 1).into[Failed])
    def there(n: Int): Unit ! Turned =
      if n == 0 then Fail.raise[String, Turned]("end")
      else ensuring(tick[Turned])(said(n)).flatMap(_ => here(n - 1).into[Turned])
    def ran(p: Unit ! Failed): ((Int, Either[String, (Unit, (Int, Int))]), List[Int]) =
      log = Nil
      (run(State.run(0)(Fail.run(probed(p)))), log.reverse)
    assertEquals(ran(here(1000)), ((1000, Left("end")), (1000 to 1 by -1).toList))
    assertEquals(ran(here(2000)), ((2000, Left("end")), (2000 to 1 by -1).toList))
    // the call itself under a finalizer: that one waits, a frame for every call, and they run the last first
    def kept(n: Int): Unit ! Failed =
      if n == 0 then Fail.raise[String, Failed]("end") else ensuring(lost(n - 1).into[Failed])(said(n))
    def lost(n: Int): Unit ! Turned =
      if n == 0 then Fail.raise[String, Turned]("end") else ensuring(kept(n - 1).into[Turned])(said(n))
    assertEquals(ran(kept(1000)), ((0, Left("end")), (1 to 1000).toList))
  }

  test("a program put into a row, and that into another, with nothing between: one renaming, of the two") {
    type Counter[X] = (State % Int)[X]
    type Twice[X] = (State % Int + State % Int)[X]
    // two states of one type in a row: one changed and read through each evidence
    def both[R[_]](using inner: In[State % Int, R], outer: In[State % Int, R]): (Int, Int) ! R =
      for
        _ <- State.modify[Int, R](_ + 1)(using inner)
        _ <- State.modify[Int, R](_ * 10)(using outer)
        a <- State.get[Int, R](using inner)
        b <- State.get[Int, R](using outer)
      yield (a, b)
    val last = In[State % Int, Twice](Place.Last())
    val first = In[State % Int, Twice](Place.Init(Place.Only()))
    val two: (Int, Int) ! Twice = both[Twice](using last, first)
    assertEquals(run(State.run(0)(State.run(0)(two))), (0, (1, (1, 0))))
    // two states into one, and that into two again: both are the last one now
    val one: (Int, Int) ! Twice = two.into[Counter].into[Twice]
    assertEquals(run(State.run(0)(State.run(0)(one))), (0, (10, (10, 10))))
  }

  test("a renaming of no shape the machine knows, before or after one it knows: kept as two, the places right") {
    // `split` takes the state to the end of the row by a renaming of its own
    val turned: Unit ! Back = split[State % Int, Forth, Probe, Unit](ping(1000))
    assertEquals(run(State.run(0)(probed(turned).map(_._2))), (1000, (2, 2)))
    val back: Unit ! Forth = split[State % Int, Forth, Probe, Unit](ping(1000)).into[Forth]
    assertEquals(run(State.run(0)(probed(back).map(_._2))), (1000, (2, 2)))
    // an interceptor in the row the calls come back to: it sees every state operation of both rows
    var seen = 0
    val counted = observe[State % Int, Forth, Unit](ping(1000))([X] => (_: State[Int, X]) => defer(pure { seen += 1 }))
    assertEquals(run(State.run(0)(probed(counted).map(_._2)))._1, 1000)
    assertEquals(seen, 2000)
  }

  test("a loop that binds programs of a smaller row: its continuations hold no more levels, however long it is") {
    // the embedding the probe is in, and the handler: an embedding that has ended has left nothing
    assertEquals((boundLoop(1000), boundLoop(2000)), ((2, 2), (2, 2)))
  }

  test("a handler that resumes in a loop, a generator looped over, cut, pulled: no more levels or frames") {
    assertEquals((resumed(1000), resumed(2000)), ((1, 1), (1, 1)))
    assertEquals((foreach(1000), foreach(2000)), ((500500, (2, 2)), (2001000, (2, 2))))
    assertEquals((taken(1000), taken(2000)), ((1000, (2, 3)), (2000, (2, 3))))
    assertEquals((pulled(1000), pulled(2000)), ((500500, (2, 4)), (2001000, (2, 4))))
    assertEquals((pulledMapped(1000)._2, pulledMapped(2000)._2), ((2, 9), (2, 9)))
  }

  test("a program lowered to a monad whose values are held now: a million of them lifted, in constant stack") {
    assertEquals(Lift.lower(lifts(1_000_000)), Some(1_000_000))
    type E[X] = Lifted[[A] =>> Either[String, A]][X]
    def rights(n: Int, sum: Int = 0): Int ! E =
      if n == 0 then pure(sum)
      else Lift.lift[[A] =>> Either[String, A], Int, E](Right(1)).flatMap(x => rights(n - 1, sum + x))
    assertEquals(Lift.lower(rights(1_000_000)), Right(1_000_000))
    // one that holds none ends the program there, as its bind does
    val none: Int ! Lifted[Option] = lifts(1000).flatMap(_ => Lift.lift[Option, Int, Lifted[Option]](None))
    assertEquals(Lift.lower(none), None)
  }

  test("a program driven as `Async` drives it, resumed wait after wait: its rest holds one level, however long") {
    assertEquals((driven(waitsInTail(1000)), driven(waitsInTail(2000))), ((1000, (1, 1)), (2000, (1, 1))))
    // what waits for each answer is the program's own: a frame a wait, and still one level
    assertEquals((driven(waits(1000)), driven(waits(2000))), ((1000, (1, 1000)), (2000, (1, 2000))))
  }
