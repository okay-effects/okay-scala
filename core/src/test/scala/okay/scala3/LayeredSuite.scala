package okay.scala3

import okay.scala3.std.*

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** other monads in a program: data monads handled in place (`Lift.handle`, a `Distributes`), one control monad
 * lowered last (`Lift.lower`), several brought to one (`Lift.via`) — layered monads */
class LayeredSuite extends munit.FunSuite:
  given ExecutionContext = Async.defaultContext

  type E[A] = Either[String, A]

  // ---- the order of handling

  type Row[X] = (Lifted[Option] + Lifted[List] + Lifted[Future])[X]

  /** each of 1, 2, 3 through a future, then an `Option`: none at `stop` */
  def three(stop: Int): Int ! Row =
    for
      x <- Lift.lift[List, Int, Row](List(1, 2, 3))
      y <- Lift.lift[Future, Int, Row](Future(x * 10))
      z <- Lift.lift[Option, Int, Row](if x == stop then None else Some(y + 1))
    yield z

  test("Option handled first, then List, Future lowered: each way of the list an Option of its own") {
    val f: Future[List[Option[Int]]] = Lift.lower(Lift.handle[List](Lift.handle[Option](three(2))))
    f.map(v => assertEquals(v, List(Some(11), None, Some(31))))
  }

  test("List handled first, then Option: the list inside the Option, a None anywhere ends it all") {
    val stopped: Future[Option[List[Int]]] = Lift.lower(Lift.handle[Option](Lift.handle[List](three(2))))
    val whole: Future[Option[List[Int]]] = Lift.lower(Lift.handle[Option](Lift.handle[List](three(0))))
    stopped.zip(whole).map((s, w) => assertEquals((s, w), (None, Some(List(11, 21, 31)))))
  }

  test("data monads handled in place, the program run with no control monad at all") {
    type R[X] = (Lifted[Try] + Lifted[Vector] + Lifted[E])[X]
    val p: Int ! R =
      for
        a <- Lift.lift[Vector, Int, R](Vector(1, 2))
        b <- Lift.lift[E, Int, R](if a > 5 then Left("big") else Right(a * 2))
        c <- Lift.lift[Try, Int, R](if b == 4 then Failure(IllegalStateException("four")) else Success(b + 1))
      yield c
    val v: E[Vector[Try[Int]]] = run(Lift.handle[E](Lift.handle[Vector](Lift.handle[Try](p))))
    assertEquals(v.map(_.map(_.toOption)), Right(Vector(Some(3), None)))
  }

  // ---- via: one monad's programs as another's

  test("via: an Option as an Either, then lowered") {
    type R[X] = (Lifted[Option] + Lifted[E])[X]
    def p(o: Option[Int]): Int ! R =
      Lift.lift[E, Int, R](Right(1)).flatMap(a => Lift.lift[Option, Int, R](o).map(_ + a))
    val toEither: Option ~> E = [X] => (o: Option[X]) => o.toRight("none")
    assertEquals(Lift.lower(Lift.via[Option, E](toEither)(p(Some(41)))), Right(42))
    assertEquals(Lift.lower(Lift.via[Option, E](toEither)(p(None))), Left("none"))
  }

  test("via: a Try as a Future, then lowered; a failure fails the future") {
    type R[X] = (Lifted[Try] + Lifted[Future])[X]
    def p(t: Try[Int]): Int ! R =
      Lift.lift[Future, Int, R](Future(1)).flatMap(a => Lift.lift[Try, Int, R](t).map(_ + a))
    val toFuture: Try ~> Future = [X] => (t: Try[X]) => Future.fromTry(t)
    val ok = Lift.lower(Lift.via[Try, Future](toFuture)(p(Success(41))))
    val failed = Lift.lower(Lift.via[Try, Future](toFuture)(p(Failure(IllegalStateException("no")))))
    ok.zip(failed.failed).map((v, e) => assertEquals((v, e.getMessage), (42, "no")))
  }

  // ---- the laws of flatMapIn, with effectful continuations, as properties over seeded cases

  type LR[X] = (State % Int + Writer % String)[X]

  def ran[A](p: A ! LR): (List[String], (Int, A)) = run(Writer.run(State.run(1)(p)))

  /** an effectful continuation of effects that DO NOT commute: it reads the state, writes it, tells */
  def noncommuting[G[_]](tag: String, make: (Int, Int) => G[Int]): Int => G[Int] ! LR =
    a => State.get[Int, LR].flatMap(s => State.put[Int, LR](s * 31 + a)
      .andThen(Writer.tell[String, LR](s"$tag$a@$s")).as(make(a, s)))

  /** an effectful continuation of effects that commute: a sum, not read */
  def commuting[G[_]](make: (Int, Int) => G[Int]): Int => G[Int] ! LR =
    a => State.modify[Int, LR](_ + a).as(make(a, 0))

  def laws[G[_]](name: String, gen: scala.util.Random => G[Int], make: (Int, Int) => G[Int], commute: Boolean)(
    using G: Distributes[G]): Unit =
    test(s"$name: left identity, right identity, associativity of flatMapIn, effectful, 300 cases each") {
      val random = scala.util.Random(7)
      for i <- 0 until 300 do
        val a = random.nextInt(100) - 50
        val ga = gen(random)
        val k = noncommuting[G](s"k$i.", make)
        val h = noncommuting[G](s"h$i.", (x, s) => make(x * 7 + 3, s + i))
        assertEquals(ran(G.flatMapIn(G.pure(a))(k)), ran(k(a)), s"left identity, $a")
        assertEquals(ran(G.flatMapIn(ga)(x => pure[G[Int], LR](G.pure(x)))), ran(pure[G[Int], LR](ga)),
          s"right identity, $ga")
        val (k2, h2) = if commute then (commuting[G](make), commuting[G]((x, s) => make(x * 7 + 3, s + i)))
          else (k, h)
        val left = G.flatMapIn(ga)(k2).flatMap(gb => G.flatMapIn(gb)(h2))
        val right = G.flatMapIn(ga)(x => k2(x).flatMap(gb => G.flatMapIn(gb)(h2)))
        assertEquals(ran(left), ran(right), s"associativity, $ga")
    }

  laws[Option]("Option", r => Option.when(r.nextInt(4) > 0)(r.nextInt(100)),
    (a, s) => Option.when((a + s) % 3 != 0)(a + s), commute = false)
  laws[E]("Either", r => if r.nextInt(4) > 0 then Right(r.nextInt(100)) else Left(s"e${r.nextInt(9)}"),
    (a, s) => if (a + s) % 3 != 0 then Right(a - s) else Left(s"at $a"), commute = false)
  laws[Try]("Try", r => if r.nextInt(4) > 0 then Success(r.nextInt(100)) else Failure(Oops(r.nextInt(9))),
    (a, s) => if (a + s) % 3 != 0 then Success(a * 2 + s) else Failure(Oops(a)), commute = false)
  laws[List]("List", r => List.fill(r.nextInt(5))(r.nextInt(100)),
    (a, s) => List.tabulate(math.abs(a + s) % 4)(j => a + s + j), commute = true)
  laws[Vector]("Vector", r => Vector.fill(r.nextInt(5))(r.nextInt(100)),
    (a, s) => Vector.tabulate(math.abs(a + s) % 4)(j => a - s + j), commute = true)

  test("List's associativity under effects that do not commute: breadth first against depth first; handle's is depth") {
    val G = summon[Distributes[List]]
    val k = noncommuting[List]("k", (a, _) => List(a, a + 1))
    val h = noncommuting[List]("h", (b, _) => List(b))
    val breadth = ran(G.flatMapIn(List(1, 2))(k).flatMap(G.flatMapIn(_)(h)))._1
    val depth = ran(G.flatMapIn(List(1, 2))(x => k(x).flatMap(G.flatMapIn(_)(h))))._1
    assertEquals(breadth.map(_.takeWhile(_.isLetter)), List("k", "k", "h", "h", "h", "h"))
    assertEquals(depth.map(_.takeWhile(_.isLetter)), List("k", "h", "h", "k", "h", "h"))
    type R[X] = (State % Int + Writer % String + Lifted[List])[X]
    val p: Int ! R =
      for
        x <- Lift.lift[List, Int, R](List(1, 2))
        _ <- Writer.tell[String, R]("k")
        b <- Lift.lift[List, Int, R](List(x, x + 1))
        _ <- Writer.tell[String, R]("h")
      yield b
    assertEquals(run(Writer.run(State.run(0)(Lift.handle[List](p))))._1, List("k", "h", "h", "k", "h", "h"))
  }

  // ---- with another effect: State

  type SL[X] = (State % Int + Lifted[List])[X]
  /** each element: the state read, the element added */
  val adding: (Int, Int) ! SL =
    for
      x <- Lift.lift[List, Int, SL](List(1, 2, 3))
      s <- State.get[Int, SL]
      _ <- State.put[Int, SL](s + x)
    yield (x, s)

  test("State handled outside List: threaded through the elements in order, each sees the state the one before left") {
    assertEquals(run(State.run(0)(Lift.handle[List](adding))), (6, List((1, 0), (2, 1), (3, 3))))
  }

  test("State handled inside List: each element's way its own state, from the time of the lift") {
    assertEquals(run(Lift.handle[List](State.run(0)(adding))), List((1, (1, 0)), (2, (2, 0)), (3, (3, 0))))
  }

  // ---- constant stack

  test("constant stack: a List of 10 000 elements, a state through them; a Vector of 10 000") {
    type R[X] = (State % Long + Lifted[List])[X]
    val p: Int ! R = Lift.lift[List, Int, R](List.range(0, 10000)).flatMap(x => State.modify[Long, R](_ + x).as(x))
    val (sum, xs) = run(State.run(0L)(Lift.handle[List](p)))
    assertEquals((sum, xs.length, xs.last), (49995000L, 10000, 9999))
    type V[X] = Lifted[Vector][X]
    val v = run(Lift.handle[Vector](Lift.lift[Vector, Int, V](Vector.range(0, 10000)).map(_ * 2)))
    assertEquals((v.length, v.last), (10000, 19998))
  }

  test("constant stack: 100 000 lifts in a chain, of Option, of a one-element List; 100 000 through via") {
    type O[X] = Lifted[Option][X]
    def options(n: Int): Int ! O = loop(0)(i => Lift.lift[Option, Int, O](Some(i)).map(j =>
      if j == n then Right(j) else Left(j + 1)))
    assertEquals(run(Lift.handle[Option](options(100000))), Some(100000))
    type L[X] = Lifted[List][X]
    val lists: Int ! L = loop(0)(i => Lift.lift[List, Int, L](List(i)).map(j =>
      if j == 100000 then Right(j) else Left(j + 1)))
    assertEquals(run(Lift.handle[List](lists)), List(100000))
    type OE[X] = (Lifted[Option] + Lifted[E])[X]
    val viaMany: Int ! OE = loop(0)(i => Lift.lift[Option, Int, OE](Some(i)).map(j =>
      if j == 100000 then Right(j) else Left(j + 1)))
    val toEither: Option ~> E = [X] => (o: Option[X]) => o.toRight("none")
    // handled in place: `lower` of a strict monad nests its runs, one per operation, so it takes stack
    assertEquals(run(Lift.handle[E](Lift.via[Option, E](toEither)(viaMany))), Right(100000))
  }

/** a failure equal by its number, for comparing `Try`s */
final case class Oops(n: Int) extends Exception(s"oops $n")
