package okay.scala3

import okay.scala3.std.*

/** LEAVING: `ensuring` and `unwind` — the rules of a program left, each a test: with a value, or unwound by a handler
 * that does not go on with the rest */
class EnsuringSuite extends munit.FunSuite:
  var log = List.empty[String]

  def said[R[_]](what: String): Unit ! R = defer(pure { log ::= what })

  /** a result, and what was said on the way, in order */
  def logged[A](result: => A): (A, List[String]) =
    log = Nil
    val a = result
    (a, log.reverse)

  type Failing[X] = (State % Int + Fail % String)[X]
  type Counter[X] = (State % Int)[X]
  type F[X] = (Fail % String)[X]

  def tick[R[_]](using In[State % Int, R]): Unit ! R = State.modify[Int, R](_ + 1)

  test("ensuring, a value: the finalizer runs after the program, and the program's value is the result") {
    val p: Int ! Pure = ensuring(said[Pure]("body").map(_ => 1))(said[Pure]("closed").map(_ => 2))
    assertEquals(logged(run(p)), (1, List("body", "closed")))
    // a finalizer is made when it runs: building the program runs nothing
    log = Nil
    val built: Int ! Pure = ensuring(pure[Int, Pure](1))(pure[Unit, Pure] { log ::= "closed" })
    assertEquals(log, Nil)
    assertEquals(logged(run(built)), (1, List("closed")))
  }

  test("ensuring, a thunk: a finalizer that is no program runs as one that asks nothing; its value is dropped") {
    val buffer = collection.mutable.ListBuffer.empty[String]
    val p: Int ! F = ensuring(ensuring(Fail.raise[String, F]("no").map(_ => 1))(buffer += "inner"))(log ::= "outer")
    assertEquals(logged(run(Fail.run(p))), (Left("no"), List("outer")))
    assertEquals(buffer.toList, List("inner"))
    val used: Int ! F = bracket[String, Int, F](pure("file"))(f => pure(f.length))(f => buffer += s"close $f")
    assertEquals(run(Fail.run(used)), Right(4))
    assertEquals(buffer.toList, List("inner", "close file"))
  }

  test("ensuring, a failure: the finalizer runs before the failure is answered") {
    val p: Int ! F = ensuring(said[F]("body").flatMap(_ => Fail.raise[String, F]("no")))(said("closed"))
    val rescued = Fail.recover[String, F, Pure, Int](p)(e => said[Pure](s"rescued $e").map(_ => 0))
    assertEquals(logged(run(rescued)), (0, List("body", "closed", "rescued no")))
    assertEquals(logged(run(Fail.run(p))), (Left("no"), List("body", "closed")))
  }

  test("ensuring, a handler's abort: any handler made by `Handler.abort` unwinds what it leaves") {
    val p: Int ! Account = ensuring(Account.deposit(1).into[Account])(said("closed"))
    val refused = handle(p, Handler.abort[Account, Pure, Int, Int](pure)([X] => (_: Account[X]) => pure(-1)))
    assertEquals(logged(run(refused)), (-1, List("closed")))
  }

  test("ensuring, the first of the choices: a path with no value is unwound, a path not begun holds nothing") {
    def path(x: Int): Int ! Choose =
      ensuring(said[Choose](s"open $x").flatMap(_ => if x == 1 then Choose.none[Int, Choose] else pure(x)))(
        said(s"closed $x"))
    val p: Int ! Choose = Choose.among[Int, Choose](List(1, 2, 3)).flatMap(path)
    assertEquals(logged(run(Choose.first(p))), (Some(2), List("open 1", "closed 1", "open 2", "closed 2")))
  }

  test("ensuring, a generator cut short: the producer is unwound where it stopped") {
    type Y[X] = (Yield % Int)[X]
    def from(n: Int): Nothing ! Y = Yield.emit[Int, Y](n).flatMap(_ => from(n + 1))
    val cut = Yield.take[Int, Y, Pure, Int](2)(ensuring(from(0))(said("closed")))
    assertEquals(logged(run(cut)), ((List(0, 1), None), List("closed")))
    // what a finalizer emits when the generator is cut nobody takes: the cut has left, and the finalizer ends there
    val last = ensuring(from(0))(said[Y]("closing").flatMap(_ => Yield.emit[Int, Y](-1)).flatMap(_ => said("not run")))
    assertEquals(logged(run(Yield.take[Int, Y, Pure, Int](2)(last))), ((List(0, 1), None), List("closing")))
    // a generator that ends by then was not cut: its finalizer ran at its end
    val three = ensuring(Yield.emit[Int, Y](1).flatMap(_ => Yield.emit[Int, Y](2)).map(_ => 3))(said("closed"))
    assertEquals(logged(run(Yield.take[Int, Y, Pure, Int](2)(three))), ((List(1, 2), Some(3)), List("closed")))
  }

  test("ensuring, every path of a choice: a finalizer after the choice runs once in each path") {
    def path(x: Int): Int ! Choose =
      ensuring(if x == 2 then Choose.none[Int, Choose] else pure[Int, Choose](x * 10))(said(s"closed $x"))
    val p: Int ! Choose = Choose.among[Int, Choose](List(1, 2, 3)).flatMap(path)
    assertEquals(logged(run(Choose.all(p))), (List(10, 30), List("closed 1", "closed 2", "closed 3")))
  }

  test("ensuring, every path of a choice: a finalizer before the choice runs once for each path that passes it") {
    val chosen: Int ! Choose = Choose.among[Int, Choose](List(1, 2, 3)).flatMap(x => said[Choose](s"path $x").map(_ => x))
    val p: Int ! Choose =
      ensuring(chosen)(said("closed")).flatMap(x => if x == 2 then Choose.none[Int, Choose] else pure(x))
    val told = List("path 1", "closed", "path 2", "closed", "path 3", "closed")
    assertEquals(logged(run(Choose.all(p))), (List(1, 3), told))
  }

  test("ensuring, a cut: the alternatives it drops were not begun, and hold nothing") {
    val before: Int ! Choose = ensuring(Choose.among[Int, Choose](List(1, 2, 3)))(said("closed"))
    val p: Int ! Choose = Choose.once(before.flatMap(x => said[Choose](s"path $x").map(_ => x)))
    assertEquals(logged(run(Choose.all(p))), (List(1), List("closed", "path 1")))
  }

  test("ensuring, nested: the innermost finalizer first, at a value and at a failure") {
    val ended: Int ! F = ensuring(ensuring(pure[Int, F](1))(said("inner")))(said("outer"))
    assertEquals(logged(run(Fail.run(ended))), (Right(1), List("inner", "outer")))
    val raised: Int ! F = Fail.raise[String, F]("no")
    val failed = ensuring(ensuring(raised)(said("inner")).flatMap(_ => said[F]("not run").map(_ => 2)))(said("outer"))
    assertEquals(logged(run(Fail.run(failed))), (Left("no"), List("inner", "outer")))
  }

  test("ensuring, state: a finalizer reads and writes the state as its path left it, inside the handler or outside") {
    val saw: Unit ! Failing =
      State.get[Int, Failing].flatMap(s => said[Failing](s"saw $s").flatMap(_ => State.put[Int, Failing](s + 1)))
    val failing: Int ! Failing = State.put[Int, Failing](5).flatMap(_ => Fail.raise[String, Failing]("no"))
    val p = ensuring(failing)(saw)
    // the state inside the handler that leaves: it is in what is unwound, and is dropped after the finalizer
    assertEquals(logged(run(Fail.run(State.run(0)(p)))), (Left("no"), List("saw 5")))
    // the state outside it: what the finalizer wrote stays
    assertEquals(logged(run(State.run(0)(Fail.run(p)))), ((6, Left("no")), List("saw 5")))
    // each path of a choice has the state it left
    type Paths[X] = (Choose + State % Int)[X]
    val seen: Unit ! Paths = State.get[Int, Paths].flatMap(s => said(s"saw $s"))
    def ended(x: Int): Int ! Paths = State.put[Int, Paths](x).flatMap(_ => Choose.none[Int, Paths])
    val q: Int ! Paths = Choose.among[Int, Paths](List(1, 2)).flatMap(x => ensuring(ended(x))(seen))
    assertEquals(logged(run(Choose.all(State.run(0)(q)))), (Nil, List("saw 1", "saw 2")))
  }

  test("ensuring, a finalizer that fails at a value: the failure is the result, and what waits outside is unwound") {
    val one: Int ! F = pure(1)
    val p = ensuring(ensuring(one)(Fail.raise[String, F]("closing")))(said("outer"))
    assertEquals(logged(run(Fail.run(p))), (Left("closing"), List("outer")))
  }

  test("ensuring, a finalizer that fails in an unwinding: the unwinding goes on, the failure goes to its handler") {
    type Two[X] = (Fail % Int + Fail % String)[X]
    val failing: Int ! Two = Fail.raise[String, Two]("no")
    val closing: Unit ! Two = Fail.raise[Int, Two](7).flatMap(_ => said("not run"))
    val p = ensuring(ensuring(failing)(closing))(said("outer"))
    // its handler is outside the one that leaves: it unwinds the rest, and its answer is the result
    assertEquals(logged(run(Fail.run(Fail.run(p)))), (Left(7), List("outer")))
    // its handler is inside what is unwound: the failure is answered there, and the unwinding goes on from there
    type One[X] = (Fail % String)[X]
    val inside: Unit ! One =
      Fail.recover[Int, Two, One, Int](p)(n => said[One](s"rescued $n").map(_ => 0)).flatMap(_ => said("not run"))
    assertEquals(logged(run(Fail.run(inside))), (Left("no"), List("outer", "rescued 7")))
    // its handler is the one that leaves: nobody answers, the finalizer ends there, the first failure stands
    val again: Unit ! F = Fail.raise[String, F]("closing").flatMap(_ => said("not run"))
    val own = ensuring(ensuring(Fail.raise[String, F]("no"): Int ! F)(again))(said("outer"))
    assertEquals(logged(run(Fail.run(own))), (Left("no"), List("outer")))
  }

  test("ensuring, `k` is a value: unwound twice the finalizers run twice; resumed after, its path ends once more") {
    val p: Int ! Account =
      ensuring(Account.deposit(1).into[Account].flatMap(n => said[Account](s"got $n").map(_ => n)))(said("closed"))
    val handler = Handler.control[Account, Pure, Int, Int](pure)(
      [X] => (op: Account[X], k: Continuation[X, Pure, Int]) => op match
        case Account.Deposit(n) => k.unwind.flatMap(_ => k.unwind).flatMap(_ => k(n + 1)))
    assertEquals(logged(run(handle(p, handler))), (2, List("closed", "closed", "got 2", "closed")))
  }

  test("ensuring, the rest of an unwinding kept by a handler inside it: resumed where the unwinding is not, refused") {
    type Kept[X] = (Fail % String + Account)[X]
    val failing: Int ! Kept = Fail.raise[String, Kept]("no")
    val p = ensuring(failing)(Account.deposit(1).into[Kept].flatMap(_ => said("closed")))
    var kept = Option.empty[Int ! F]
    val keeper = Handler.general[Account, F, Int, Int](pure)(
      [X] => (op: Account[X], k: X => Int ! F) => op match
        case Account.Deposit(n) =>
          kept = Some(k(n))
          pure[Int, F](0))
    // the finalizer waits at its operation; the unwinding goes on without it
    val left: Int ! F = handle(p, keeper)
    assertEquals(logged(run(Fail.run(left))), (Left("no"), Nil))
    // resumed, the finalizer ends — and there is no unwinding to go on with, and no value
    intercept[NoUnwinding](run(Fail.run(kept.get)))
    assertEquals(log, List("closed"))
  }

  test("ensuring, `k` neither resumed nor unwound: nothing of it runs") {
    val p: Int ! Account = ensuring(Account.deposit(1).into[Account])(said("closed"))
    val kept = Handler.general[Account, Pure, Int, Int](pure)([X] => (_: Account[X], _: X => Int ! Pure) => pure(0))
    assertEquals(logged(run(handle(p, kept))), (0, Nil))
  }

  test("ensuring, a stream pulled and not read to its end: no finalizer runs; read to its end, it runs") {
    type Y[X] = (Yield % Int)[X]
    def from(n: Int): Unit ! Y = Yield.emit[Int, Y](n).flatMap(_ => from(n + 1))
    def three: Unit ! Y =
      ensuring(Yield.emit[Int, Y](1).flatMap(_ => Yield.emit[Int, Y](2)).flatMap(_ => Yield.emit[Int, Y](3)))(
        said("closed"))
    def pulled(p: Unit ! Y): Stream[Int, Pure] = Stream.generated[Int, Y, Pure](p)
    // the stream keeps the rest of the producer, and nobody leaves it
    assertEquals(logged(pulled(ensuring(from(0))(said("closed"))).toLazyList.take(3).toList), (List(0, 1, 2), Nil))
    assertEquals(logged(pulled(three).toLazyList.take(2).toList), (List(1, 2), Nil))
    assertEquals(logged(run(pulled(three).toList)), (List(1, 2, 3), List("closed")))
  }

  test("bracket: acquired, used, released — when the use ends and when it fails") {
    def use(fails: Boolean): Int ! F =
      bracket(said[F]("open").map(_ => "file"))(f => said[F](s"use $f").flatMap(_ =>
        if fails then Fail.raise[String, F]("no") else pure(f.length)))(f => said(s"close $f"))
    assertEquals(logged(run(Fail.run(use(false)))), (Right(4), List("open", "use file", "close file")))
    assertEquals(logged(run(Fail.run(use(true)))), (Left("no"), List("open", "use file", "close file")))
  }

  test("ensuring, constant stack: a million finalizers in a loop") {
    def loop(n: Int): Int ! Counter =
      if n == 0 then State.get[Int, Counter]
      else ensuring(tick[Counter])(tick[Counter]).flatMap(_ => loop(n - 1))
    assertEquals(run(State.run(0)(loop(1_000_000))), (2_000_000, 2_000_000))
  }

  test("ensuring, constant stack: a continuation with a hundred thousand waiting finalizers is unwound") {
    val raised: Int ! Failing = Fail.raise[String, Failing]("no")
    val p = (1 to 100_000).foldLeft(raised)((p, _) => ensuring(p)(tick[Failing]))
    assertEquals(run(State.run(0)(Fail.run(p))), (100_000, Left("no")))
    // and of thunks
    var closed = 0
    val q = (1 to 100_000).foldLeft(raised)((p, _) => ensuring(p)(closed += 1))
    assertEquals(run(State.run(0)(Fail.run(q))), (0, Left("no")))
    assertEquals(closed, 100_000)
  }

  test("ensuring, an interceptor between the finalizer and the handler that leaves sees what the finalizer asks") {
    var seen = List.empty[String]
    def audit[A](p: A ! Failing): A ! Failing =
      okay.scala3.intercept(p, Handler.general[State % Int, Failing, A, A](pure)(
        [X] => (op: State[Int, X], k: X => A ! Failing) =>
          seen ::= op.toString
          perform[State % Int](op).flatMap(k)))
    val failing: Int ! Failing = State.put[Int, Failing](1).flatMap(_ => Fail.raise[String, Failing]("no"))
    val p = ensuring(failing)(State.put[Int, Failing](9))
    assertEquals(run(State.run(0)(Fail.run(audit(p)))), (9, Left("no")))
    assertEquals(seen.reverse, List("Put(1)", "Put(9)"))
  }

  test("ensuring, a program of a smaller row put into a bigger one: its finalizer runs there, its places renamed") {
    type Small[X] = (Fail % String + State % Int)[X]
    type Big[X] = (State % Int + Writer % String + Fail % String)[X]
    val failing: Unit ! Small = State.put[Int, Small](1).flatMap(_ => Fail.raise[String, Small]("no"))
    val small = ensuring(failing)(State.put[Int, Small](2))
    val p: Unit ! Big =
      for
        _ <- Writer.tell[String, Big]("before")
        _ <- small
        _ <- Writer.tell[String, Big]("after")
      yield ()
    assertEquals(run(State.run(0)(Fail.run(Writer.run(p)))), (2, Left("no")))
    assertEquals(run(State.run(0)(Writer.run(Fail.run(p)))), (2, (List("before"), Left("no"))))
  }

  /** where a program stopped: at an operation, with its rest — as a handler that steps it gives it */
  enum Stepped[A]:
    case Done(a: A)
    case Asked[A, X](op: Account[X], k: Continuation[X, Pure, Stepped[A]]) extends Stepped[A]

  def step[A](p: A ! Account): Stepped[A] ! Pure =
    handle(p, Handler.control[Account, Pure, A, Stepped[A]](a => pure(Stepped.Done(a)))(
      [X] => (op: Account[X], k: Continuation[X, Pure, Stepped[A]]) => pure(Stepped.Asked(op, k))))

  test("ensuring, a program stepped by its handler: whoever holds its rest resumes it, or unwinds it") {
    val p: Int ! Account =
      ensuring(Account.deposit(1).into[Account].flatMap(n => said[Account](s"got $n").map(_ => n)))(said("closed"))
    def resumed[X](op: Account[X], k: Continuation[X, Pure, Stepped[Int]]): Stepped[Int] ! Pure = op match
      case Account.Deposit(n) => k(n + 1)
    run(step(p)) match
      case asked: Stepped.Asked[a, x] =>
        assertEquals(logged(run(asked.k.unwind)), ((), List("closed")))
        assertEquals(logged(run(asked.k.unwind)), ((), List("closed")))
        assertEquals(logged(run(resumed(asked.op, asked.k))), (Stepped.Done(2), List("got 2", "closed")))
      case _ => fail("an operation was expected")
  }

  test("ensuring, an interceptor that unwinds: what a finalizer asks of its own effect reaches the handler outside") {
    var seen = List.empty[String]
    // an interceptor of the state that leaves at a put: the rest is unwound, then the state outside is read
    def leaving(p: Int ! Counter): Int ! Counter =
      okay.scala3.intercept(p, Handler.control[State % Int, Counter, Int, Int](pure)(
        [X] => (op: State[Int, X], k: Continuation[X, Counter, Int]) =>
          seen ::= op.toString
          op match
            case State.Put(_) => k.unwind.flatMap(_ => State.get[Int, Counter].map(_ + 100))
            case State.Get() => State.get[Int, Counter].flatMap(s => k(s))))
    val closing: Unit ! Counter = State.get[Int, Counter].flatMap(s => State.put[Int, Counter](s + 9))
    val body: Int ! Counter = State.put[Int, Counter](1).flatMap(_ => said[Counter]("not run").map(_ => 0))
    // the finalizer read the 5 outside and wrote 14 there: the put of 1 never passed the interceptor
    assertEquals(logged(run(State.run(5)(leaving(ensuring(body)(closing))))), ((14, 114), Nil))
    // the interceptor has left: it saw the put it left at, and nothing of the finalizer
    assertEquals(seen, List("Put(1)"))
  }

  test("ensuring, an interceptor that aborts: its rest is unwound, and it has left as one that unwinds has") {
    val closing: Unit ! Counter = State.get[Int, Counter].flatMap(s => State.put[Int, Counter](s + 9))
    val body: Int ! Counter = State.put[Int, Counter](1).flatMap(_ => said[Counter]("not run").map(_ => 0))
    val refused: Int ! Counter = okay.scala3.intercept(ensuring(body)(closing),
      Handler.abort[State % Int, Counter, Int, Int](pure)([X] => (_: State[Int, X]) => pure(-1)))
    assertEquals(logged(run(State.run(5)(refused))), ((14, -1), Nil))
  }

  test("ensuring, a finalizer stopped at an operation of the handler that left: its own finalizer still runs") {
    val failing: Int ! F = Fail.raise[String, F]("no")
    // the finalizer fails as the program did; that handler has left, under `handle`: nobody answers
    val raised: Unit ! F = Fail.raise[String, F]("again").flatMap(_ => said("not run"))
    val closing: Unit ! F = ensuring(said[F]("closing").flatMap(_ => raised))(said("inner"))
    val p = ensuring(ensuring(failing)(closing.flatMap(_ => said("not run"))))(said("outer"))
    // it ends at its operation, through its own finalizer; the unwinding goes on, and the first failure stands
    assertEquals(logged(run(Fail.run(p))), (Left("no"), List("closing", "inner", "outer")))
    // the same effect at another place of the row, its handler outside the one that left: answered there
    type Two[X] = (Fail % String + Fail % String)[X]
    val first = In[Fail % String, Two](Place.Init(Place.Only()))
    val inner: Int ! Two = Fail.raise[String, Two]("no")
    val other: Unit ! Two = Fail.raise[String, Two]("other")(using first).flatMap(_ => said("not run"))
    val q = ensuring(ensuring(inner)(other))(said("outer"))
    assertEquals(logged(run(Fail.run(Fail.run(q)))), (Left("other"), List("outer")))
  }

  test("ensuring, `k` unwound twice, then resumed, a handler in place inside it: each from the state `k` took") {
    type Kept[X] = (Account + State % Int)[X]
    val saw: Unit ! Kept =
      State.get[Int, Kept].flatMap(s => said[Kept](s"saw $s").flatMap(_ => State.put[Int, Kept](s + 1)))
    val body: Int ! Kept = State.put[Int, Kept](5).flatMap(_ =>
      Account.deposit(1).into[Kept].flatMap(n => State.get[Int, Kept].map(_ + n)))
    val p: (Int, Int) ! Account = State.run(0)(ensuring(body)(saw))
    val handler = Handler.control[Account, Pure, (Int, Int), (Int, Int)](pure)(
      [X] => (op: Account[X], k: Continuation[X, Pure, (Int, Int)]) => op match
        case Account.Deposit(n) => k.unwind.flatMap(_ => k.unwind).flatMap(_ => k(n + 1)))
    // each finalizer saw the 5 that `k` holds, and wrote its 6 in a state of its own path; the last path's is kept
    assertEquals(logged(run(handle(p, handler))), ((6, 7), List("saw 5", "saw 5", "saw 5")))
  }

  test("ensuring, a lifted value that gives the rest nothing: the rest is left, unwound") {
    type L[X] = (Fail % String + Lifted[Option])[X]
    def p(lifted: Option[Int]): Int ! L = ensuring(Lift.lift[Option, Int, L](lifted).map(_ + 1))(said("closed"))
    assertEquals(logged(run(Fail.run(Lift.handle[Option](p(Some(1)))))), (Right(Some(2)), List("closed")))
    assertEquals(logged(run(Fail.run(Lift.handle[Option](p(None))))), (Right(None), List("closed")))
    def lowered(lifted: Either[String, Int]): Either[String, Int] =
      type E[X] = Lifted[[A] =>> Either[String, A]][X]
      Lift.lower(ensuring(Lift.lift[[A] =>> Either[String, A], Int, E](lifted).map(_ + 1))(said("closed")))
    assertEquals(logged(lowered(Right(1))), (Right(2), List("closed")))
    assertEquals(logged(lowered(Left("no"))), (Left("no"), List("closed")))
    type Ls[X] = Lifted[List][X]
    val each: Int ! Ls = ensuring(Lift.lift[List, Int, Ls](List(1, 2)))(said("closed"))
    assertEquals(logged(run(Lift.handle[List](each.flatMap(x => Lift.lift[List, Int, Ls](Nil).map(_ + x))))),
      (Nil, List("closed", "closed")))
  }

  test("Scope: its releases run when the scope is left by any handler, the last acquired first") {
    type T[X] = (Choose + Scope)[X]
    val p: Int ! T =
      for
        _ <- Scope.acquire[String, T](pure("a"))(s => log ::= s"release $s")
        x <- Choose.among[Int, T](List(1, 2))
        _ <- Scope.defer[T](log ::= s"release $x")
        y <- if x == 1 then Choose.none[Int, T] else pure[Int, T](x)
      yield y
    // each path releases what it acquired, and what was acquired before the paths parted
    assertEquals(logged(run(Choose.all(Scope.run(p)))),
      (List(2), List("release 1", "release a", "release 2", "release a")))
  }
