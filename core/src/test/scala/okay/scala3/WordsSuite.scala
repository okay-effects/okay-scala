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

  test("the first of two: a handler's place and an operation's are one place, however each was found") {
    // an operation at the place a handler's evidence found: the first of two
    def got[R[_], Rest[_]](using Split[State % Int, R, Rest]): Int ! R = perform[State % Int](State.Get())
    val p: Int ! App = got[App, Choose].flatMap(s => Choose.among[Int, App](List(s, s + 1)))
    assertEquals(run(Choose.all(State.run(5)(p))), List((5, 5), (5, 6)))
    assertEquals(run(State.run(5)(Choose.all(p))), (5, List(5, 6)))
    // the handler at the place as it is written, the operation at the first of two
    assertEquals(run(Choose.all(State.run[Int, App, Pure + Choose, Int](5)(p))), List((5, 5), (5, 6)))
    // and in a bigger row, its places renamed
    type Wider[X] = (Writer % String + Choose + State % Int)[X]
    assertEquals(run(Writer.run(Choose.all(State.run(5)(p.into[Wider])))), (Nil, List((5, 5), (5, 6))))
  }

  def all[A](p: A ! App): List[(Int, A)] = run(Choose.all(State.run(0)(p)))
  def tick[R[_]: Has[State % Int]]: Unit ! R = State.modify[Int, R](_ + 1)

  test("a program with a declared row: a word at its head says no row") {
    val q: Int ! App = Choose.among(List(1, 2)).map(_ + 1)
    assertEquals(all(q), List((0, 2), (0, 3)))
    // the rows said, as before
    val said: Int ! App =
      for x <- Choose.among(List(1, 2)); s <- State.get[Int, App]; _ <- State.put[Int, App](s + x) yield s + x
    assertEquals(all(said), List((1, 1), (2, 2)))
    // and not said: `State.get[Int]` is the word, its type its own
    val p: Int ! App = for x <- Choose.among(List(1, 2)); s <- State.get[Int]; _ <- State.put(s + x) yield s + x
    assertEquals(all(p), List((1, 1), (2, 2)))
  }

  test("an effect of one's own: its words are made by `word`, with no evidence") {
    type Bank[X] = (State % Int + Account)[X]
    val paid: Int ! Bank = for a <- Account.deposit(10); b <- Account.deposit(5); _ <- State.put(a + b) yield b
    val counted = handle(paid, Handler.stateful[Account, State % Int, Int, Int, Int](0)((_, a) => pure(a))(
      [X] => (op: Account[X], sum: Cell[Int]) => op match
        case Account.Deposit(amount) => sum.set(sum.get + amount); sum.get))
    assertEquals(run(State.run(0)(counted)), (25, 15))
    // generic in its row, the evidence in scope; and `into` says the row
    def twice[R[_]: Has[Account]]: Int ! R = Account.deposit(1).flatMap(a => Account.deposit(a).map(_ + a))
    def sum[A](p: A ! Account): A = run(handle(p, Handler.stateful[Account, Pure, A, A, Int](0)((_, a) => pure(a))(
      [X] => (op: Account[X], sum: Cell[Int]) => op match
        case Account.Deposit(amount) => sum.set(sum.get + amount); sum.get)))
    assertEquals(sum(twice[Account]), 3)
    assertEquals(sum(Account.deposit(4).into[Account]), 4)
    assert(compileErrors("val p: Int ! App = Account.deposit(1)").contains(
      "the effect Account is not in the row State % Int + Choose"))
  }

  test("a word where a program is expected: alone, in a branch, in a recursion, after `pure` and `unit`") {
    val alone: Int ! App = Choose.among(List(1, 2))
    def down(n: Int): Int ! App = if n == 0 then State.get else State.put(n).flatMap(_ => down(n - 1))
    def either(n: Int): Int ! App = n match
      case 0 => Choose.among(List(1, 2))
      case _ => pure(n)
    val after: Int ! App = for _ <- unit; x <- pure(2); y <- Choose.among(List(x, 3)) yield y
    assertEquals(all(alone).map(_._2), List(1, 2))
    assertEquals(all(down(3)), List((1, 1)))
    assertEquals((all(either(0)) ++ all(either(5))).map(_._2), List(1, 2, 5))
    assertEquals(all(after).map(_._2), List(2, 3))
  }

  test("an `if` or a `match` that is bound, a word in one branch and a program in another") {
    val guarded: Int ! App =
      for x <- Choose.among(List(1, 2, 3)); _ <- if x == 2 then Choose.none[Unit] else unit yield x
    assertEquals(all(guarded).map(_._2), List(1, 3))
    def step(n: Int): Unit ! Three =
      for
        s <- State.get[Int]
        _ <-
          if n < 0 then Writer.tell("negative")
          else if n == 0 then tick
          else if s > 100 then unit
          else State.put(s + n)
        r <- n match
          case 0 => Reader.ask[Int]
          case 1 => State.get[Int]
          case _ => pure(n)
        _ <- Writer.tell(s"$n: $r")
      yield ()
    val p: Unit ! Three = step(-1).flatMap(_ => step(0)).flatMap(_ => step(1)).flatMap(_ => step(5))
    val told = List("negative", "-1: -1", "0: 7", "1: 2", "5: 5")
    assertEquals(run(Reader.run(7)(State.run(0)(Writer.run(p)))), (7, (told, ())))
    // a branch of an effect that is not in the row: the error names both
    val missing = compileErrors("val p: Unit ! App = for _ <- if true then Writer.tell(\"no\") else unit yield ()")
    assert(missing.contains("the effect Writer % String is not in the row State % Int + Choose"), missing)
  }

  test("a program of a smaller row is bound in a declared row: at the head of a `for`, and later") {
    val p: Int ! Three =
      for
        _ <- State.modify((s: Int) => s + 1)
        r <- Reader.ask[Int]
        _ <- tick
        s <- State.get[Int]
      yield r + s
    assertEquals(run(Reader.run(10)(State.run(0)(Writer.run(p)))), (2, (Nil, 12)))
    // `map` alone: the row expected of it is bigger than the program's
    val mapped: String ! Three = tick.map(_ => "done")
    assertEquals(run(Reader.run(0)(State.run(0)(Writer.run(mapped)))), (1, (Nil, "done")))
  }

  test("a continuation taken inside a program put into a row where it is bound is resumed twice across it") {
    type Big[X] = (Choose + Writer % String + State % Int)[X]
    val inner: Int ! App = for x <- Choose.among(List(1, 2)); s <- State.get[Int]; _ <- State.put(s + x) yield s + x
    val p: Int ! Big = for a <- inner; _ <- Writer.tell(s"$a"); b <- inner yield a * 10 + b
    // each way its own state, or one state through all
    assertEquals(run(Choose.all(Writer.run(State.run(0)(p)))).map(_._2._2), List(12, 13, 23, 24))
    assertEquals(run(State.run(0)(Choose.all(Writer.run(p))))._1, 9)
  }

  test("constant stack through a million programs put into a row where they are bound") {
    def ticks(n: Int): Int ! Three = if n == 0 then State.get else tick.flatMap(_ => ticks(n - 1))
    assertEquals(run(Reader.run(0)(State.run(0)(Writer.run(ticks(1_000_000)))))._1, 1_000_000)
  }

  test("a word without its type: from the evidence in scope, or where a program is expected; else it is said") {
    def bumped[R[_]: Has[State % Int] : Has[Reader % Int]]: Int ! R =
      for s <- State.get; r <- Reader.ask; _ <- State.put(s + r) yield s
    assertEquals(run(Reader.run(10)(State.run(1)(Writer.run(bumped[Three])))), (11, (Nil, 1)))
    val read: Int ! Three = State.get
    assertEquals(run(Reader.run(10)(State.run(1)(Writer.run(read))))._2._2, 1)
    // bound in a program of a declared row, with no evidence in scope, its type is said: the row comes to a bind
    // with the function it is bound to, too late to say what that function takes
    val said: Int ! Three = for s <- State.get[Int]; _ <- State.put(s + 1) yield s
    assertEquals(run(Reader.run(10)(State.run(1)(Writer.run(said))))._1, 2)
    assert(compileErrors("val p: Int ! Three = for s <- State.get; _ <- State.put(s + 1) yield s").nonEmpty)
  }

  test("a program with no declared type: of one effect it is that effect's; of more, the row is said once") {
    assertEquals(run(Choose.all(Choose.among(List(1, 2)).map(_ + 1))), List(2, 3))
    assertEquals(run(State.run(1)(for s <- State.get[Int]; _ <- State.put(s + 1) yield s)), (2, 1))
    val unsaid = compileErrors(
      "run(Choose.all(State.run(0)(for x <- Choose.among(List(1, 2)); _ <- State.put(x) yield x)))")
    assert(unsaid.contains("the effect Choose is not in the row State % Int"), unsaid)
    // said on the last word: a bind with no row expected takes the row of what follows it
    val said = for x <- Choose.among(List(1, 2)); _ <- State.put[Int, App](x) yield x
    assertEquals(all(said), List((1, 1), (2, 2)))
  }

  test("a bind with no row expected, of a program with a row: what follows it says its row, or is of that row") {
    val p: Int ! App = Choose.among(List(1, 2))
    // as before: what follows is of the row
    assertEquals(run(Choose.all(State.run(0)(p.flatMap(n => State.put[Int, App](n))))), List((1, ()), (2, ())))
    assertEquals(run(Choose.all(State.run(0)(p.flatMap(n => pure(n + 1))))), List((0, 2), (0, 3)))
    // a row expected: the words in it say none
    val declared: Unit ! App = p.flatMap(n => State.put(n))
    assertEquals(all(declared), List((1, ()), (2, ())))
    // NOT as before: with no row expected, a word that says no row is of its effect alone, and `p` does not fit it
    assert(compileErrors("run(Choose.all(State.run(0)(p.flatMap(n => State.put(n)))))").nonEmpty)
  }

enum Account[X]:
  case Deposit(amount: Int) extends Account[Int]

/** an effect's words: its operations, not yet placed in a row */
object Account:
  def deposit(amount: Int) = word(Deposit(amount))
