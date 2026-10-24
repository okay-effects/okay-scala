package okay.scala3

import okay.scala3.std.*

/** docs/getting-started.md's example, as it is written there */
class GettingStartedSuite extends munit.FunSuite:
  enum Accounts[X]:
    case Balance(who: String) extends Accounts[Int]
    case Deposit(who: String, amount: Int) extends Accounts[Unit]

  object Accounts:
    def balance[R[_]](who: String)(using In[Accounts, R]): Int ! R = perform[Accounts](Balance(who))
    def deposit[R[_]](who: String, n: Int)(using In[Accounts, R]): Unit ! R = perform[Accounts](Deposit(who, n))

    def inMemory[R[_], Rest[_], A](start: Map[String, Int])(p: A ! R)(using Split[Accounts, R, Rest])
        : (Map[String, Int], A) ! Rest =
      handle(p, Handler.stateful[Accounts, Rest, A, (Map[String, Int], A), Map[String, Int]](start)(
        (m, a) => pure((m, a)))(
        [X] => (op: Accounts[X], cell: Cell[Map[String, Int]]) => op match
          case Balance(who) => cell.get.getOrElse(who, 0)
          case Deposit(who, n) => cell.set(cell.get.updated(who, cell.get.getOrElse(who, 0) + n))))

  def payday[R[_]: Has[Accounts] : Has[Fail % String]]: Int ! R =
    for
      _ <- Accounts.deposit("ann", 100)
      b <- Accounts.balance("ann")
      _ <- if b > 1000 then Fail.raise("too rich") else unit
    yield b

  type App[X] = (Accounts + Fail % String)[X]

  test("getting started: payday on accounts in memory") {
    assertEquals(run(Fail.run(Accounts.inMemory(Map("ann" -> 50))(payday[App]))), Right((Map("ann" -> 150), 150)))
  }

  test("getting started: the handlers' other order keeps the accounts up to a failure") {
    type Other[X] = (Fail % String + Accounts)[X]
    assertEquals(run(Accounts.inMemory(Map("ann" -> 950))(Fail.run(payday[Other]))),
      (Map("ann" -> 1050), Left("too rich")))
    assertEquals(run(Fail.run(Accounts.inMemory(Map("ann" -> 950))(payday[App]))), Left("too rich"))
  }
