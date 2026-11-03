package okay.scala3

import okay.scala3.std.{Coeffects, History}
import scala.util.Random

/** COEFFECTS OVER AN EFFECT: the history a comonad (its laws); commands refused after three failures in a row, the
 * world not asked; the audit by extend the decisions made live */
class CoeffectsSuite extends munit.FunSuite:
  enum Cmd[X]:
    case Exec(cmd: String) extends Cmd[Boolean]

  test("the history's comonad laws over 300 histories"):
    val f: History[Int] => Int = h => h.past.sum * 3 + h.focus
    val g: History[Int] => Int = h => h.past.size - h.focus
    for seed <- 1 to 300 do
      val r = Random(seed)
      val h = History(Vector.fill(r.nextInt(8))(r.nextInt(10)), r.nextInt(10))
      assertEquals(h.extend(f).extract, f(h))
      assertEquals(h.extend(_.extract), h)
      assertEquals(h.extend(g).extend(f), h.extend(x => f(x.extend(g))))

  type Seen = (String, Option[Boolean])
  def stuck(h: History[Seen]): Boolean = h.past.reverseIterator.takeWhile(_._2.contains(false)).size >= 3

  def session(cmds: Vector[String]): (Vector[Boolean], Vector[Boolean], Int) =
    var ran = 0
    var decided = Vector.empty[Boolean]
    val program = cmds.foldLeft(pure[Vector[Boolean], Cmd](Vector.empty))((acc, c) =>
      acc.flatMap(v => perform[Cmd](Cmd.Exec(c)).map(v :+ _)))
    val guarded = Coeffects.overHistory[Cmd, Seen, Cmd, Vector[Boolean]](program)(
      [X] => (op: Cmd[X]) => op match { case Cmd.Exec(c) => (c, None) },
      [X] => (op: Cmd[X], x: X) => op match { case Cmd.Exec(c) => (c, Some(x)) })(
      [X] => (h: History[Seen], op: Cmd[X]) => op match
        case Cmd.Exec(_) =>
          val no = stuck(h)
          decided :+= no
          if no then Some(false) else None)
    val answers = run(handle(guarded, Handler.tail[Cmd, Pure, Vector[Boolean], Vector[Boolean]](pure)(
      [X] => (op: Cmd[X]) => op match { case Cmd.Exec(c) => ran += 1; !c.startsWith("bad") })))
    (answers, decided, ran)

  test("refused after three failures in a row, the world not asked; a success resets it"):
    val (answers, _, ran) = session(Vector("bad1", "bad2", "bad3", "ls", "ls"))
    assertEquals(answers, Vector(false, false, false, false, false))
    assertEquals(ran, 3)
    assertEquals(session(Vector("bad1", "bad2", "ok", "bad3", "ls"))._3, 5)

  test("the audit's law: extend of the decision over the history kept gives the decisions made live (300 sessions)"):
    var refused = 0
    for seed <- 1 to 300 do
      val r = Random(seed)
      val cmds = Vector.fill(1 + r.nextInt(15))(if r.nextDouble() < 0.5 then "bad" else "ok")
      val (answers, live, _) = session(cmds)
      val kept = cmds.zip(answers).map((c, a) => (c, Some(a)): Seen)
      assertEquals(History.of(kept).map(_.extend(stuck).all).getOrElse(Vector.empty), live, s"seed $seed")
      refused += live.count(identity)
    assert(refused > 100, s"$refused refused")
