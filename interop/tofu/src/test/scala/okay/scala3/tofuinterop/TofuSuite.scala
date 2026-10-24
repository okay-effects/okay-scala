package okay.scala3.tofuinterop

import okay.scala3.*
import okay.scala3.std.*

/** tofu's typeclasses over the programs */
class TofuSuite extends munit.FunSuite:
  type Row[X] = (Fail % String + Reader % String)[X]
  type NoFail[X] = (Pure + Reader % String)[X]

  def runRow[A](p: A ! Row): Either[String, A] =
    run(Fail.run[String, Fail % String, Pure, A](Reader.run[String, Row, Fail % String, A]("ann")(p)))

  test("WithLocal: the context asked, and changed for a part") {
    val wl = withLocal[Row, Fail % String, String]
    val p: String ! Row = wl.context.flatMap(c =>
      wl.local(Reader.ask[String, Row].map(_ + "!"))(_.toUpperCase).map(c + " " + _))
    assertEquals(runRow(p), Right("ann ANN!"))
  }

  test("Errors: raised, handled with, restored") {
    val er = errors[Row, NoFail, String]
    val handled: Int ! Row = er.tryHandleWith(er.raise[Int]("bad"))(e => Some(pure[Int, Row](e.length)))
    val unhandled: Int ! Row = er.tryHandleWith(er.raise[Int]("bad"))(_ => None)
    val restored: Option[Int] ! Row = er.restore(er.raise[Int]("bad"))
    assertEquals(runRow(handled), Right(3))
    assertEquals(runRow(unhandled), Left("bad"))
    assertEquals(runRow(restored), Right(None))
  }

  test("code written over tofu's classes, run on the programs") {
    def greet[F[_]](using wl: tofu.WithLocal[F, String], er: tofu.Errors[F, String]): F[String] =
      wl.functor.map(er.restore(wl.context))(_.fold("nobody")(_.capitalize))
    assertEquals(runRow(greet[okay.scala3.catsinterop.Program[Row]](using withLocal[Row, Fail % String, String],
      errors[Row, NoFail, String])), Right("Ann"))
  }
