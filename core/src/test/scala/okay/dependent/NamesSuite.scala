package okay.dependent

import okay.scala3.*
import okay.scala3.std.*

/** `import okay.scala3.*` brings what one writes — the words, the rows, the handlers — and not the library's evidence
 * and machinery: a dependent's own `Known`, `Fits`, `Natural` are still its own, beside programs written with words */
class NamesSuite extends munit.FunSuite:
  type App[X] = (State % Int + Choose)[X]

  test("a dependent's own names, the library's evidence under the same ones: each is its own") {
    val known: Known = Known("three")
    val fits: Fits = new Fits { def fits = Known.all.contains(known) }
    val unplaced: Unplaced = None
    val unsaid: Unsaid = "nothing"
    assertEquals((Known.all.size, fits.fits, Natural.first, unplaced, unsaid), (2, false, 1, None, "nothing"))
    assertEquals(Branches.of(2), List(0, 1))
    assertEquals(List[Any](SplitFound, FitsInto, NaturalFirst, NaturalInit).size, 4)
    assertEquals(List[Any](StateWords, ReaderWords, AbortWords, ChooseWords).size, 4)
  }

  test("and the library's still found: a bind, a word, branches, a handler's natural row") {
    val p: Int ! App =
      for
        x <- Choose.among(List(1, 2))
        s <- State.get[Int]
        _ <- if x == 2 then Choose.none[Unit] else State.put(s + x)
      yield s + x
    val chosen: (Int, Int) ! Choose = State.run(0)(p)
    assertEquals(run(Choose.all(chosen)), List((1, 1)))
    def generic[R[_]: Has[State % Int]]: Int ! R = for s <- State.get; _ <- State.put(s + 1) yield s
    assertEquals(run(State.run(4)(generic[State % Int])), (5, 4))
    val one: Word[State % Int, Int] = word(State.Get())
    assertEquals(run(State.run(7)(one.into[State % Int])), (7, 7))
  }
