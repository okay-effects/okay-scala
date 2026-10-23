package okay.scala3.bench

/** every lane gives the same value on each */
class LanesSuite extends munit.FunSuite:
  test("the lanes agree") {
    val all = List(Lanes(okay.effects.machine.Machine), Lanes(okay.effects.cek.OnCek), NativeLanes())
    for lane <- all do
      assertEquals(lane.binds, 1000)
      assertEquals(lane.askNear, 1000)
      assertEquals(lane.askAcross4, 1000)
      assertEquals(lane.stateLoop, 1000)
      assertEquals(lane.askNearIn, 1000)
      assertEquals(lane.askAcross4In, 1000)
      assertEquals(lane.stateLoopIn, 1000)
  }
