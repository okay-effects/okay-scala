package okay.scala3

import okay.scala3.std.*

/** a row put into itself with effects added after it: its places shifted, and two such renamings composed as one */
class PrefixSuite extends munit.FunSuite:
  type Y[X] = (Yield % Int)[X]
  type YS[X] = (Y + State % Int)[X]
  type YSR[X] = (YS + Reader % Int)[X]

  test("shifted twice, composed: every place two further in") {
    val first = Sub.Shifted[Y, Y, State % Int](Sub.Same[Y]())
    val next = Sub.Shifted[YS, YS, Reader % Int](Sub.Same[YS]())
    val both = Sub.composed[Y, YS, YSR](first, next)
    assert(both.isInstanceOf[Sub.Shifted[?, ?, ?]], both)
    assertEquals(both.nn[Yield % Int](Place.Only[Yield % Int]()), Place.Init(Place.Init(Place.Only[Yield % Int]())))
    assertEquals(next[Yield % Int](first[Yield % Int](Place.Only[Yield % Int]())), Place.Init(Place.Init(Place.Only[Yield % Int]())))
  }

  test("shifted, then a row of two renamed effect by effect (More): through its renaming of the row before") {
    val first = Sub.Shifted[Y, Y, State % Int](Sub.Same[Y]())
    val same = Sub.More[Y, State % Int, YS](Sub.Shifted[Y, Y, State % Int](Sub.Same[Y]()), Place.Last())
    val both = Sub.composed[Y, YS, YS](first, same)
    assertEquals(both.nn[Yield % Int](Place.Only[Yield % Int]()), Place.Init(Place.Only[Yield % Int]()))
  }
