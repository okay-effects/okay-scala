package okay.scala3

/** an operation that counts: its handler resumes the rest, and keeps how much the rest held */
enum Probe[X]:
  case Here() extends Probe[Unit]

/**
 * WHAT A LOOP MUST NOT GATHER, counted whatever the clock says: the levels and the frames a `k` holds, read from the
 * program a `k` resumed is (`Prog.Resume`) — the machine's own data. A handler put outermost counts all that is
 * between an operation and it.
 */
object Depth:
  /** the levels a `k` took, and `n` */
  def levels(crossed: Crossed[?, ?, ?, ?], n: Int = 0): Int = crossed match
    case _: Crossed.Nothing[r, v] => n
    case h: Crossed.Handled[ri, vi, e, r, rest, a, b, w] => levels(h.inner, n + 1)
    case r: Crossed.Renamed[ri, vi, r1, r2, a, w] => levels(r.inner, n + 1)
    case g: Crossed.Gone[ri, vi, e, r, rest, a, w] => levels(g.inner, n + 1)

  def frames(σ: Frames[?, ?, ?], n: Int = 0): Int = σ match
    case b: Frames.Bind[r, x, y, v] => frames(b.rest, n + 1)
    case m: Frames.Map[r, x, y, v] => frames(m.rest, n + 1)
    case f: Frames.Finally[r, x, v] => frames(f.rest, n + 1)
    case u: Frames.Unwinding[r, y, v] => frames(u.rest, n + 1)
    case e: Frames.Unwound[r, v] => frames(e.rest, n + 1)
    case _: Frames.End[r, v] => n

  /** the frames outside each level a `k` took, and `n` */
  def outside(crossed: Crossed[?, ?, ?, ?], n: Int = 0): Int = crossed match
    case _: Crossed.Nothing[r, v] => n
    case h: Crossed.Handled[ri, vi, e, r, rest, a, b, w] => outside(h.inner, frames(h.outside, n))
    case r: Crossed.Renamed[ri, vi, r1, r2, a, w] => outside(r.inner, frames(r.outside, n))
    case g: Crossed.Gone[ri, vi, e, r, rest, a, w] => outside(g.inner, frames(g.outside, n))

  /** of a `k` resumed: its levels, its handler's one of them, and its frames */
  def of(resumed: Prog[?, ?]): (Int, Int) = resumed match
    case r: Prog.Resume[rest, x, b, r0, v0, e, rr, a] => (1 + levels(r.crossed), outside(r.crossed, frames(r.inner)))
    case _ => (-1, -1)

  /** `p`, its probes answered by a handler that resumes: the value, and the most levels and frames a `k` held */
  def probed[R[_], Rest[_], A](p: A ! R)(using Split[Probe, R, Rest]): (A, (Int, Int)) ! Rest = defer {
    var most = (0, 0)
    handle(p, Handler.general[Probe, Rest, A, (A, (Int, Int))](a => pure((a, most)))(
      [X] => (op: Probe[X], k: X => (A, (Int, Int)) ! Rest) => op match
        case Probe.Here() =>
          val rest = k(())
          val (levels, frames) = of(rest)
          most = (most._1.max(levels), most._2.max(frames))
          rest))
  }

  def here[R[_]](using In[Probe, R]): Unit ! R = perform[Probe](Probe.Here())
