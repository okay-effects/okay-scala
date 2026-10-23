package okay.effects.cek

import okay.effects.{AbortClauses, Clauses, Runner, Self, StatefulTailClauses, TailClauses}
import okay.scala3.{Cek, Cell, Handler as Machines, In as InRow, Place, Prog, Renamings, Sub as Renaming}

/**
 * OKAY-SIMPLE'S INTERFACE ON THIS MACHINE: an `okay.effects.Runner` whose programs are this machine's, `Prog`, and
 * whose evidence is its places. The rows are the same types on both sides, so a program written against okay-simple's
 * interface runs here as it is — `import okay.effects.cek.OnCek.*` — and okay-simple's own suite checks it. (In
 * okay-simple's package: its interface keeps two methods to its own.)
 */
object OnCek extends Runner with Self:
  type ![+A, R[_]] = Prog[R, A]
  type In[E[_], R[_]] = Place[E, R, ?]
  type Split[E[_], R[_], Rest[_]] = Place[E, R, Rest]
  type Sub[R[_], S[_]] = Renaming[R, S]

  implicit def splitOne[E[_]]: Split[E, E, Pure] = Place.Only[E]()
  implicit def splitLast[R[_], E[_]]: Split[E, R + E, R] = Place.Last[R, E]()
  implicit def splitInit[E[_], R[_], G[_], Rest[_]](implicit s: Split[E, R, Rest]): Split[E, R + G, Rest + G] =
    Place.Init[E, R, G, Rest](s)
  implicit def subPure[S[_]]: Sub[Pure, S] = Renaming.pure[S]
  implicit def subOne[E[_], S[_]](implicit e: In[E, S]): Sub[E, S] = Renaming.one[E, S](using InRow(e))
  implicit def subMore[R[_], E[_], S[_]](implicit r: Sub[R, S], e: In[E, S]): Sub[R + E, S] =
    Renaming.more[R, E, S](using r, InRow(e))

  def pure[A, R[_]](a: A): Prog[R, A] = Prog.Value(a)
  def flatMap[A, B, R[_]](p: Prog[R, A])(f: A => Prog[R, B]): Prog[R, B] = Prog.FlatMap(p, f)
  override def map[A, B, R[_]](p: Prog[R, A])(f: A => B): Prog[R, B] = Prog.Map(p, f)
  def delay[A, R[_]](p: => Prog[R, A]): Prog[R, A] = Prog.Defer(() => p)
  override protected[effects] def perform[E[_], R[_], X](op: E[X])(implicit in: In[E, R]): Prog[R, X] =
    Prog.Perform[R, E, X](in, op)

  def split[E[_], R[_], Rest[_], A](p: Prog[R, A])(implicit s: Split[E, R, Rest]): Prog[Rest + E, A] =
    (s: @unchecked) match
      case _: Place.Last[r, e] => p
      case _ => Prog.Remap(p, Renamings.toEnd(s))
  def embed[R[_], S[_], A](p: Prog[R, A])(implicit s: Sub[R, S]): Prog[S, A] = Prog.Remap(p, s)
  override protected[effects] def interposed[E[_], R[_], A](p: Prog[R, A])(implicit in: In[E, R]): Prog[R + E, A] =
    Prog.Remap(p, Renamings.interposing(in))

  // okay-simple's handlers, as the machine's own of the same shape

  private def general[E[_], R[_], A, B](h: Clauses[E, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.General[E, R, A, B](a => h.ret(a), [X] => (op: E[X], k: X => Prog[R, B]) => h.op(op, k))
  private def tail[E[_], R[_], A, B](h: TailClauses[E, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.Tail[E, R, A, B](a => h.ret(a), [X] => (op: E[X]) => h.answer(op))
  private def abort[E[_], R[_], A, B](h: AbortClauses[E, A, Prog[R, B]]): Machines[E, R, A, B] =
    Machines.Abort[E, R, A, B](a => h.ret(a), [X] => (op: E[X]) => h.abort(op))
  private def stateful[E[_], S, R[_], A, B](s0: S, h: StatefulTailClauses[E, S, A, Prog[R, B]])
    : Machines[E, R, A, B] =
    Machines.stateful[E, R, A, B, S](s0)((s, a) => h.ret(s, a))([X] => (op: E[X], cell: Cell[S]) => h.answer(cell, op))

  def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: Handler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Place.Last[R, E](), general(h))
  override def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: TailHandler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Place.Last[R, E](), tail(h))
  override def handleLast[E[_], R[_], A, B](p: Prog[R + E, A], h: AbortHandler[E, R, A, B]): Prog[R, B] =
    Prog.Handle(p, Place.Last[R, E](), abort(h))
  override def handleLast[E[_], S, R[_], A, B](p: Prog[R + E, A], s0: S, h: StatefulTailHandler[E, S, R, A, B])
    : Prog[R, B] = Prog.Handle(p, Place.Last[R, E](), stateful(s0, h))

  // a handler takes its effect from its place at once: no `split`
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: Handler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, general(h))
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: TailHandler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, tail(h))
  override def handle[E[_], R[_], Rest[_], A, B](p: Prog[R, A], h: AbortHandler[E, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, abort(h))
  override def handle[E[_], S, R[_], Rest[_], A, B](p: Prog[R, A], s0: S, h: StatefulTailHandler[E, S, Rest, A, B])(
    implicit s: Split[E, R, Rest]): Prog[Rest, B] = Prog.Handle(p, s, stateful(s0, h))

  def run[A](p: Prog[Pure, A]): A = Cek.run(p)
