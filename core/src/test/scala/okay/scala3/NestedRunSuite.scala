package okay.scala3

import okay.scala3.std.*

import scala.concurrent.{ExecutionContext, Promise}

/** a run begun inside a running program, on the same thread, refused at once (`NestedRun`); the escape, a handler
 * installed inside a continuation, and runs begun elsewhere — after the run, from a callback — go on */
class NestedRunSuite extends munit.FunSuite:
  type RS[X] = (State % Int)[X]

  /** a loop whose every continuation runs the rest of the loop as a run of its own */
  def nested(i: Int, steps: Array[Int]): Int ! Pure =
    if i == 0 then pure(0)
    else pure[Int, Pure](1).flatMap { x =>
      steps(0) += 1
      pure(x + run(nested(i - 1, steps)))
    }

  test("a run inside a continuation, at each step: refused at the first, with what to do instead") {
    val steps = Array(0)
    val e = intercept[NestedRun](run(nested(1000, steps)))
    assertEquals(steps(0), 1)
    assert(e.getMessage.contains("runIsolated"), e.getMessage)
    assert(e.getMessage.contains("State.run(s)(p)"), e.getMessage)
    // the mark is cleared when the run is left by a throw: the thread runs again
    assertEquals(run(pure[Int, Pure](1).map(_ + 1)), 2)
  }

  test("a run inside a handler's clause: refused") {
    val p: Int ! RS = State.get[Int, RS].map(_ + 1)
    val h = Handler.general[State % Int, Pure, Int, Int](a => pure(a))(
      [X] => (op: State[Int, X], k: X => Int ! Pure) => op match
        case State.Get() => k(run(pure[Int, Pure](41)))
        case State.Put(_) => k(()))
    intercept[NestedRun](run(handle(p, h)))
  }

  test("Cek.run with an escape of another representation's rows: refused the same") {
    intercept[NestedRun](Cek.run(pure[Int, Pure](1).flatMap(x => pure(x + Cek.run(pure[Int, Pure](1), Escape.pure))),
      Escape.pure))
    assertEquals(Cek.run(pure[Int, Pure](1).flatMap(x => pure(x + Cek.runIsolated(pure[Int, Pure](1), Escape.pure))),
      Escape.pure), 2)
  }

  test("the escape: a closed sub-program run to its end inside a continuation, on purpose") {
    val p = pure[Int, Pure](1).flatMap(x => pure(x + runIsolated(State.run(x)(State.get[Int, RS]).map(_._2))))
    assertEquals(run(p), 2)
    // inside the isolated run, a run of its own is refused again; after it, the outer run goes on
    val q = pure[Int, Pure](1).flatMap(x => pure(x + runIsolated(pure[Int, Pure](1).map(y => y + run(pure(1))))))
    intercept[NestedRun](run(q))
    val r = pure[Int, Pure](1).flatMap(x => pure(runIsolated(pure[Int, Pure](x))).flatMap(y => pure(y + 1)))
    assertEquals(run(r), 2)
  }

  test("a handler installed inside a continuation, at each step, as a part of the program: no nested run") {
    def counting(i: Int): Unit ! RS =
      if i == 0 then pure(()) else State.get[Int, RS].flatMap(s => State.put[Int, RS](s + 1)).andThen(counting(i - 1))
    def each(i: Int, acc: Int): Int ! Pure =
      if i == 0 then pure(acc) else State.run(0)(counting(3)).flatMap((s, _) => each(i - 1, acc + s))
    assertEquals(run(each(100_000, 0)), 300_000)
  }

  test("a program started inside a program (a fork), and one resumed by a callback called inside a run") {
    // the callback of the wait is called by a step of another run on this thread: the waiting run goes on there,
    // on the parasitic context — a run of its own, not refused
    given ExecutionContext = ExecutionContext.parasitic
    val promise = Promise[Int]()
    val waiting = Async.toFuture(Async.fromFuture[Int, Async](promise.future).map(_ + 1))
    run(defer(pure[Unit, Pure](promise.success(41))))
    assertEquals(waiting.value.flatMap(_.toOption), Some(42))
    // a fork started by a step: driven at once, on this thread
    val forked = run(defer(pure[Int, Pure](Async.toFuture(Async.fromFuture[Int, Async](promise.future)).value
      .flatMap(_.toOption).getOrElse(-1))))
    assertEquals(forked, 41)
  }

  test("Lift.lower of a strict monad, inside a program: a conversion, its runs isolated") {
    // a monad that holds no value it says: each lifted list bound by its own bind, the rest run inside it
    given Lowering[List] with
      def pure[A](a: A): List[A] = List(a)
      def flatMap[A, B](ga: List[A])(f: A => List[B]): List[B] = ga.flatMap(f)
    val lowered = run(pure[Int, Pure](2).map(n => Lift.lower(Lift.lift[List, Int, Lifted[List]](List.range(0, n))
      .map(_ * 10))))
    assertEquals(lowered, List(0, 10))
  }

  test("a stream's iterator read inside a program: a conversion, its pulls isolated") {
    val s = Stream.emits[Int, Pure](List(1, 2, 3))
    assertEquals(run(pure[Unit, Pure](()).map(_ => s.iterator.toList)), List(1, 2, 3))
  }
