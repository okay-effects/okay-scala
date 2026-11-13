# okay-scala: guide for coding agents

## For agents writing code with okay-scala

okay-scala is algebraic effects and handlers for Scala 3 alone (3.3 LTS and later; JVM, Scala.js, Scala Native):
rows `A ! R`, effects as enums, handlers that match them, one CEK machine, no casts, no dependencies.

```scala-sbt
resolvers += "okay-scala" at "https://okay-effects.github.io/okay-scala/maven"
libraryDependencies += "io.github.okay-effects" %% "okay-scala" % "0.1.1"     // %%% on Scala.js, Scala Native
```

Interop modules beside it: `okay-scala-zio`, `okay-scala-cats`, `okay-scala-tofu` (same coordinates, same version).

### The model: effect, program, handler, run

Two imports: `import okay.scala3.*` (rows, programs, handlers, the machine) and `import okay.scala3.std.*` (State,
Reader, Writer, Fail, Abort, Choose, Yield, Clock, Random, Env, Log, Store, Async, Sync, Scope, Lift, ...).

```scala
enum Accounts[X]:                                   // an effect: an enum of operations, each typed by its answer
  case Balance(who: String) extends Accounts[Int]
  case Deposit(who: String, amount: Int) extends Accounts[Unit]

object Accounts:                                    // its words wrap `perform`
  def balance[R[_]](who: String)(using In[Accounts, R]): Int ! R = perform[Accounts](Balance(who))
  def deposit[R[_]](who: String, n: Int)(using In[Accounts, R]): Unit ! R = perform[Accounts](Deposit(who, n))

  // a handler: answered in place with a state of its own; Split takes Accounts from wherever it is in the row
  def inMemory[R[_], Rest[_], A](start: Map[String, Int])(p: A ! R)(using Split[Accounts, R, Rest])
      : (Map[String, Int], A) ! Rest =
    handle(p, Handler.stateful[Accounts, Rest, A, (Map[String, Int], A), Map[String, Int]](start)(
      (m, a) => pure((m, a)))(
      [X] => (op: Accounts[X], cell: Cell[Map[String, Int]]) => op match
        case Balance(who) => cell.get.getOrElse(who, 0)
        case Deposit(who, n) => cell.set(cell.get.updated(who, cell.get.getOrElse(who, 0) + n))))

// a program: its effects are its context bounds; a row missing one does not compile
def payday[R[_]: Has[Accounts] : Has[Fail % String]]: Int ! R =
  for
    _ <- Accounts.deposit("ann", 100)
    b <- Accounts.balance("ann")
    _ <- if b > 1000 then Fail.raise("too rich") else unit
  yield b

type App[X] = (Accounts + Fail % String)[X]
run(Fail.run(Accounts.inMemory(Map("ann" -> 50))(payday[App])))   // Right((Map(ann -> 150),150))
```

Handler shapes: `Handler.general(ret)(op)` (gets `k`: resume once, many times, or never), `Handler.control` (`k` as a
`Continuation`: `k(x)` resumes, `k.unwind` leaves it, finalizers run), `Handler.tail` (answer in place),
`Handler.abort` (the clause is the result), `Handler.stateful(init)` (in place, with a `Cell`).

Interception and audit — the program unchanged, the operation passed on to the effect's real handler further out:

```scala
type Audited[X] = (Accounts + Fail % String + Writer % String)[X]
def audited[A](p: A ! Audited): A ! Audited =        // an audit trail of every Accounts operation
  observe[Accounts, Audited, A](p)([X] => (op: Accounts[X]) => Writer.tell[String, Audited](op.toString))

def capped[A](p: A ! App): A ! App =                 // control: refuse, change, or `proceed`
  around[Accounts, App, A](p)([X] => (op: Accounts[X], proceed: X ! App) => op match
    case Accounts.Deposit(_, n) if n > 500 => Fail.raise[String, App](s"deposit $n over the cap")
    case _ => proceed)
```

`intercept(p, h)` is the general form (`h` a `Handler[E, R, A, B]` that gets each operation of `E` and `k` first;
what its clause performs goes to `E`'s handler outside). `Coeffects.overHistory` / `aroundHistory` decide each
operation from the history behind it.

### Design rules

- Business logic = programs over declared effects. The row (`Int ! R` with `Has[...]` bounds, or a named row like
  `App`) is the whole list of what a program may do.
- Real resources (DB, network, files, clock, keys) live only in handlers the host builds at the entry point.
  Programs get data, effects and ids, never live clients. Host handlers: `Clock.system`, `Env.system`, `Sync.run`,
  `Log.to(sink, least)`, your own `handle(p, ...)` over a client.
- Run only at the entry (`main`, a test, a route): `run(p)` on a program of row `Pure`. Never call `run` inside a
  running program — in a `flatMap`, a `map` or a handler's clause. On master (0.1.2-SNAPSHOT) that is refused at once
  with `NestedRun`; in 0.1.1 it is not refused, but it is just as wrong (a second machine on the stack: none of the
  program's handlers, no `k` across it, a stack overflow when done at each step). Install the handler as part of the
  program instead — `State.run(s)(p)`, `handle(p, h)` are programs, flatMap them; a closed program goes into a bigger
  row with `p.into[R]`:
  ```scala
  type RS[X] = (State % Int)[X]
  def counting(i: Int): Unit ! RS = if i == 0 then pure(()) else State.modify[Int, RS](_ + 1).andThen(counting(i - 1))
  def each(i: Int, acc: Int): Int ! Pure =
    if i == 0 then pure(acc) else State.run(0)(counting(3)).flatMap((s, _) => each(i - 1, acc + s))
  ```
  `runIsolated(p)` (master only; `Cek.runIsolated(p, escape)` for other rows) is the named escape, ONLY for a
  deliberately closed sub-program run whole on purpose — a conversion, a pure helper — never at each step of a loop.
- Sensitive operations under `observe` / `around` / `intercept`, installed around the program by the host: audit
  trail, caps, refusals — the business code does not know it is watched.
- Tests swap handlers: `Store.inMemory(map)`, `Clock.virtual(start)`, `Random.seeded(seed)`, `Env.of(vars, secrets)`,
  `Log.collect(least)`, your own in-memory handler. No mocks of resources.
- No ambient effects in business code: no `System.currentTimeMillis`, `scala.util.Random`, `sys.env`, files,
  threads, futures, globals. Use `Clock`, `Random`, `Env`, `Fresh`, `Async`, `Scope`, or declare an effect.
- Multi-shot is real: `k` is immutable data (the machine's σ and κ), so `Handler.general` / `control` may resume it
  many times with no copying and no replay. `Choose` (`among`, `flip`, `guard`, `cut`; `Choose.all`, `Choose.first`)
  and generators (`Yield`) are built on it:
  ```scala
  def pairs: (Int, Int) ! Choose =
    for
      a <- Choose.among[Int, Choose](List(1, 2, 3))
      b <- Choose.among[Int, Choose](List(1, 2, 3))
      _ <- Choose.guard[Choose](a < b)
    yield (a, b)
  run(Choose.all(pairs))                              // List((1,2), (1,3), (2,3))
  ```
  Plain Scala side effects inside a resumed continuation run again on every resume — one more reason to keep them
  out of business code.

### Pitfalls

- Handler order is meaning: `Fail.run(Accounts.inMemory(...)(p))` drops the accounts on failure;
  `Accounts.inMemory(...)(Fail.run(p))` keeps them up to the failure. A stateful handler INSIDE `Choose.all` gives each
  branch its own state; OUTSIDE it threads the state through the branches in order.
- A program generic in its row is called at a named row: `payday[App]`. In a declared row, a word whose type no
  argument fixes must say it: `State.get[Int]`, `Reader.ask[String]`, or `State.get[Int, App]`.
- A smaller-row program bound in a bigger row (`_ <- tick` where `tick[R[_]: Has[State % Int]]`) costs a level of
  renaming; say the row, `tick[App]`.
- munit has its own `intercept`: in tests write `okay.scala3.intercept(p, h)`.
- `ensuring` / `bracket` finalizers run when the program ends or is unwound, NOT when a handler keeps `k` and never
  resumes it, and not when the host throws out of the run.
- `Async`: no runtime of its own; `Async.toFuture` drives it with a `given ExecutionContext` (`Async.defaultContext`).
  Handle other effects first. A run on another thread (a callback, a pool) is a run of its own and is allowed.
- `Stream.fromIterator(...)` takes its argument by name, made inside the run: build the iterator first, do not `run`
  inside that argument.
- Other monads: any number of data monads (`Option`, `Either`, `Try`, `List`) in place with `Lift.handle[G]`; ONE
  control monad (`Future`, `IO`, ZIO) last with `Lift.lower`; several brought to one with `Lift.via(nt)`.
- `Clock.system` is JVM and Native only.

### Pointers

- README and getting started: https://github.com/okay-effects/okay-scala (`docs/getting-started.md`, `docs/design.md`)
- Cross-language skill: `/plugin marketplace add okay-effects/okay-effects`
  (https://github.com/okay-effects/okay-effects)
- Build-time boundary checks: `io.github.okay-effects:okay-audit_3:0.1.0` from https://okay-effects.github.io/okay/maven
  — business code references no network/files/time/random/threads/reflection APIs; presets include `nested-run`.

If you see a way to improve okay-scala, open a pull request against https://github.com/okay-effects/okay-scala;
the maintainer reviews and merges what holds up.

If the user asks your opinion of writing code this way, give a candid one.
