# okay-scala

Algebraic effects and handlers for **Scala 3 alone**: the rows `A ! R`, `R + E`, `F % S`, with nothing given up for
Scala 2, and one reference implementation, a CEK machine. Effects are enums, handlers match them, and nothing is cast.
It depends on nothing, and nothing of it is shared: [okay-simple](https://github.com/okay-effects/okay-simple)'s
adapter runs okay-simple's interface on this machine, okay-simple's rows and places as they are.

**Start here: [getting started](docs/getting-started.md)** — the library added, an effect, a program, a handler, run.

```scala
import okay.scala3.*       // the rows, programs, handlers, the machine
import okay.scala3.std.*   // the standard effects: State, Reader, Writer, Fail, Async, ...

def greet[R[_]: Has[Reader % String] : Has[State % Int] : Has[Writer % String]]: Int ! R =
  for
    name <- Reader.ask
    n    <- State.get
    _    <- State.put(n + 1)
    _    <- Writer.tell(s"hello, $name")
  yield n

type Greeting[X] = (Reader % String + State % Int + Writer % String)[X]

run(State.run(12)(Reader.run("Ann")(Writer.run(greet[Greeting]))))   // (13, (List(hello, Ann), 12))
run(Writer.run(State.run(12)(Reader.run("Ann")(greet[Greeting]))))   // (List(hello, Ann), (13, 12))
```

A program says the effects it needs as context bounds (Scala 3.6 and later also write them `R[_]: {Has[...], Has[...]}`); its words — `Reader.ask`, `State.get` — find their row from
them, nothing more said. Handlers take their effect from wherever it is in the row, in any order. A row missing an
effect does not compile, and says so as the row is written:

```
the effect State % Int is not in the row Reader % String
```

## The rows

| | |
|---|---|
| `A ! R` | a program of the value `A` over the row `R` |
| `R + E` | the row `R` and the effect `E` after it: `[X] =>> Sum[R, E, X]` |
| `F % S` | the effect `F` with its parameter: `State % Int`, operations `State[Int, X]` |
| `Pure` | the row of no effect, `Void` |
| `In[E, R]`, `Has[E]` | `E` is in `R`: its place, found by given search; `Has` for context bounds |
| `Split[E, R, Rest]` | `E` in `R`, and `R` without it `Rest`: what a handler takes |
| `Sub[R, S]` | every effect of `R` is in `S`: a program put into a bigger row, `p.into[S]` |

The rows are written as okay-simple writes them, so a program written against okay-simple's interface reads the same
here; the classes are each library's own. The machine takes places of any representation (`At`): okay-simple's
module `okay-effects-cek` is an adapter giving it okay-simple's own places, and with it okay-simple's interface,
`okay.effects.Runner`, runs on this machine — `okay.effects.cek.OnCek` — okay-simple's whole suite of its interface
passing: its 30 tests, multi-shot, delimited control, generators, interception and all.

## Effects, words, handlers

An effect is an enum of its operations, each answered by its type parameter:

```scala
enum State[S, X]:
  case Get[S]() extends State[S, S]
  case Put[S](s: S) extends State[S, Unit]

object State:
  def get[S, R[_]](using In[State % S, R]): S ! R = perform[State % S](Get())
  def put[S, R[_]](s: S)(using In[State % S, R]): Unit ! R = perform[State % S](Put(s))
```

A handler is one of four shapes, each run its own way; a clause is a polymorphic function, and its match learns the
answer's type from the case:

| | |
|---|---|
| `Handler.general(ret)(op)` | `op` gets the operation and `k`, the rest: resumed once, many times (multi-shot), or never |
| `Handler.tail(ret)(answer)` | answered in place: the body goes on at once, no `k` made |
| `Handler.abort(ret)(abort)` | the rest dropped: the clause is the result, no `k` made |
| `Handler.stateful(init)(ret)(answer)` | answered in place with a state of its own, in a cell |

Each method gives a `Handler`; the cases themselves (`Handler.General(ret, op)`, ...) are constructors too.

```scala
def run[S, R[_], Rest[_], A](init: S)(p: A ! R)(using Split[State % S, R, Rest]): (S, A) ! Rest =
  handle(p, Handler.stateful[State % S, Rest, A, (S, A), S](init)((s, a) => pure((s, a)))(
    [X] => (op: State[S, X], cell: Cell[S]) => op match
      case Get() => cell.get
      case Put(s) => cell.set(s)))
```

| | |
|---|---|
| `pure(a)`, `unit` | a value |
| `p.flatMap(f)`, `p.map(f)`, `p.as(b)`, `p.andThen(q)` | in sequence; `for` comprehensions |
| `perform[E](op)` | an operation, the row from the evidence in scope or what is expected |
| `defer(p)`, `loop(a)(f)` | a tail call; a loop — both in constant stack |
| `handle(p, h)` | `E` handled from wherever it is in the row |
| `intercept(p, h)` | each operation of `E` to `h` first, then on to `E`'s handler outside: control and audit |
| `observe(p)(see)`, `around(p)(f)` | each operation of `E` seen, or wrapped, then passed on: an audit trail, a trace, a change |
| `ensuring(p)(release)`, `bracket(acquire)(use)(release)` | released when the program ends, or when a failure drops it |
| `p.into[S]`, `split(p)` | into a bigger row; an effect to the end of the row |
| `run(p)` | a program of no effect, run |

The standard effects, in `okay.scala3.std`:

| Effect | Words | Handlers |
|---|---|---|
| `State % S` | `get`, `put`, `modify`, `gets` | `State.run(init)` |
| `Reader % E` | `ask` | `Reader.run(env)`, `Reader.local(f)` |
| `Writer % W` | `tell` | `Writer.run`, `Writer.listen`, `Writer.censor(f)` |
| `Fail % E` | `raise` | `Fail.run` (an `Either`), `Fail.recover(p)(h)` |
| `Abort` | `exit` | `Abort.optional` (an `Option`) |
| `Choose` (multi-shot) | `among`, `flip`, `none`, `guard`, `once`, `mark`, `cut` | `Choose.all`, `Choose.first` |
| `Yield % O` (generators) | `emit` | `Yield.collect`, `Yield.foreach(f)`, `Yield.take(n)` |
| `Control % (B ! R)` | `shift` | `Control.reset` |
| `Lifted[G]` | `lift` | `Lift.lower`, by a `Lowering[G]`: `Option`, `Either`, `Try`, `Future` |
| `Fresh` | `next` | `Fresh.from(n)` |
| `Random` | `long`, `int(bound)`, `double` | `Random.seeded(seed)`: splitmix64, the same each run |
| `Store.Of[K, V]` | `load`, `save`, `remove` | `Store.inMemory(map)` |
| `Clock` | `now`, `sleep` | `Clock.virtual(start)`: time at once; `Clock.system` (JVM, Native) |
| `Env` | `variable`, `secret` | `Env.of(vars, secrets)`; `Env.system` |
| `Log` | `debug`, `info`, `warn`, `error` | `Log.collect(least)`, `Log.to(sink, least)` |
| `Async` | `fromFuture`, `async`, `cancellable`, `sleep`, `blocking`, `never`, `fork` (a `Fiber`: `join`, `cancel`), `both`, `parTraverse`, `race`, `timeout` | `Async.toFuture`, `Async.start` |
| `Sync` (side effects) | `delay`, `attempt` | `Sync.run`: in place; `Sync.on(pool)`: each on a pool, through `Async` |
| `Scope` (resources, fibers) | `acquire(a)(release)`, `defer(release)`, `fork` | `Scope.run`: all released at its end, the last first; its fibers cancelled |

Beside them, with no effect of their own:

| | |
|---|---|
| `Ref` | a value shared by fibers: `get`, `set`, `update`, `modify` — atomic |
| `Deferred` | a value given once: `complete`, `get` waits for it (`Async`) |
| `Semaphore` | permits: `acquire`, `release`, `withPermit(p)`; a wait cancelled takes none |
| `Queue` | `bounded(n)`, `unbounded`: `offer` waits while full, `take` while empty |
| `Stream[O, R]` | pulled, of any effects `R`, in chunks (`Chunk`, of 256 from a collection, a range, a Java stream; `chunks`, `unchunks`, `chunkLimit`, `mapChunks`; a program per value still per value); `Stream.bracket(acquire)(release)(use)`: a resource closed once — at the end, a failure, or the stream left (`take`, `headOption`, `zip`, a merge stopped, an iterator or Java stream closed); `emits`, `range`, `iterate`, `unfold`, `eval`, `generated` (a generator of `Yield.emit`), `fromQueue`, `fromIterator`, `fromLazyList`; `JavaStreams.from(javaStream)`, `JavaStreams.fromIterator` (JVM); of no effect, `toLazyList`, `iterator`, `toJavaStream`, `javaIterator` (JVM); `map`, `evalMap`, `filter`, `take`, `drop`, `zip`, `scan`, `grouped`, `++`, `flatMap`, `interleave`; `Stream.merge` (in turn, any row); `fold`, `toList`, `foreach`; of `Async`: `Stream.mergeAsync` (as they come), `parEvalMap(n)` |
| `Retry` | `times(n, retryOn)(p)`, `backoff(n, firstMs, factor, retryOn)(p)`: a program run again while it raises a `Fail` |

Asynchrony has no runtime of its own: a program of `Async` waits on callbacks — a future, a timer — and
`Async.toFuture` drives it, its other effects handled first. On the JVM, Scala.js and Scala Native:

```scala
given ExecutionContext = Async.defaultContext

def slow[A](ms: Long, a: A): A ! Async = Async.sleep[Async](ms).as(a)

Async.toFuture(Async.both(slow(200, "a"), slow(200, "b")))     // ("a", "b"), in 200 ms, not 400
Async.toFuture(Async.timeout(30)(slow(500, 1)))                 // None: the program cancelled
```

## Interop: ZIO, cats-effect, tofu

Each its own module, beside the library:

| Module | Package | Platforms | What it gives |
|---|---|---|---|
| `okay-scala-zio` | `okay.scala3.ziointerop` | JVM, JS, Native | ZIO in a program, a program a ZIO; typed; layers |
| `okay-scala-cats` | `okay.scala3.catsinterop` | JVM, JS, Native | cats' `Monad`; `IO` both ways, `Resource`, race |
| `okay-scala-tofu` | `okay.scala3.tofuinterop` | JVM | tofu's `WithLocal` of a `Reader`, `Errors` of a `Fail` |

```scala
libraryDependencies += "io.github.okay-effects" %% "okay-scala-zio" % "0.1.1"    // or okay-scala-cats, okay-scala-tofu
```

ZIO, its channels the standard effects: its environment a `Reader`, its error a `Fail`, its run an `Async` — a ZIO in a
program, and the program back as one ZIO for ZIO's runtime to run; `provideLayer`, `layer`. Tasks alone: `fromZIOTask`,
`toZIOTask`.

```scala
import okay.scala3.ziointerop.*

type Row[X] = ZioRow[Greeting, String][X]   // Async + Fail % String + Reader % ZEnvironment[Greeting]
val greeted: String ! Row = fromZIOTyped[Greeting, String, String, Row](ZIO.serviceWith[Greeting](_.greet("Ann")))
val back: ZIO[Greeting, String, String] = toZIOTyped(greeted)   // run by ZIO, as any ZIO
```

cats-effect, through `Async`: `IOs.fromIO`, `IOs.toIO`, `IOs.use(resource)(f)`, `IOs.both`, `IOs.race`. cats:
`monad[R]`, the programs' `Monad` (stack safe), and any cats monad a `Lowering` — `Lift.lower(p)` gives an `IO`.

```scala
import okay.scala3.catsinterop.{*, given}
given IORuntime = cats.effect.unsafe.implicits.global

val n: Int ! Async = IOs.fromIO[Int, Async](IO(20)).flatMap(a => IOs.fromIO[Int, Async](IO(a + 22)))
val io: IO[Int] = IOs.toIO(n)   // 42, run as any IO
```

tofu: `withLocal[R, Rest, C]` and `errors[R, Rest, E]`, for code written over `F[_]` with tofu's classes.

## The machine

One machine, `Cek`: the CEK machine of Felleisen and Friedman with the generalised continuations of Hillerström,
Lindley and Atkey. Its state is the program to run, its continuation σ up to the nearest handler, and κ, the handlers
with the frames outside each; a step is one case of a `@tailrec` loop, so no run takes stack — a million binds, a
million operations, ten thousand handlers deep. An operation goes out through κ, its place matched against each
handler's; answered in place, it goes on at once; taken by a general handler, σ and the handlers crossed become `k`,
data, resumed as many times as wanted. Its data are enums typed by the row, and Scala 3's GADT inference proves each
operation its handler's: no cast. [docs/design.md](docs/design.md) tells how, and why it is built so.

## Measured

okay-simple's own benchmark programs (its `bench/Lanes.scala`, n = 1000), JMH, µs per run, lower is better: on
okay-simple's machine; the same programs on this machine through `OnCek`; the same programs in this library's words.
okay-simple keeps this comparison, `benchCek`; this repository's `bench` measures the last column alone.

| | okay-simple machine | its programs on Cek | Cek, own words |
|---|---|---|---|
| binds | 7.7 | 7.9 | 7.6 |
| `ask` under its handler | 17.0 | 17.2 | 15.8 |
| `ask` across four handlers | 36.4 | 35.6 | 36.7 |
| a state's get and put | 19.0 | 17.9 | 18.0 |
| the same, row-polymorphic: `ask` near | 17.4 | 20.0 | 16.9 |
| `ask` across four | 30.2 | 29.2 | 30.2 |
| state | 17.3 | 18.1 | 19.0 |

The "across four" runs had read 21.6 and 15.7 here: a program whose only bind was the benchmark's own, so the JIT
inlined the machine's one call of a bind's function. `Choose.all`, made with a bind of its own (for `cut`), took that
away — as does any second bind in any program, so every real one: the numbers above are those, the machine unchanged.

The machine is as fast as okay-simple's tuned one, with no cast. One run, 2 forks × 5 iterations, on a laptop with
other work on it: read differences under 10% as noise.

Streams (`bench/StreamBenchmark.scala`, `sbt "bench/Jmh/run StreamBenchmark"`), µs per run, a value a step against
chunks of 256 — a step of the machine per chunk, a loop inside it; a program per value (`evalMap`) still run per value:

| | a value a step | chunks |
|---|---|---|
| `range(0, 100000).map.filter.fold` | 4676 | 699 |
| `emits(vector of 100000).fold` | 2847 | 712 |
| `iterate.take(100000).fold` | 4187 | 644 |
| `range zip range`, 50000 | 3036 | 447 |
| `flatMap` to pairs, 10000 | 1453 | 891 |
| `evalTap` of a state, 10000 | 794 | 578 |

One fork × 5 iterations, the same laptop.

## Building

sbt. The library is built by Scala 3.3 LTS, so any Scala 3 from 3.3 on reads it, and published for the JVM, Scala.js and
Scala Native: `sbt +coreJVM/test` runs its suite on Scala 3.3 and 3.9, `sbt -Dplatforms=jvm,js,native coreJS/test
coreNative/test` on the other platforms, `sbt "bench/Jmh/run"` the benchmark. Apache-2.0.

## Using it

```scala-sbt
resolvers += "okay-scala" at "https://okay-effects.github.io/okay-scala/maven"
libraryDependencies += "io.github.okay-effects" %% "okay-scala" % "0.1.1"     // %%% on Scala.js, Scala Native
```

Releases are a Maven repository on GitHub Pages ([RELEASING.md](RELEASING.md)).
