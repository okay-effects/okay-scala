# okay-scala

Algebraic effects and handlers for **Scala 3 alone**: the rows `A ! R`, `R + E`, `F % S`, with nothing given up for
Scala 2, and one reference implementation, a CEK machine. Effects are enums, handlers match them, and nothing is cast.
It depends on nothing, and nothing of it is shared: [okay-simple](https://github.com/okay-effects/okay-simple)'s
adapter runs okay-simple's interface on this machine, okay-simple's rows and places as they are.

```scala
import okay.scala3.*

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
| `p.into[S]`, `split(p)` | into a bigger row; an effect to the end of the row |
| `run(p)` | a program of no effect, run |

The standard effects: `State`, `Reader`, `Writer`, `Fail`, `Choose` (multi-shot), `Yield` (generators).

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
| `ask` across four handlers | 36.4 | 35.6 | 21.6 |
| a state's get and put | 19.0 | 17.9 | 18.0 |
| the same, row-polymorphic: `ask` near | 17.4 | 20.0 | 16.9 |
| `ask` across four | 30.2 | 29.2 | 15.7 |
| state | 17.3 | 18.1 | 19.0 |

The machine is as fast as okay-simple's tuned one, with no cast. The gap across four handlers is
not the machine's: okay-simple's `Choose` handler is a general handler with a state of its own, which its interface
makes of functions of the state; here `Choose` is a plain general handler. One run, 2 forks × 5 iterations, on a laptop
with other work on it: read differences under 10% as noise.

## Building

sbt. The library is built by Scala 3.3 LTS, so any Scala 3 from 3.3 on reads it, and published for the JVM, Scala.js and
Scala Native: `sbt +coreJVM/test` runs its suite on Scala 3.3 and 3.9, `sbt -Dplatforms=jvm,js,native coreJS/test
coreNative/test` on the other platforms, `sbt "bench/Jmh/run"` the benchmark. Apache-2.0.

## Using it

```scala-sbt
resolvers += "okay-scala" at "https://okay-effects.github.io/okay-scala/maven"
libraryDependencies += "io.github.okay-effects" %% "okay-scala" % "0.1.0"     // %%% on Scala.js, Scala Native
```

Releases are a Maven repository on GitHub Pages ([RELEASING.md](RELEASING.md)).
