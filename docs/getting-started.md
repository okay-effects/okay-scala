# Getting started

Five steps: add the library, write an effect, a program that uses it, a handler, and run them.

## 1. Add it

```scala
// build.sbt
scalaVersion := "3.3.8"   // any Scala 3 from 3.3 on
resolvers += "okay-scala" at "https://okay-effects.github.io/okay-scala/maven"
libraryDependencies += "io.github.okay-effects" %% "okay-scala" % "0.1.1"   // %%% for Scala.js and Scala Native
```

Two imports: the library, and its standard effects:

```scala
import okay.scala3.*       // rows, programs, handlers, the machine
import okay.scala3.std.*   // State, Reader, Writer, Fail, Async, ... — here, Fail
```

## 2. An effect

An effect is an enum of its operations, each answered by its type parameter. Its words wrap `perform`:

```scala
enum Accounts[X]:
  case Balance(who: String) extends Accounts[Int]
  case Deposit(who: String, amount: Int) extends Accounts[Unit]

object Accounts:
  def balance[R[_]](who: String)(using In[Accounts, R]): Int ! R = perform[Accounts](Balance(who))
  def deposit[R[_]](who: String, n: Int)(using In[Accounts, R]): Unit ! R = perform[Accounts](Deposit(who, n))
```

`Int ! R` is a program giving an `Int` with the effects of the row `R`; `In[Accounts, R]` says `Accounts` is in it.

## 3. A program

A program says the effects it needs as context bounds, and is written with their words:

```scala
def payday[R[_]: Has[Accounts] : Has[Fail % String]]: Int ! R =
  for
    _ <- Accounts.deposit("ann", 100)
    b <- Accounts.balance("ann")
    _ <- if b > 1000 then Fail.raise("too rich") else unit
  yield b
```

Run it where an effect is not handled, and it does not compile — the error names the effect missing from the row.

## 4. A handler

A handler gives an effect its meaning. It is one of these shapes:

| | |
|---|---|
| `Handler.general` | gets `k`, the rest of the program: resumes it once, many times, or never |
| `Handler.control` | gets `k` as it is: resumes it, or leaves it with `k.unwind` — its finalizers (`ensuring`) run |
| `Handler.tail` | answers in place: the program goes on at once |
| `Handler.abort` | ends the program: the rest is left — unwound, its finalizers run |
| `Handler.stateful` | answers in place, with a state of its own in a `Cell` |

Accounts kept in a map, answered in place:

```scala
  def inMemory[R[_], Rest[_], A](start: Map[String, Int])(p: A ! R)(using Split[Accounts, R, Rest])
      : (Map[String, Int], A) ! Rest =
    handle(p, Handler.stateful[Accounts, Rest, A, (Map[String, Int], A), Map[String, Int]](start)(
      (m, a) => pure((m, a)))(
      [X] => (op: Accounts[X], cell: Cell[Map[String, Int]]) => op match
        case Balance(who) => cell.get.getOrElse(who, 0)
        case Deposit(who, n) => cell.set(cell.get.updated(who, cell.get.getOrElse(who, 0) + n))))
```

(it goes in `object Accounts`). `Split[Accounts, R, Rest]` says `Accounts` is in `R`, and `R` without it is `Rest`:
the handler takes its effect from wherever it is in the row.

## 5. Run

Name the row, put the program in its handlers, `run` it:

```scala
type App[X] = (Accounts + Fail % String)[X]

run(Fail.run(Accounts.inMemory(Map("ann" -> 50))(payday[App])))   // Right((Map(ann -> 150),150))
```

The order of the handlers is a choice of meaning: with `Fail` outside, a failure drops the accounts with the
result; put it inside, `Accounts.inMemory(...)(Fail.run(...))`, and the accounts are kept up to the failure.

## A row declared: words that say none

The program above is generic in its row, and takes its effects as evidence. A program can also DECLARE its row, and
then nothing in it has to say one: a bind takes the row expected of it, and what is bound is put into that row.

```scala
val payday: Int ! App =
  for
    _ <- Accounts.deposit("ann", 100)
    b <- Accounts.balance("ann")
    _ <- if b > 1000 then Fail.raise("too rich") else unit
  yield b
```

An effect's own words need no evidence, either. The old way, still there, and the new beside it:

```scala
def balance[R[_]](who: String)(using In[Accounts, R]): Int ! R = perform[Accounts](Balance(who))   // a program of `R`
def owed(who: String) = word(Balance(who))                 // a `Word[Accounts, Int]`: the operation, not yet placed
```

A word is placed where the row is known: bound in a `for` of a declared row, or under an evidence in scope; where a
program is expected (`val p: Int ! App = Accounts.owed("ann")`); or by `owed("ann").into[App]`. Of the standard
effects, the words whose type no argument says are there both ways: `State.get[Int]` beside `State.get[Int, App]`,
`Reader.ask[String]`, `Abort.exit[Int]`, `Choose.none[Int]`.

What still says a type:

- `s <- State.get[Int]` in a declared row — the row comes too late to say what the next line takes; under an evidence
  in scope (`R[_]: Has[State % Int]`) `State.get` says nothing, as before;
- a program with NO declared type takes its row from what follows its last bind: of two effects or more, declare it,
  or say the row there — `for x <- Choose.among(xs); _ <- State.put[Int, App](x) yield x`;
- a program of a smaller row bound in a bigger one (`_ <- tick`, with `def tick[R[_]: Has[State % Int]]`) runs under
  one more level of the machine, which renames its operations on their way out; said its row — `tick[App]` — it does
  not. A word costs nothing; an operation alone, or a value, is made anew at its place, with no level.

And a handler now leaves the row as one would write it: `App` without `Accounts` is `Fail % String` (it was
`Pure + Fail % String`, which is still found where it is the row asked for).

## Next

- the standard effects — `State`, `Reader`, `Writer`, `Fail`, `Abort`, `Choose`, `Yield`, `Fresh`, `Random`,
  `Store`, `Clock`, `Env`, `Log`, `Async`, `Sync`, `Scope`, `Control`, `Lift`; `Ref`, `Deferred`, `Semaphore`, `Queue`,
  `Retry`, `Stream` — and interception: [the README](../README.md)
- ZIO, cats-effect and tofu: modules of their own, [the README's interop](../README.md#interop-zio-cats-effect-tofu)
- how it works, and the code file by file: [design](design.md)
