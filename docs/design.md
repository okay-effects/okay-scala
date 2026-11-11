# Design

Why okay-scala is built as it is. It is okay-simple's interface written again for Scala 3 alone, and one machine for
it: what Scala 2 made okay-simple give up is taken back, and nothing is kept that one implementation does not need.

## The code, file by file

| File | What is in it |
|---|---|
| `Rows.scala` | the rows (`+`, `%`, `Void`), places (`Place`, `At`, `Taken`, `Escape`), `In`, `Split` (`Split.Natural`), `Sub` |
| `Prog.scala` | the language: `!`, `Prog` (the machine's C), its words, from `pure` to `run`; `Word`, `word`; in `Prog`'s companion the evidence of a bind, `Prog.Fits` and `Prog.Known` |
| `Handler.scala` | `Handler`, its shapes and their makers, `Cell`; `Continuation`, the rest as a clause is given it; `fresh` and `ret` |
| `Frames.scala` | K in its parts — `Frames` (σ), `Levels` (κ), `Crossed` — with `put`, `putLeft` and `holds` |
| `Cek.scala` | the machine's steps alone: `steps`, `enter`, `leave`, `out`, `here`, `inPlace`, `walk`, `run` |
| `Messages.scala` | the macro that says what effect a program lacks |
| `Branches.scala` | the macro that places each branch of an `if` or a `match` that is bound |
| `std/` | `okay.scala3.std`: the standard effects, their words and handlers — `Std`, `Choose`, `Services`, `Async`, `Sync`, `Concurrent` (`Ref`, `Deferred`, `Semaphore`, `Queue`), `Retry`, `Scope`, `Stream`, ... |
| `interop/zio` | `okay-scala-zio`: ZIO's tasks lifted and lowered; `ZioRow`, its channels effects; layers |
| `interop/cats` | `okay-scala-cats`: cats' `Monad` of programs, any monad a `Lowering`; `IOs` via `Async` |
| `interop/tofu` | `okay-scala-tofu` (JVM): tofu's `WithLocal` and `Errors` of a `Reader` and a `Fail` |

## The rows, and no shared class

A row is a type constructor: an effect (`Choose`, `State % Int`), `Pure`, or `R + E`, which is `[X] =>> Sum[R, E, X]`,
written as okay-simple writes it. The classes — `Sum`, `Void`, `Cell` — are this library's own, and okay-simple keeps
its own: the two depend on nothing of each other. What lets okay-simple's interface run on this machine anyway is that
the machine does not ask for its own places: it asks for an `At`.

`Sum` itself cannot be abstracted: the machine's proof that an operation is its handler's needs `R + E = R2 + E2` to
give `E = E2`, and Scala derives that only for a class, which is injective — of an abstract type it derives nothing.
So each library proves it for its own rows, with its own class, and the machine takes the proof as `Taken.Mine`.

The library is built by Scala 3.3 LTS: a compiler reads the TASTy of its own version and older, so any Scala 3 from
3.3 on — okay-simple's adapter among them — can use it. Its tests run on 3.3 and on 3.9.

## Places of any representation: `At` and `Escape`

```scala
trait At[E[_], R[_], Rest[_]]:
  def take[E2[_]](at: At[E2, R, ?]): Taken[E, E2, Rest]
  def outside[E2[_]](at: At[E2, R, ?]): At[E2, Rest, ?] | Null    // by default through take

trait Escape[P[_]]:
  def apply[E[_], X](at: At[E, P, ?], op: E[X]): Nothing
```

A place of `E` in `R` is whatever compares itself with another place of the same row: `take` answers `Mine`, whose type
says the operation's effect is the handler's, or `Theirs`, its place outside. This library's places, `Place`, are one
`At`, and the machine compares the commonest of them at once on its hot path — the last place against the last, a row
of one against itself — in a method kept small for the JIT to inline; anything else goes to a loop of its own
(`inPlaceTaken`, `outTaken`) and stays there. That loop asks each level `outside` — the operation's place beyond
it, or `null` at its own handler — which needs no proof and so can make nothing; the proof, `take`, it asks once, at
the handler it stops at. okay-simple's adapter answers both making nothing, its proof kept in the handler's place:
its runs allocate what okay-simple's own machine allocates, to a few hundred bytes a run. Measured against the machine before `At`, its own programs allocate the
same bytes per run and run in the same time, within the noise. Another representation compares its own: okay-simple's
adapter wraps okay-simple's places and calls okay-simple's `take`, whose `Mine` it turns into the machine's, the equality
proved where okay-simple's own types show it.

`Escape` is the top of a run: a run's row has no effect, so no operation gets past every handler, and its places say
why — here, an operation at `Pure`'s place is of `Void`, which has no value. `Cek.run(p)` runs this library's programs;
`Cek.run(p, escape)` another representation's.

## Places: the evidence is a typed path

`E` in `R` is its place, a path typed by the row:

```scala
enum Place[E[_], R[_], Rest[_]]:
  case Only[E[_]]() extends Place[E, E, Pure]
  case Last[R[_], E[_]]() extends Place[E, R + E, R]
  case Init[E[_], R[_], G[_], Rest[_]](at: Place[E, R, Rest]) extends Place[E, R + G, Rest + G]
  case Absurd[R[_]]() extends Place[Void, R, R]
```

found by given search, `only`, then `last`, then `init` — priorities by inheritance, so an effect twice in a row is found
at its last place, nearest the program, and an unknown row is taken to be one effect. An operation carries its place;
a handler its own. `Taken(mine, at)` compares two places of one row and answers `Mine` — whose type says the operation's
effect IS the handler's — or `Theirs`, its place outside. That is how the machine gives a handler its operation with no
cast: the GADT match proves it. The matches are `@unchecked` only because the checker cannot rule out an effect that is
itself a row; none is.

`Absurd` is the empty effect's place, in every row: where `Pure`'s places go when a program of no effect is put into a
row. No operation is ever there; one that were would be of `Void`, which has no value — `op.absurd`.

## The machine: CEK with generalised continuations

The CEK machine (Felleisen and Friedman, 1986) runs a program by a loop over a state ⟨C, E, K⟩: the control, the
environment, the continuation. For effect handlers Hillerström, Lindley and Atkey ("Effect handlers via generalised
continuations", JFP 2020) split K in two: σ, the pure frames up to the nearest handler, and κ, the handlers, each with
the frames outside it. Here:

| | |
|---|---|
| C | `Prog[R, X]`: the program, at the row `R` |
| E | Scala's closures: a bind's function carries what it needs |
| σ | `Frames[R, X, V]`: binds and maps, from `X` to the value `V` of the nearest handler's body |
| κ | `Levels[R, V, Z]`: each handler with its place and the frames outside it; renamings of places (`into`, `split`, `intercept`) |
| `k` | `Crossed` and σ: the levels an operation went out through, data, put back by `Resume` |

Everything is indexed by the row its program is at, so each step's types line up and the loop needs no cast. The loop is
`@tailrec`: no run takes stack. Its hot cases — a value into a bind, a bind, an operation answered in place, a `k`
resumed — are in the loop, kept small for the JIT; entering a handler (`enter`), leaving one (`leave`) and an
operation going out to a general handler (`out`) are methods of their own.

Multi-shot needs no copying: σ and κ are immutable data, so resuming `k` twice puts the same data back twice. The one
mutable thing, a stateful handler's cell, is given afresh — from the state it has then — each time its handler is
installed or put back by a `k`: so a state inside a choice is each branch's, one outside it threads through them.

## Handlers: their shapes, polymorphic clauses

```scala
enum Handler[E[_], R[_], A, B]:
  case General(ret: A => B ! R, op: [X] => (E[X], X => B ! R) => B ! R)
  case Tail(ret: A => B ! R, answer: [X] => E[X] => X)
  case Abort(ret: A => B ! R, abort: [X] => E[X] => B ! R)
  case Stateful[...](cell: Cell[S], ret: (S, A) => B ! R, answer: [X] => (E[X], Cell[S]) => X)
  case Control(ret: A => B ! R, op: [X] => (E[X], Continuation[X, R, B]) => B ! R)
  case Kept(h: Handler[E, R, A, B], at: At[E, R, ?])
```

A clause is a polymorphic function, `[X] => (op: State[S, X]) => op match ...`, and its match learns `X` from each case.
The shape tells the machine what it may skip: a `Tail` or `Stateful` operation is answered where it is, no `k` made, the
answer straight into the next frame; an `Abort` leaves the rest, and makes nothing for one that holds no finalizer;
a `General` or a `Control` handler is given `k` — as a function, or as it is: a `Continuation`, to resume or to unwind.
`Kept` is what `intercept` installs, its handler with the place its effect is kept at.

## Leaving: `ensuring` and `unwind`

`ensuring(p)(release)` is `p` with a finalizer, run when `p` is left: `p` ends with a value, or a handler outside it
unwinds the `k` that `p` is waiting in. The finalizer is a program of the same row, made when it runs — it reads a
state, emits, asks any effect of its row — or a thunk, a program that asks nothing. `bracket` is `ensuring` of the use.

1. **A value.** `p` ends with `x`: the finalizer runs, its value is dropped, and `x` is the result.
2. **Unwinding.** `k.unwind` runs the finalizers waiting in `k`, the innermost first, and nothing else of `k`: no
   function of a bind, no value clause of a handler inside `k`; the states in place inside `k` are dropped. Then `()`.
3. **Where a finalizer runs.** Where it was installed: the handlers of `k` between it and the one that unwinds answer
   it, a handler in place from the state `k` holds; the handlers outside answer it too.
4. **The handler that unwinds has left.** It answers no operation of a finalizer. One of its own effect goes to the
   handler outside under `intercept`; under `handle` nobody answers it: that finalizer ends there, a finalizer
   waiting inside it still runs, and the unwinding goes on.
5. **A finalizer that is itself left** — it fails — is left as any program is. At a value, the value that waited is
   dropped and the failure's handler has the result. In an unwinding, with its handler outside the one that unwinds:
   the finalizers still waiting run, and this handler's result stands. With its handler inside what is unwound: the
   failure is answered there, the handler's result dropped, and the unwinding goes on.
6. **`k` is a value.** Nothing is remembered of what was done with it. Unwound twice, its finalizers run twice;
   resumed after, its path runs and they run again at its end; each starts from the states in place as `k` took them.
7. **What runs nothing.** A clause that neither resumes nor unwinds `k` (`Handler.general` not calling it, a
   `Control.shift` that drops it); a generator kept in a stream that is not pulled to its end (`Stream.generated`);
   a throw of the host out of the run.
8. **Refused.** A handler inside the part unwound keeps the `k` of a finalizer's operation and resumes it on another
   stack: the finalizer ends, and there is no unwinding to go on with — `NoUnwinding`.

Who unwinds: `Handler.abort` — the machine does it, so `Fail`, `Abort`, `Retry` between its runs; `Choose` at a choice
of nothing (`none`, `guard`); `Yield.take` at its cut; `Lift` at a value that gives the rest none (a `None`, a `Left`);
`Async` when a wait fails or the run is cancelled (`cancel`, the loser of a `race`, a `timeout`, a fiber of a `Scope`
that ended) — there on the thread that leaves it, a finalizer that waits ending at its wait. `Scope.run` is `ensuring`
of its releases, kept as its handler's state: each path of a program resumed twice releases its own. A stream's
`release`, a thunk, is still what its own combinators call when they leave it unpulled (`take`, `headOption`, `zip`);
a pull that is left runs it through `ensuring`.

In the machine a finalizer is a frame on σ, `Finally`: a value that comes to it waits while the finalizer runs. A `k`
unwound (`Prog.Unwind`) is put back as it is resumed, each level's frames outside it under a frame `Unwinding`, and
without its handler: where that was is a level that answers nothing, `Gone`, over the end of the unwinding, `Unwound`.
Then `walk` passes the frames with no value: a bind's is dropped, a finalizer's runs its finalizer and comes back to
`Unwinding`, a level is left without its value clause. A rest that holds no finalizer is not walked: it is data, so
`holds` knows.

## Calls in tail position from row to row

A program put into another row (`into`, a bind of a smaller row) runs under a level that renames its operations'
places. In tail position — no frame waits for its value — under such a level, it gets none of its own: that one
renames for both, by the two renamings as one (`Sub.composed`), each place of the row taken to its place outside
once, so the renaming stays as big as the row. `ping` calling `pong.into` calling `ping.into`, a million times, keeps
one level; each call used to keep one, and every operation passed them all.

## Inline, and the one macro

`inline` is where it cannot change what a program means: the machine's own helpers, and `perform`, whose `Performing`
is an opaque `Unit` — nothing at run time. The words users write inside a handler's clause are plain methods. When Scala
inlines a method, it binds the arguments to proxies typed outside the clause's GADT match, so `k` — an `X => P`, `X`
known to be `Unit` in that case — no longer fits a `flatMap` that wants a `Unit => P`. The JIT inlines those small
methods anyway.

Two macros. `Branches` takes apart an `if` or a `match` that is bound, and places each branch — a word, a program —
in the row. `Messages.notIn`: the error of a missing effect. `In` and `Split` are opaque types over `Place`, each with a
fallback given, below the one that finds a place; it runs only when no place is found, at the top of the search, and
says the effect and the row as they are written — `State % Int`, `Reader % String + State % Int` — where Scala would
print the type lambdas they are, or, under a context bound, nothing at all.

## What the tests found

| | Found by | Fixed |
|---|---|---|
| `inline` words broke a clause's GADT match: `k` no longer fit `flatMap` | `Yield.foreach`, an interceptor | the words plain methods |
| `perform(Get[S]())` inferred a strange effect: `S` twice in `State[S, S]` | the standard effects | `perform[State % S](op)`: the effect said |
| under a context bound the not-found message said `E` and `R` | the compile-error test | opaque `In` and `Split`, a macro's message |
| `last` and `init` were ambiguous for an effect twice in a row | the prototype | priorities by inheritance |
| on Scala.js, a program of ten thousand handlers, built eagerly, overflowed the stack while it was built | the test of depth | the program built as the run gets to it, `defer` |
| `At` made the machine's own programs 8–25% slower: the place comparison grew too big to be inlined | an A/B of the commit before against the commit after | the commonest places compared in a small method, the rest in loops of their own |
| `Lift.lower` of a strict monad (`Either`) took stack: a run nested in each bind, one per operation | 100 000 lifts through `Lift.via` | such a program handled in place, `Lift.handle`, in constant stack; `lower` kept for the last, control monad |
