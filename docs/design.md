# Design

Why okay-scala is built as it is. It is okay-simple's interface written again for Scala 3 alone, and one machine for
it: what Scala 2 made okay-simple give up is taken back, and nothing is kept that one implementation does not need.

## The code, file by file

| File | What is in it |
|---|---|
| `Rows.scala` | the rows (`+`, `%`, `Void`), places (`Place`, `At`, `Taken`, `Escape`), `In`, `Split`, `Sub` |
| `Prog.scala` | the language: `!`, `Prog` (the machine's C), its words, from `pure` to `run` |
| `Handler.scala` | `Handler`, its four shapes and their makers, `Cell`; `fresh` and `ret` |
| `Frames.scala` | K in its parts — `Frames` (σ), `Levels` (κ), `Crossed` — with `released` and `put` |
| `Cek.scala` | the machine's steps alone: `steps`, `enter`, `leave`, `out`, `here`, `inPlace`, `run` |
| `Messages.scala` | the macro that says what effect a program lacks |
| `Std`, `Choose`, `Services`, `Control`, `Lift` | the standard effects, each with its words and handlers |
| `Async` (and `Timers` of each platform) | asynchrony: waits on callbacks, driven to a `Future` |

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

## Handlers: four shapes, polymorphic clauses

```scala
enum Handler[E[_], R[_], A, B]:
  case General(ret: A => B ! R, op: [X] => (E[X], X => B ! R) => B ! R)
  case Tail(ret: A => B ! R, answer: [X] => E[X] => X)
  case Abort(ret: A => B ! R, abort: [X] => E[X] => B ! R)
  case Stateful[...](cell: Cell[S], ret: (S, A) => B ! R, answer: [X] => (E[X], Cell[S]) => X)
```

A clause is a polymorphic function, `[X] => (op: State[S, X]) => op match ...`, and its match learns `X` from each case.
The shape tells the machine what it may skip: a `Tail` or `Stateful` operation is answered where it is, no `k` made, the
answer straight into the next frame; an `Abort` drops the rest with no `k`; only a `General` handler makes one.

## Resources: released when the rest is dropped

`ensuring(p)(release)` puts a frame on σ, `Finally`: a value passing it runs `release`. A handler that aborts — its
shape says it, `Abort` (`Fail`, `Abort`) — drops the rest as data the machine holds: the frames inside, the levels
crossed, each level's frames outside it. So the machine runs their `release`s, innermost first, before the clause.

A general handler drops the rest by not calling `k`, and that the machine cannot see: `k` is a function, never
called. So a `release` inside a way a `Choose` drops, or past what `Yield.take` took, does not run; and one inside a
`k` resumed twice runs at each end. A resource held across a general handler's operations needs that handler's own
care.

## Inline, and the one macro

`inline` is where it cannot change what a program means: the machine's own helpers, and `perform`, whose `Performing`
is an opaque `Unit` — nothing at run time. The words users write inside a handler's clause are plain methods. When Scala
inlines a method, it binds the arguments to proxies typed outside the clause's GADT match, so `k` — an `X => P`, `X`
known to be `Unit` in that case — no longer fits a `flatMap` that wants a `Unit => P`. The JIT inlines those small
methods anyway.

One macro, `Messages.notIn`: the error of a missing effect. `In` and `Split` are opaque types over `Place`, each with a
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
