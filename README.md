# fsdatalog

A Datalog engine written in F# with no runtime dependencies beyond FSharp.Core.

> Work in progress: positive programs and stratified negation evaluate today.
> Aggregates parse and type-check but are rejected by the engine until the
> next milestone.

## Language

```prolog
% facts
edge(1, 2).  edge(2, 3).  city("New York").

% rules (variables start with an upper-case letter or _)
path(X, Y) :- edge(X, Y).
path(X, Z) :- path(X, Y), edge(Y, Z).

% negation, comparisons and arithmetic
leaf(X)       :- node(X), not edge(X, _).
next(X, Y)    :- node(X), Y = X + 1, Y <= 10.

% aggregates in rule heads
out_degree(X, count<Y>) :- edge(X, Y).

% queries
?- path(1, X).
```

The front end already enforces:

* **Arity consistency**: every predicate is used with one arity.
* **Safety (range restriction)**: head variables, variables in negated
  literals and variables in comparisons must be bound by a positive literal
  (or by an `X = expr` assignment whose right side is bound).
* **Aggregate well-formedness**: aggregated variables are bound and are not
  also group-by keys.

Errors carry line and column information.

## Evaluation

Programs are evaluated bottom-up with **semi-naive iteration**. Each rule is
compiled once per recursive body atom into a plan where that atom reads only
the tuples derived in the previous round (the *delta*) and every other atom
reads the full relation, so a round never re-derives what earlier rounds
already found. Transitive closure over a 300-node chain takes about 300
rounds and touches each path once.

Relations are hash sets of tuples with **lazily built hash indexes**: the
first lookup on a given set of bound columns builds an index on those
columns, and later inserts keep it up to date. A greedy planner orders each
rule body: filters run as soon as their variables are bound, assignments
bind as early as possible, and among the remaining atoms the one with the
most bound arguments is joined next.

### Stratified negation

`not p(...)` is evaluated under the standard stratified semantics. The
engine builds the predicate dependency graph, finds its strongly connected
components with Tarjan's algorithm, and evaluates them in dependency order,
so a negated predicate is always complete before anything reads it. A
program in which a predicate depends on itself through negation, such as
`win(X) :- move(X, Y), not win(Y).`, has no stratification and is rejected
with an error naming the cycle.

Arithmetic is checked: overflow, division by zero and arithmetic on symbols
are reported as errors rather than silently wrapping.

```fsharp
open FsDatalog

let db = Engine.run "edge(1,2). edge(2,3). path(X,Y) :- edge(X,Y). path(X,Z) :- path(X,Y), edge(Y,Z)."
let answer = Engine.queryText db "path(1, X)"
// answer.Columns = ["X"]; answer.Rows = [[|Int 2L|]; [|Int 3L|]]
```

## Build and test

```sh
dotnet test
```

Besides unit tests, the suite generates hundreds of random programs and
checks the engine against a deliberately simple naive evaluator, and checks
that shuffling rule bodies never changes the result.

## License

MIT
