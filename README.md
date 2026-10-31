# fsdatalog

A Datalog engine written in F#, with no runtime dependencies beyond FSharp.Core.
It evaluates programs bottom-up with **semi-naive iteration** over **hash-indexed
relations**, supports **stratified negation**, **head aggregates**
(`count`, `sum`, `min`, `max`), checked integer arithmetic and comparisons,
and comes with an interactive REPL.

```text
$ dotnet run --project src/FsDatalog.Cli -- examples/graph.dl
X = a
X = b
X = c
3 answers.
...
```

## Language

```prolog
% facts: predicates and symbols are lower-case, strings are quoted
edge(a, b).  edge(b, c).  edge(c, a).  edge(c, d).
city("New York").

% rules: variables start with an upper-case letter or _
reach(X, Y) :- edge(X, Y).
reach(X, Z) :- reach(X, Y), edge(Y, Z).

% negation, comparisons and arithmetic (+ - * / mod)
sink(X)    :- reach(_, X), not edge(X, _).
next(X, Y) :- num(X), Y = X + 1, Y mod 2 = 0, Y <= 10.

% aggregates in rule heads, grouped by the other head arguments
out_degree(X, count<Y>) :- edge(X, Y).

% queries
?- reach(a, X).
```

* Comments are `% ...`, `// ...` or `/* ... */`.
* `_` is a fresh anonymous variable each time it appears. Other variables
  that start with `_` behave normally but are left out of query answers.
* Values are 64-bit integers and symbols. Integers sort before symbols, and
  symbols compare ordinally, so `<`, `min` and `max` work on both.

### Static checks

Before anything runs, every program is checked for:

* **Arity consistency**: each predicate is used with a single arity.
* **Safety (range restriction)**: head variables, variables in negated
  literals and variables in comparisons must be bound by a positive literal,
  or by an `X = expr` assignment whose right side is bound.
* **Aggregate well-formedness**: an aggregated variable is bound in the body
  and is not also a group-by key.
* **Stratifiability**: no predicate may depend on itself through negation or
  an aggregate.

Errors include the line number. Predicates that are used but never defined
produce a warning and are treated as empty.

## How it works

**Semi-naive evaluation.** Each rule is compiled once per recursive body atom
into a plan where that atom reads only the tuples derived in the previous
round (the *delta*) and every other atom reads the full relation. A round
therefore only does work involving new facts, and the transitive closure of a
300-node chain takes about 300 rounds and never re-derives a path.

**Indexes.** A relation is a hash set of tuples. The first lookup with a given
set of bound columns builds a hash index on those columns, and later inserts
keep every index up to date.

**Join planning.** A greedy planner orders each rule body. Filters and
negations run as soon as their variables are bound, assignments bind as early
as possible, and among the remaining atoms the one with the most bound
arguments is joined next. The delta atom always runs first. Results don't
depend on how the rule body is written (the test suite checks this).

**Stratification.** The predicate dependency graph is split into strongly
connected components with Tarjan's algorithm and evaluated in dependency
order, so a negated or aggregated predicate is complete before anything
reads it. A program like

```prolog
win(X) :- move(X, Y), not win(Y).
```

has no stratification and is rejected with an error naming the cycle.

**Aggregates.** An aggregate rule runs once its body's strata are done. Each
group is computed over the distinct bindings of the body's named variables,
so two different orders with the same quantity both count toward a `sum`.
A body with no matches produces no groups (`count` never yields 0).

**Arithmetic.** Overflow, division or `mod` by zero, and arithmetic on symbols
are reported as errors rather than silently wrapping.

## Command line and REPL

```sh
dotnet run --project src/FsDatalog.Cli -- [options] [file ...]
```

| Option | Meaning |
| --- | --- |
| `-e, --eval <text>` | evaluate clauses and queries given on the command line |
| `-i, --interactive` | start the prompt after loading files |
| `--dump` | print every relation after loading |
| `--stats` | print strata, rounds, fact counts and time |

With no files or `-e`, it starts a prompt. Clauses can span lines and end with
a `.`. The exit code is 1 if any input fails.

```text
dl> e(1,2). e(2,3).
dl> p(X,Y) :- e(X,Y).
dl> p(X,Z) :- p(X,Y), e(Y,Z).
dl> ?- p(1, X).
X = 2
X = 3
2 answers.
dl> ?- p(3, 1).
no.
dl> :stats
1 stratum, 3 rounds, 2 base facts, 3 derived facts, 41.4 ms
```

The prompt also accepts `:help`, `:rules`, `:dump [pred]`, `:strata`,
`:stats`, `:load <file>` and `:clear`.

## Library use

```fsharp
open FsDatalog

let db = Engine.run "edge(1,2). edge(2,3). path(X,Y) :- edge(X,Y). path(X,Z) :- path(X,Y), edge(Y,Z)."
let answer = Engine.queryText db "path(1, X)"
// answer.Columns = ["X"]; answer.Rows = [[|Int 2L|]; [|Int 3L|]]

let session = Session()
session.Execute "edge(3,4)." |> ignore
session.Execute "?- edge(X, 4)."   // ["X = 3"; "1 answer."]
```

## Examples

| File | Shows |
| --- | --- |
| [`examples/graph.dl`](examples/graph.dl) | reachability, cycles, negation |
| [`examples/family.dl`](examples/family.dl) | same-generation (non-linear recursion), `count` |
| [`examples/bom.dl`](examples/bom.dl) | bill of materials with arithmetic and `sum` |
| [`examples/shortest.dl`](examples/shortest.dl) | bounded shortest paths with `min` and `max` |
| [`examples/pointsto.dl`](examples/pointsto.dl) | Andersen-style points-to analysis |
| [`examples/game.dl`](examples/game.dl) | a program that is correctly rejected as unstratifiable |

## Layout

```text
src/FsDatalog/       Ast, Lexer, Parser, Pretty, Analysis (front end)
                     Relation, Plan, Stratify, Engine, Session (evaluation)
src/FsDatalog.Cli/   the fsdl command-line tool and REPL
tests/               xUnit tests, a naive reference evaluator, differential tests
examples/            sample programs (also run by the tests)
```

## Build and test

```sh
dotnet test
```

The suite has unit tests for each component, and it also generates hundreds
of random programs (positive, with stratified negation, and with aggregates).
Each one is checked against a deliberately simple naive evaluator, and the
suite checks that shuffling rule bodies never changes the result.

Requires the .NET 8 SDK.

## License

MIT
