# fsdatalog

A Datalog engine written in F# with no runtime dependencies beyond FSharp.Core.

> Work in progress: this first milestone contains the language front end
> (lexer, parser, pretty-printer) and the static checks. Evaluation lands next.

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

## Build and test

```sh
dotnet test
```

## License

MIT
