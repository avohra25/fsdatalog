module AnalysisTests

open Xunit
open FsDatalog

let check src = Analysis.check (Parser.parse src)

let fails (fragment: string) src =
    try
        check src |> ignore
        failwithf "expected an error containing '%s'" fragment
    with DatalogError msg -> Assert.Contains(fragment, msg)

[<Fact>]
let ``a safe program has no warnings`` () =
    Assert.Empty(check "e(1,2). p(X,Y) :- e(X,Y). p(X,Z) :- p(X,Y), e(Y,Z), not e(Z,X), Z > X. ?- p(1, Q).")

[<Fact>]
let ``assignment binds a variable`` () =
    Assert.Empty(check "e(1). p(X, Y) :- e(X), Y = X + 1. q(Z) :- p(_, Y), W = Y * 2, Z = W - 1.")

[<Fact>]
let ``assignment can be written either way round`` () =
    Assert.Empty(check "e(1). p(Y) :- e(X), X * 3 = Y.")

[<Fact>]
let ``undefined predicates produce a warning`` () =
    let w = check "p(X) :- q(X)."
    Assert.Equal<string list>([ "predicate 'q/1' is used but never defined" ], w)

[<Theory>]
[<InlineData("p(X) :- q(Y). q(1).", "head variable 'X' is not bound")>]
[<InlineData("p(X) :- q(X), not r(Y). q(1). r(1).", "variable 'Y' in 'not r(Y)' must also appear in a positive literal")>]
[<InlineData("p(X) :- q(X), X < Y. q(1).", "variable 'Y' in 'X < Y' is not bound")>]
[<InlineData("p(X) :- q(X), Y = Z + 1, Z = Y. q(1).", "is not bound")>]
[<InlineData("p(X).", "fact 'p(X)' must not contain variables")>]
[<InlineData("p(_) :- q(_). q(1).", "'_' is not allowed in a rule head")>]
[<InlineData("p(1). p(1, 2).", "predicate 'p' is used with 2 argument(s) here but with 1 on line 1")>]
[<InlineData("p(X) :- q(X, 1). q(1).", "predicate 'q' is used with 1 argument(s) here but with 2")>]
[<InlineData("c(count<X>).", "aggregates need a rule body")>]
[<InlineData("c(X, count<X>) :- q(X). q(1).", "both aggregated and grouped")>]
[<InlineData("c(sum<Y>) :- q(X). q(1).", "aggregated variable 'Y'")>]
[<InlineData("q(1). ?- q(X), X > Y.", "variable 'Y' in 'X > Y'")>]
let ``rejects ill-formed programs`` (src: string, fragment: string) = fails fragment src

[<Fact>]
let ``errors carry the line of the offending clause`` () =
    fails "line 3:" "q(1).\nr(2).\np(X) :- q(Y)."

[<Fact>]
let ``wildcards are allowed inside negation`` () =
    Assert.Empty(check "n(1). e(1,2). leaf(X) :- n(X), not e(X, _).")
