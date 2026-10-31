module NegationTests

open Xunit
open FsDatalog
open TestUtil

[<Fact>]
let ``set difference`` () =
    let db = Engine.run "a(1). a(2). a(3). b(2). d(X) :- a(X), not b(X)."
    assertRows (ints [ [ 1 ]; [ 3 ] ]) (rows db "d")

[<Fact>]
let ``negation over a derived recursive relation`` () =
    let src =
        """
        node(1). node(2). node(3). node(4).
        edge(1,2). edge(2,3).
        reach(X,Y) :- edge(X,Y).
        reach(X,Z) :- reach(X,Y), edge(Y,Z).
        unreachable(X,Y) :- node(X), node(Y), X != Y, not reach(X,Y).
        """
    let db = Engine.run src
    Assert.Equal(12 - 3, (rows db "unreachable").Length)
    Assert.DoesNotContain([| Int 1L; Int 3L |], rows db "unreachable")

[<Fact>]
let ``wildcards in negated atoms mean there is no such tuple`` () =
    let db = Engine.run "n(1). n(2). n(3). e(1,5). e(3,1). sink(X) :- n(X), not e(X, _)."
    assertRows (ints [ [ 2 ] ]) (rows db "sink")

[<Fact>]
let ``negated constants`` () =
    let db = Engine.run "p(1). p(2). bad(2). good(X) :- p(X), not bad(X), not bad(7)."
    assertRows (ints [ [ 1 ] ]) (rows db "good")

[<Fact>]
let ``negation of an undefined predicate is always true`` () =
    let db = Engine.run "p(1). q(X) :- p(X), not nothing(X)."
    assertRows (ints [ [ 1 ] ]) (rows db "q")

[<Fact>]
let ``three strata`` () =
    let db = Engine.run "a(1). a(2). a(3). b(1). c(X) :- a(X), not b(X). d(X) :- a(X), not c(X)."
    assertRows (ints [ [ 1 ] ]) (rows db "d")
    Assert.Equal(3, db.Stats.Strata)

[<Fact>]
let ``negation in queries`` () =
    let db = Engine.run "a(1). a(2). b(2)."
    assertRows (ints [ [ 1 ] ]) (Engine.queryText db "a(X), not b(X)").Rows

[<Fact>]
let ``unsafe negation is rejected`` () =
    failsWith "must also appear in a positive literal" (fun () -> Engine.run "a(1). p(X) :- a(X), not b(Y).")

[<Fact>]
let ``non-stratifiable programs are rejected before evaluation`` () =
    failsWith "not stratifiable" (fun () -> Engine.run "move(a,b). move(b,c). win(X) :- move(X,Y), not win(Y).")
