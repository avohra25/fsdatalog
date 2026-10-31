module AggregateTests

open Xunit
open FsDatalog
open TestUtil

[<Fact>]
let ``count per group`` () =
    let db = Engine.run "e(1,2). e(1,3). e(2,3). e(3,1). e(3,2). e(3,4). outdeg(X, count<Y>) :- e(X, Y)."
    assertRows (ints [ [ 1; 2 ]; [ 2; 1 ]; [ 3; 3 ] ]) (rows db "outdeg")

[<Fact>]
let ``count with no group gives a single total`` () =
    let db = Engine.run "e(1,2). e(1,3). e(2,3). edges(count<X>) :- e(X, _). total(count<P>) :- e(P, Q), Q > 0."
    // distinct named bindings: X in {1, 2} for edges; (P,Q) pairs for total.
    assertRows (ints [ [ 2 ] ]) (rows db "edges")
    assertRows (ints [ [ 3 ] ]) (rows db "total")

[<Fact>]
let ``sum min and max`` () =
    let db =
        Engine.run
            """
            sale(east, 10). sale(east, 25). sale(west, 7). sale(west, 7). sale(west, 3).
            total(R, sum<A>) :- sale(R, A).
            best(R, max<A>) :- sale(R, A).
            worst(R, min<A>) :- sale(R, A).
            """
    assertRows [ [| Sym "east"; Int 35L |]; [| Sym "west"; Int 10L |] ] (rows db "total")
    assertRows [ [| Sym "east"; Int 25L |]; [| Sym "west"; Int 7L |] ] (rows db "best")
    assertRows [ [| Sym "east"; Int 10L |]; [| Sym "west"; Int 3L |] ] (rows db "worst")

[<Fact>]
let ``sum aggregates over distinct bindings, not distinct values`` () =
    let db = Engine.run "line(o1, widget, 5). line(o2, widget, 5). qty(I, sum<N>) :- line(_O, I, N)."
    assertRows [ [| Sym "widget"; Int 10L |] ] (rows db "qty")

[<Fact>]
let ``empty bodies produce no groups`` () =
    let db = Engine.run "n(1). none(count<X>) :- n(X), X > 5."
    Assert.Empty(rows db "none")

[<Fact>]
let ``min and max work on symbols`` () =
    let db = Engine.run "name(bob). name(alice). name(carol). first(min<N>) :- name(N). last(max<N>) :- name(N)."
    assertRows [ [| Sym "alice" |] ] (rows db "first")
    assertRows [ [| Sym "carol" |] ] (rows db "last")

[<Fact>]
let ``aggregates feed later strata`` () =
    let src =
        """
        e(1,2). e(1,3). e(2,3). e(3,1).
        deg(X, count<Y>) :- e(X, Y).
        hub(X) :- deg(X, N), N >= 2.
        reach(X,Y) :- e(X,Y). reach(X,Z) :- reach(X,Y), e(Y,Z).
        reachable(X, count<Y>) :- reach(X, Y).
        """
    let db = Engine.run src
    assertRows (ints [ [ 1 ] ]) (rows db "hub")
    assertRows (ints [ [ 1; 3 ]; [ 2; 3 ]; [ 3; 3 ] ]) (rows db "reachable")

[<Fact>]
let ``bill of materials`` () =
    let src =
        """
        part(bike, frame, 1). part(bike, wheel, 2). part(wheel, spoke, 32). part(wheel, rim, 1).
        cost(frame, 120). cost(spoke, 1). cost(rim, 30).
        partCost(P, C) :- cost(P, C).
        contrib(Whole, Sub, X) :- part(Whole, Sub, N), unitCost(Sub, C), X = N * C.
        unitCost(P, C) :- partCost(P, C).
        """
    // recursion through sum is not allowed, so roll up one level at a time
    let db = Engine.run (src + "wheelCost(sum<X>) :- contrib(wheel, _S, X).")
    assertRows (ints [ [ 62 ] ]) (rows db "wheelCost")

[<Fact>]
let ``recursion through an aggregate is rejected`` () =
    failsWith "not stratifiable" (fun () -> Engine.run "e(1,2). n(X, count<Y>) :- e(X,Y), n(Y, _).")

[<Fact>]
let ``sum of symbols is an error`` () =
    failsWith "sum needs integers" (fun () -> Engine.run "v(a). s(sum<X>) :- v(X).")

[<Fact>]
let ``sum overflow is an error`` () =
    failsWith "integer overflow in sum" (fun () -> Engine.run "v(9223372036854775807). v(1). s(sum<X>) :- v(X).")
