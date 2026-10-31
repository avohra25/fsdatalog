module EngineTests

open Xunit
open FsDatalog
open TestUtil

[<Fact>]
let ``transitive closure of a chain`` () =
    let db = Engine.run "edge(1,2). edge(2,3). edge(3,4). path(X,Y) :- edge(X,Y). path(X,Z) :- path(X,Y), edge(Y,Z)."
    assertRows (ints [ [ 1; 2 ]; [ 1; 3 ]; [ 1; 4 ]; [ 2; 3 ]; [ 2; 4 ]; [ 3; 4 ] ]) (rows db "path")

[<Fact>]
let ``transitive closure of a cycle terminates`` () =
    let db = Engine.run "edge(a,b). edge(b,c). edge(c,a). path(X,Y) :- edge(X,Y). path(X,Z) :- path(X,Y), path(Y,Z)."
    Assert.Equal(9, (rows db "path").Length)

[<Fact>]
let ``semi-naive needs one round per path length`` () =
    let n = 300
    let facts = [ for i in 1 .. n - 1 -> sprintf "e(%d,%d)." i (i + 1) ] |> String.concat " "
    let db = Engine.run (facts + " p(X,Y) :- e(X,Y). p(X,Z) :- p(X,Y), e(Y,Z).")
    Assert.Equal(n * (n - 1) / 2, (rows db "p").Length)
    Assert.InRange(db.Stats.Iterations, n - 2, n + 1)
    Assert.Equal(n - 1, db.Stats.Facts)

[<Fact>]
let ``same generation (non-linear recursion)`` () =
    let src =
        """
        parent(ann, bob). parent(ann, cat). parent(bob, dan). parent(cat, eve). parent(dan, fay).
        sg(X, Y) :- parent(P, X), parent(P, Y), X != Y.
        sg(X, Y) :- parent(A, X), sg(A, B), parent(B, Y).
        """
    let db = Engine.run src
    let pairs = rows db "sg" |> List.map (fun r -> Pretty.value r.[0], Pretty.value r.[1])
    Assert.Equal<(string * string) list>([ "bob", "cat"; "cat", "bob"; "dan", "eve"; "eve", "dan" ], pairs)

[<Fact>]
let ``repeated variables and constants in atoms`` () =
    let db = Engine.run "e(1,1). e(1,2). e(2,2). e(3,1). loop(X) :- e(X,X). fromOne(Y) :- e(1,Y)."
    assertRows (ints [ [ 1 ]; [ 2 ] ]) (rows db "loop")
    assertRows (ints [ [ 1 ]; [ 2 ] ]) (rows db "fromOne")

[<Fact>]
let ``wildcards do not join`` () =
    let db = Engine.run "e(1,2). e(3,4). src(X) :- e(X, _), e(_, _)."
    assertRows (ints [ [ 1 ]; [ 3 ] ]) (rows db "src")

[<Fact>]
let ``arithmetic with bounded recursion`` () =
    let db = Engine.run "nat(0). nat(Y) :- nat(X), Y = X + 1, Y < 10. sq(X, Y) :- nat(X), Y = X * X, Y mod 2 = 0."
    Assert.Equal(10, (rows db "nat").Length)
    assertRows (ints [ [ 0; 0 ]; [ 2; 4 ]; [ 4; 16 ]; [ 6; 36 ]; [ 8; 64 ] ]) (rows db "sq")

[<Fact>]
let ``fibonacci`` () =
    let db = Engine.run "fib(0, 0). fib(1, 1). fib(N, F) :- fib(A, F1), fib(B, F2), B = A + 1, N = B + 1, N <= 30, F = F1 + F2."
    Assert.Contains([| Int 30L; Int 832040L |], rows db "fib")

[<Fact>]
let ``assignment can bind on the right hand side`` () =
    let db = Engine.run "n(3). d(Y) :- n(X), X * 2 = Y."
    assertRows (ints [ [ 6 ] ]) (rows db "d")

[<Fact>]
let ``comparisons order integers before symbols`` () =
    let db = Engine.run "v(1). v(a). v(\"B\"). v(-5). big(X) :- v(X), X > 0."
    assertRows [ [| Int 1L |]; [| Sym "B" |]; [| Sym "a" |] ] (rows db "big")

[<Fact>]
let ``zero-arity predicates`` () =
    let db = Engine.run "e(1). nonempty :- e(_). empty :- e(2)."
    Assert.Equal(1, (rows db "nonempty").Length)
    Assert.Empty(rows db "empty")

[<Fact>]
let ``undefined predicates are empty and produce a warning`` () =
    let db = Engine.run "p(X) :- q(X)."
    Assert.Empty(rows db "p")
    Assert.Equal<string list>([ "predicate 'q/1' is used but never defined" ], db.Warnings)

[<Fact>]
let ``division by zero is reported`` () =
    failsWith "division by zero" (fun () -> Engine.run "n(0). r(Y) :- n(X), Y = 10 / X.")

[<Fact>]
let ``overflow is reported`` () =
    failsWith "integer overflow" (fun () -> Engine.run "n(9223372036854775807). r(Y) :- n(X), Y = X + 1.")

[<Fact>]
let ``arithmetic on symbols is reported`` () =
    failsWith "arithmetic needs integers" (fun () -> Engine.run "n(a). r(Y) :- n(X), Y = X + 1.")

[<Fact>]
let ``queries return named variables in order of appearance`` () =
    let db = Engine.run "e(1,2). e(1,3). e(2,3)."
    let a = Engine.queryText db "e(X, Y), Y > X, X = 1"
    Assert.Equal<string list>([ "X"; "Y" ], a.Columns)
    assertRows (ints [ [ 1; 2 ]; [ 1; 3 ] ]) a.Rows

[<Fact>]
let ``ground queries return one empty row when true`` () =
    let db = Engine.run "e(1,2)."
    Assert.Equal(1, (Engine.queryText db "e(1, 2)").Rows.Length)
    Assert.Empty((Engine.queryText db "e(2, 1)").Rows)

[<Fact>]
let ``queries hide underscore variables and deduplicate`` () =
    let db = Engine.run "e(1,2). e(1,3)."
    let a = Engine.queryText db "e(X, _Y)"
    Assert.Equal<string list>([ "X" ], a.Columns)
    Assert.Equal(1, a.Rows.Length)

[<Fact>]
let ``queries over unknown predicates are empty`` () =
    let db = Engine.run "e(1)."
    Assert.Empty((Engine.queryText db "nope(X)").Rows)

[<Fact>]
let ``query arity mismatch is an error`` () =
    let db = Engine.run "e(1)."
    failsWith "has 1 argument(s) but the query uses 2" (fun () -> Engine.queryText db "e(X, Y)")

[<Fact>]
let ``planner result does not depend on literal order`` () =
    let a = Engine.run "e(1,2). e(2,3). f(3). r(X) :- e(X,Y), e(Y,Z), f(Z), Z > X."
    let b = Engine.run "e(1,2). e(2,3). f(3). r(X) :- Z > X, f(Z), e(Y,Z), e(X,Y)."
    assertRows (rows a "r") (rows b "r")
    assertRows (ints [ [ 1 ] ]) (rows a "r")

[<Fact>]
let ``negation is rejected for now`` () =
    failsWith "negation is not supported yet" (fun () -> Engine.run "a(1). b(X) :- a(X), not c(X).")

[<Fact>]
let ``aggregates are rejected for now`` () =
    failsWith "aggregates are not supported yet" (fun () -> Engine.run "a(1). n(count<X>) :- a(X).")
