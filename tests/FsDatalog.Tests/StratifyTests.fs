module StratifyTests

open Xunit
open FsDatalog
open TestUtil

let strata src = Stratify.stratify (Parser.parse src).Rules

let levelOf (ss: Stratum list) p = (ss |> List.find (fun s -> List.contains p s.Preds)).Level

[<Fact>]
let ``positive programs live in level zero`` () =
    let ss = strata "e(1,2). p(X,Y) :- e(X,Y). p(X,Z) :- p(X,Y), e(Y,Z)."
    Assert.True(ss |> List.forall (fun s -> s.Level = 0))
    Assert.True((ss |> List.find (fun s -> s.Preds = [ "p" ])).Recursive)
    Assert.False((ss |> List.find (fun s -> s.Preds = [ "e" ])).Recursive)

[<Fact>]
let ``dependencies come first`` () =
    let ss = strata "c(X) :- b(X). b(X) :- a(X). a(1)."
    let order = ss |> List.collect (fun s -> s.Preds)
    Assert.True(List.findIndex ((=) "a") order < List.findIndex ((=) "b") order)
    Assert.True(List.findIndex ((=) "b") order < List.findIndex ((=) "c") order)

[<Fact>]
let ``mutual recursion forms one stratum`` () =
    let ss = strata "n(0). even(0). even(X) :- n(X), odd(Y), X = Y + 1. odd(X) :- n(X), even(Y), X = Y + 1."
    let s = ss |> List.find (fun s -> List.contains "even" s.Preds)
    Assert.Equal<string list>([ "even"; "odd" ], List.sort s.Preds)

[<Fact>]
let ``negation raises the level along the longest path`` () =
    let ss = strata "a(1). b(X) :- a(X), not c(X). c(X) :- a(X), not d(X). d(X) :- a(X). e(X) :- b(X)."
    Assert.Equal(0, levelOf ss "d")
    Assert.Equal(1, levelOf ss "c")
    Assert.Equal(2, levelOf ss "b")
    Assert.Equal(2, levelOf ss "e")

[<Fact>]
let ``negation through recursion is rejected`` () =
    failsWith "not stratifiable" (fun () -> strata "move(a,b). win(X) :- move(X,Y), not win(Y).")

[<Fact>]
let ``negative cycle across two predicates is rejected`` () =
    failsWith "within the recursive cycle" (fun () -> strata "n(1). p(X) :- n(X), not q(X). q(X) :- n(X), p(X).")
