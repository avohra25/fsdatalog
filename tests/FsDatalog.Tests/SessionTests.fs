module SessionTests

open System.IO
open Xunit
open FsDatalog

[<Fact>]
let ``answers are printed per row with a count`` () =
    let s = Session()
    Assert.Empty(s.Execute "e(1,2). e(1,3).")
    Assert.Equal<string list>([ "X = 2"; "X = 3"; "2 answers." ], s.Execute "?- e(1, X).")
    Assert.Equal<string list>([ "Y = 2"; "1 answer." ], s.Execute "?- e(1, Y), Y < 3.")
    Assert.Equal<string list>([ "no answers." ], s.Execute "?- e(5, X).")

[<Fact>]
let ``ground queries answer yes or no`` () =
    let s = Session()
    s.Execute "e(1,2)." |> ignore
    Assert.Equal<string list>([ "yes." ], s.Execute "?- e(1, 2).")
    Assert.Equal<string list>([ "no." ], s.Execute "?- e(2, 1).")

[<Fact>]
let ``clauses accumulate and re-evaluate`` () =
    let s = Session()
    s.Execute "e(1,2). p(X,Y) :- e(X,Y). p(X,Z) :- p(X,Y), e(Y,Z)." |> ignore
    Assert.Equal<string list>([ "Y = 2"; "1 answer." ], s.Execute "?- p(1, Y).")
    s.Execute "e(2,3)." |> ignore
    Assert.Equal<string list>([ "Y = 2"; "Y = 3"; "2 answers." ], s.Execute "?- p(1, Y).")

[<Fact>]
let ``a bad clause leaves the session unchanged`` () =
    let s = Session()
    s.Execute "e(1). p(X) :- e(X)." |> ignore
    let err = s.Execute "q(X) :- e(X), not q(X)."
    Assert.StartsWith("error: ", List.exactlyOne err)
    Assert.Contains("not stratifiable", List.exactlyOne err)
    Assert.Equal(2, s.Rules.Length)
    Assert.Equal<string list>([ "yes." ], s.Execute "?- p(1).")

[<Fact>]
let ``syntax errors are reported`` () =
    let s = Session()
    Assert.StartsWith("error: ", List.exactlyOne (s.Execute "p(X :- q."))

[<Fact>]
let ``warnings for undefined predicates`` () =
    let s = Session()
    Assert.Equal<string list>([ "warning: predicate 'q/1' is used but never defined" ], s.Execute "p(X) :- q(X).")

[<Fact>]
let ``rules, dump and strata commands`` () =
    let s = Session()
    s.Execute "e(1,2). r(X) :- e(X, _). s(X) :- e(_, X), not r(X)." |> ignore
    Assert.Equal<string list>([ "e(1, 2)."; "r(X) :- e(X, _)."; "s(X) :- e(_, X), not r(X)." ], s.Execute ":rules")
    Assert.Equal<string list>([ "e(1, 2)."; "r(1)."; "s(2)." ], s.Execute ":dump")
    Assert.Equal<string list>([ "s(2)." ], s.Execute ":dump s")
    Assert.Equal<string list>([ "0: e"; "0: r"; "1: s" ], s.Execute ":strata")
    Assert.Contains("2 strata", List.exactlyOne (s.Execute ":stats"))

[<Fact>]
let ``clear and unknown commands`` () =
    let s = Session()
    s.Execute "e(1)." |> ignore
    Assert.Equal<string list>([ "cleared." ], s.Execute ":clear")
    Assert.Equal<string list>([ "% no clauses" ], s.Execute ":rules")
    Assert.Equal<string list>([ "error: unknown command :nope (try :help)" ], s.Execute ":nope")
    Assert.NotEmpty(s.Execute ":help")

[<Fact>]
let ``load reads a file`` () =
    let path = Path.GetTempFileName()
    File.WriteAllText(path, "e(1). e(2). ?- e(X).")
    try
        let s = Session()
        Assert.Equal<string list>([ "X = 1"; "X = 2"; "2 answers." ], s.Execute(":load " + path))
    finally
        File.Delete path

let example name =
    let path = Path.Combine(System.AppContext.BaseDirectory, "examples", name)
    Session().Execute(File.ReadAllText path)

[<Fact>]
let ``example: graph`` () =
    let out = example "graph.dl"
    Assert.Equal<string list>([ "X = a"; "X = b"; "X = c"; "3 answers."; "X = d"; "1 answer."; "X = a"; "X = b"; "X = c"; "X = e"; "4 answers." ], out)

[<Fact>]
let ``example: family`` () =
    Assert.Equal<string list>([ "X = gus"; "1 answer."; "P = ann, N = 6"; "1 answer." ], example "family.dl")

[<Fact>]
let ``example: bill of materials`` () =
    let out = example "bom.dl"
    Assert.Contains("P = spoke, N = 64", out)
    Assert.Contains("P = bearing, N = 4", out)
    Assert.Contains("N = 6", out)

[<Fact>]
let ``example: shortest paths`` () =
    let out = example "shortest.dl"
    Assert.Contains("Y = berk, D = 17", out)
    Assert.Contains("Y = rich, D = 26", out)

[<Fact>]
let ``example: points-to`` () =
    Assert.Equal<string list>([ "O = b"; "1 answer." ], example "pointsto.dl" |> List.take 2)

[<Fact>]
let ``example: unstratifiable game is rejected`` () =
    Assert.Contains("not stratifiable", List.exactlyOne (example "game.dl"))
