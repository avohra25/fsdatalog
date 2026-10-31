/// Random programs are evaluated by the engine and by the naive reference
/// evaluator in Naive.fs; the two must agree on every relation.
module DifferentialTests

open System
open Xunit
open FsDatalog

type Mode =
    | Positive
    | WithNegation
    | WithAggregates

let edb = [ "e", 2; "f", 1; "g", 2 ]

/// Generates a program over a small constant domain. IDB predicates are
/// p0..p3; negation and aggregates only refer to EDB predicates or to IDB
/// predicates with a lower index, so every program is stratifiable.
let generate (rng: Random) (mode: Mode) : string =
    let sb = Text.StringBuilder()
    let constant () = string (rng.Next 5)
    for (p, n) in edb do
        for _ in 1 .. rng.Next(3, 9) do
            let args = List.init n (fun _ -> constant ()) |> String.concat ","
            sb.AppendFormat("{0}({1}).\n", p, args) |> ignore
    let idb = [ for i in 0..3 -> sprintf "p%d" i, 1 + rng.Next 2 ]
    let vars = [| "X"; "Y"; "Z"; "W" |]
    let randomArgs n = List.init n (fun _ -> if rng.Next 6 = 0 then constant () else vars.[rng.Next 3])
    for i, (p, arity) in List.indexed idb do
        let visible = edb @ (idb |> List.take (i + 1))
        let lower = edb @ (idb |> List.take i)
        for _ in 1 .. rng.Next(1, 3) do
            let positives =
                [ for _ in 1 .. rng.Next(1, 3) ->
                      let q, n = visible.[rng.Next visible.Length]
                      q, randomArgs n ]
            let bound = positives |> List.collect snd |> List.filter (fun a -> Char.IsUpper a.[0]) |> List.distinct
            if bound.IsEmpty then () else
            let pick () = bound.[rng.Next bound.Length]
            let extra =
                [ if rng.Next 3 = 0 then yield sprintf "%s != %s" (pick ()) (pick ())
                  if rng.Next 4 = 0 then yield sprintf "%s <= %s" (pick ()) (constant ())
                  if mode = WithNegation && rng.Next 2 = 0 then
                      let q, n = lower.[rng.Next lower.Length]
                      let args = List.init n (fun _ -> if rng.Next 3 = 0 then constant () else pick ())
                      yield sprintf "not %s(%s)" q (String.concat ", " args) ]
            let body =
                (positives |> List.map (fun (q, args) -> sprintf "%s(%s)" q (String.concat ", " args))) @ extra
                |> List.sortBy (fun _ -> rng.Next())
                |> String.concat ", "
            let head = List.init arity (fun _ -> pick ()) |> String.concat ", "
            sb.AppendFormat("{0}({1}) :- {2}.\n", p, head, body) |> ignore
    if mode = WithAggregates then
        // aggregate heads over lower strata (EDB and the p's), feeding one more rule
        let fns = [| "count"; "sum"; "min"; "max" |]
        for k in 0..2 do
            let q, n = (edb @ idb).[rng.Next(edb.Length + idb.Length)]
            if n = 2 then
                sb.AppendFormat("a{0}(X, {1}<Y>) :- {2}(X, Y).\n", k, fns.[rng.Next fns.Length], q) |> ignore
            else
                sb.AppendFormat("a{0}(0, {1}<X>) :- {2}(X).\n", k, fns.[rng.Next fns.Length], q) |> ignore
        sb.Append("u(X) :- a0(X, _).\nu(X) :- a1(X, _).\n") |> ignore
    sb.ToString()

let compare (src: string) =
    let program = Parser.parse src
    let db = Engine.evaluate program
    let derived = program.Rules |> List.filter (Syntax.isFact >> not) |> List.map (fun r -> r.Head.HPred) |> List.distinct
    let strata = [ derived ]
    let reference = Naive.evaluate strata program
    for KeyValue (p, rel) in db.Relations do
        let expected = Naive.rel reference p |> Set.map Array.ofList
        let actual = rel.Rows |> Seq.map id |> Set.ofSeq
        if expected <> actual then
            failwithf "relation %s differs\nprogram:\n%s\nexpected %A\nactual %A" p src expected actual

let run mode seeds =
    let mutable checkedPrograms = 0
    for seed in seeds do
        let src = generate (Random seed) mode
        try
            compare src
            checkedPrograms <- checkedPrograms + 1
        with DatalogError msg when msg.Contains "used with" ->
            // a generated rule may reuse a predicate with another arity; skip those programs
            ()
    // make sure the generator is not mostly producing rejected programs
    Assert.InRange(checkedPrograms, List.length seeds * 2 / 3, List.length seeds)

[<Fact>]
let ``positive programs agree with the naive evaluator`` () = run Positive [ 1..300 ]

[<Fact>]
let ``shuffling body literals does not change the result`` () =
    let rng = Random 42
    for seed in 1..100 do
        let program = Parser.parse (generate (Random seed) Positive)
        let shuffled =
            { program with
                Rules = program.Rules |> List.map (fun r -> { r with Body = r.Body |> List.sortBy (fun _ -> rng.Next()) }) }
        let a = Engine.evaluate program
        let b = Engine.evaluate shuffled
        for KeyValue (p, rel) in a.Relations do
            Assert.Equal<Value[] list>(rel.Sorted(), Engine.tuples b p)
