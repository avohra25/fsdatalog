module PrettyTests

open System
open Xunit
open FsDatalog

let roundTrip src =
    let p = Parser.parse src
    let printed = Pretty.program p
    let reparsed = Parser.parse printed
    Assert.Equal<Rule list>(p.Rules |> List.map (fun r -> { r with Line = 0 }), reparsed.Rules |> List.map (fun r -> { r with Line = 0 }))
    printed

[<Fact>]
let ``prints canonical source`` () =
    let printed = roundTrip "p( X,Y ):-q(X , Y),not r( Y ),X<Y+1 ."
    Assert.Equal("p(X, Y) :- q(X, Y), not r(Y), X < Y + 1.", printed)

[<Fact>]
let ``quotes symbols that are not bare identifiers`` () =
    Assert.Equal("\"New York\"", Pretty.value (Sym "New York"))
    Assert.Equal("\"Upper\"", Pretty.value (Sym "Upper"))
    Assert.Equal("\"not\"", Pretty.value (Sym "not"))
    Assert.Equal("abc_1", Pretty.value (Sym "abc_1"))
    Assert.Equal("\"a\\\"b\"", Pretty.value (Sym "a\"b"))

[<Fact>]
let ``nested arithmetic keeps its shape`` () =
    roundTrip "p(Z) :- q(X), Z = (X - 1) - (X * (X mod 3)) / -2." |> ignore

// ---------- randomized round-trip ----------

let private rnd = Random(7)

let private genValue () =
    match rnd.Next 4 with
    | 0 -> Int(int64 (rnd.Next(-50, 50)))
    | 1 -> Sym([| "a"; "bob"; "x_1" |].[rnd.Next 3])
    | 2 -> Sym([| "Hello world"; "q\"uote"; ""; "tab\t" |].[rnd.Next 4])
    | _ -> Int(int64 (rnd.Next 1000))

let private vars = [| "X"; "Y"; "Z"; "Long_Name" |]
let private genTerm () = if rnd.Next 2 = 0 then Var vars.[rnd.Next vars.Length] else Const(genValue ())

let rec private genExpr depth =
    if depth = 0 || rnd.Next 3 = 0 then ETerm(genTerm ())
    else EBin([| Add; Sub; Mul; Div; Mod |].[rnd.Next 5], genExpr (depth - 1), genExpr (depth - 1))

let private genAtom () =
    { Pred = [| "p"; "edge"; "q2" |].[rnd.Next 3]; Args = List.init (rnd.Next 4) (fun _ -> genTerm ()) }

let private genLiteral () =
    match rnd.Next 3 with
    | 0 -> Pos(genAtom ())
    | 1 -> Neg(genAtom ())
    | _ -> Cmp([| Eq; Ne; Lt; Le; Gt; Ge |].[rnd.Next 6], genExpr 3, genExpr 3)

let private genRule () =
    let args =
        List.init (rnd.Next 4) (fun _ ->
            if rnd.Next 5 = 0 then HAgg([| Count; Sum; Min; Max |].[rnd.Next 4], vars.[rnd.Next vars.Length])
            else HTerm(genTerm ()))
    { Head = { HPred = "h"; HArgs = args }; Body = List.init (rnd.Next 4) (fun _ -> genLiteral ()); Line = 0 }

[<Fact>]
let ``random ASTs survive print then parse`` () =
    for _ in 1..500 do
        let r = genRule ()
        let text = Pretty.rule r
        let back =
            try
                (Parser.parse text).Rules
            with DatalogError msg -> failwithf "could not reparse %s: %s" text msg
        Assert.Equal<Rule list>([ r ], back |> List.map (fun b -> { b with Line = 0 }))
