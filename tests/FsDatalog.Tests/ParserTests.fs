module ParserTests

open Xunit
open FsDatalog

let parseOne src =
    match (Parser.parse src).Rules with
    | [ r ] -> r
    | rs -> failwithf "expected one rule, got %d" rs.Length

let throwsWith (fragment: string) (f: unit -> 'a) =
    try
        f () |> ignore
        failwithf "expected an error containing '%s'" fragment
    with DatalogError msg -> Assert.Contains(fragment, msg)

[<Fact>]
let ``parses facts with symbols, strings and integers`` () =
    let r = parseOne "edge(a, \"New York\", -42)."
    Assert.Equal("edge", r.Head.HPred)
    Assert.Equal<HeadTerm list>([ HTerm(Const(Sym "a")); HTerm(Const(Sym "New York")); HTerm(Const(Int -42L)) ], r.Head.HArgs)
    Assert.True(Syntax.isFact r)

[<Fact>]
let ``parses a recursive rule`` () =
    let r = parseOne "path(X, Z) :- path(X, Y), edge(Y, Z)."
    Assert.Equal<Literal list>(
        [ Pos { Pred = "path"; Args = [ Var "X"; Var "Y" ] }; Pos { Pred = "edge"; Args = [ Var "Y"; Var "Z" ] } ],
        r.Body
    )

[<Fact>]
let ``zero-arity predicates`` () =
    let p = Parser.parse "go. done :- go, not stop."
    Assert.Equal(2, p.Rules.Length)
    Assert.Equal<Literal list>([ Pos { Pred = "go"; Args = [] }; Neg { Pred = "stop"; Args = [] } ], p.Rules.[1].Body)

[<Fact>]
let ``negation, comparisons and arithmetic precedence`` () =
    let r = parseOne "p(X, Y) :- q(X), not r(X), Y = X * 2 + 1, Y != 7, X <= 10."
    match r.Body with
    | [ Pos _; Neg { Pred = "r" }; Cmp (Eq, ETerm (Var "Y"), EBin (Add, EBin (Mul, ETerm (Var "X"), ETerm (Const (Int 2L))), ETerm (Const (Int 1L)))); Cmp (Ne, _, _); Cmp (Le, _, _) ] -> ()
    | other -> failwithf "unexpected body %A" other

[<Fact>]
let ``parenthesised expressions, unary minus and mod`` () =
    let r = parseOne "p(Z) :- q(X), Z = -(X - 3) mod 4."
    match r.Body.[1] with
    | Cmp (Eq, _, EBin (Mod, EBin (Sub, ETerm (Const (Int 0L)), EBin (Sub, _, _)), ETerm (Const (Int 4L)))) -> ()
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``wildcards become distinct hidden variables`` () =
    let r = parseOne "p(X) :- e(X, _), e(_, X)."
    match r.Body with
    | [ Pos { Args = [ _; Var a ] }; Pos { Args = [ Var b; _ ] } ] ->
        Assert.True(Names.isWildcard a && Names.isWildcard b)
        Assert.NotEqual<string>(a, b)
    | other -> failwithf "unexpected %A" other

[<Fact>]
let ``aggregates in heads`` () =
    let r = parseOne "stats(D, count<E>, sum<S>, min<S>, max<S>) :- emp(E, D, S)."
    Assert.Equal<HeadTerm list>(
        [ HTerm(Var "D"); HAgg(Count, "E"); HAgg(Sum, "S"); HAgg(Min, "S"); HAgg(Max, "S") ],
        r.Head.HArgs
    )

[<Fact>]
let ``count is an ordinary symbol outside aggregate position`` () =
    let r = parseOne "p(count) :- q(count)."
    Assert.Equal<HeadTerm list>([ HTerm(Const(Sym "count")) ], r.Head.HArgs)

[<Fact>]
let ``queries are collected separately`` () =
    let p = Parser.parse "e(1,2). ?- e(1, X). ?- e(X, Y), X < Y."
    Assert.Equal(1, p.Rules.Length)
    Assert.Equal(2, p.Queries.Length)
    Assert.Equal(1, p.Queries.[0].QLine)

[<Fact>]
let ``comments of all three styles are skipped`` () =
    let p = Parser.parse "% line\n// another\n/* block\n comment */ e(1). /* trailing */"
    Assert.Equal(1, p.Rules.Length)
    Assert.Equal(4, p.Rules.[0].Line)

[<Fact>]
let ``string escapes`` () =
    let r = parseOne "s(\"a\\\"b\\\\c\\nd\")."
    Assert.Equal<HeadTerm list>([ HTerm(Const(Sym "a\"b\\c\nd")) ], r.Head.HArgs)

[<Fact>]
let ``parseQuery accepts bare bodies`` () =
    let q = Parser.parseQuery "path(1, X), X > 2"
    Assert.Equal(2, q.Goals.Length)

[<Theory>]
[<InlineData("p(X :- q(X).", "line 1, col 5: expected ')'")>]
[<InlineData("p(X) :- q(X)", "expected '.' at the end of the clause")>]
[<InlineData("p(\"open).", "unterminated string literal")>]
[<InlineData("p(1) /* never closed", "unterminated block comment")>]
[<InlineData("p(99999999999999999999).", "out of range")>]
[<InlineData("p() .", "empty argument list")>]
[<InlineData("p(X) :- X.", "expected a comparison operator")>]
[<InlineData("p(1).\n  #", "line 2, col 3: unexpected character '#'")>]
[<InlineData("p(X, count<_>) :- q(X).", "named variable inside count")>]
let ``syntax errors report position and cause`` (src: string, fragment: string) =
    throwsWith fragment (fun () -> Parser.parse src)
