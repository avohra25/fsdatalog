namespace FsDatalog

/// Raised for every user-facing error: syntax errors, unsafe rules,
/// arity mismatches, unstratifiable programs and runtime arithmetic errors.
exception DatalogError of string

/// A ground value. Integers sort before symbols; symbols sort ordinally.
type Value =
    | Int of int64
    | Sym of string

type Term =
    | Var of string
    | Const of Value

type ArithOp =
    | Add
    | Sub
    | Mul
    | Div
    | Mod

type Expr =
    | ETerm of Term
    | EBin of ArithOp * Expr * Expr

type CmpOp =
    | Eq
    | Ne
    | Lt
    | Le
    | Gt
    | Ge

type Atom = { Pred: string; Args: Term list }

type Literal =
    | Pos of Atom
    | Neg of Atom
    | Cmp of CmpOp * Expr * Expr

type AggFn =
    | Count
    | Sum
    | Min
    | Max

type HeadTerm =
    | HTerm of Term
    /// An aggregate over a body variable, e.g. `count<Y>`.
    | HAgg of AggFn * string

type Head = { HPred: string; HArgs: HeadTerm list }

type Rule = { Head: Head; Body: Literal list; Line: int }

type Query = { Goals: Literal list; QLine: int }

type Program = { Rules: Rule list; Queries: Query list }

module Names =
    /// Wildcards (`_`) are renamed by the parser to `_#<n>` so every
    /// occurrence is a distinct variable that can never clash with user names.
    let isWildcard (v: string) = v.StartsWith "_#"

    /// Variables starting with `_` are never reported in query answers.
    let isHidden (v: string) = v.StartsWith "_"

module Syntax =
    let rec exprVars e =
        match e with
        | ETerm (Var v) -> Set.singleton v
        | ETerm (Const _) -> Set.empty
        | EBin (_, a, b) -> Set.union (exprVars a) (exprVars b)

    let atomVars (a: Atom) =
        a.Args |> List.choose (function Var v -> Some v | _ -> None) |> Set.ofList

    let literalVars lit =
        match lit with
        | Pos a | Neg a -> atomVars a
        | Cmp (_, l, r) -> Set.union (exprVars l) (exprVars r)

    let headVars (h: Head) =
        h.HArgs
        |> List.choose (function
            | HTerm (Var v) -> Some v
            | HAgg (_, v) -> Some v
            | _ -> None)
        |> Set.ofList

    let isFact (r: Rule) = List.isEmpty r.Body

    let hasAggregate (h: Head) =
        h.HArgs |> List.exists (function HAgg _ -> true | _ -> false)
