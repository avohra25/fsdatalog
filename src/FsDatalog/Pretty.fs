namespace FsDatalog

open System.Text.RegularExpressions

/// Renders the AST back to source text that parses to the same AST.
module Pretty =
    let private bareSymbol = Regex(@"^[a-z][A-Za-z0-9_]*$", RegexOptions.Compiled)
    let private reserved = set [ "not"; "mod" ]

    let value v =
        match v with
        | Int n -> string n
        | Sym s when bareSymbol.IsMatch s && not (reserved.Contains s) -> s
        | Sym s ->
            let escaped =
                s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n").Replace("\t", "\\t")
            "\"" + escaped + "\""

    let term t =
        match t with
        | Var v when Names.isWildcard v -> "_"
        | Var v -> v
        | Const c -> value c

    let opText op =
        match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Mod -> "mod"

    let rec expr e =
        match e with
        | ETerm t -> term t
        | EBin (op, a, b) ->
            let wrap x =
                match x with
                | EBin _ -> "(" + expr x + ")"
                | _ -> expr x
            sprintf "%s %s %s" (wrap a) (opText op) (wrap b)

    let cmpOp op =
        match op with
        | Eq -> "="
        | Ne -> "!="
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">="

    let private argList (items: string list) =
        if List.isEmpty items then "" else "(" + String.concat ", " items + ")"

    let atom (a: Atom) = a.Pred + argList (List.map term a.Args)

    let literal lit =
        match lit with
        | Pos a -> atom a
        | Neg a -> "not " + atom a
        | Cmp (op, l, r) -> sprintf "%s %s %s" (expr l) (cmpOp op) (expr r)

    let aggName f =
        match f with
        | Count -> "count"
        | Sum -> "sum"
        | Min -> "min"
        | Max -> "max"

    let head (h: Head) =
        let arg a =
            match a with
            | HTerm t -> term t
            | HAgg (f, v) -> sprintf "%s<%s>" (aggName f) v
        h.HPred + argList (List.map arg h.HArgs)

    let rule (r: Rule) =
        match r.Body with
        | [] -> head r.Head + "."
        | body -> head r.Head + " :- " + String.concat ", " (List.map literal body) + "."

    let query (q: Query) = "?- " + String.concat ", " (List.map literal q.Goals) + "."

    let program (p: Program) =
        (List.map rule p.Rules @ List.map query p.Queries) |> String.concat "\n"

    let tuple (pred: string) (t: Value[]) =
        pred + argList (t |> Array.map value |> List.ofArray)
