namespace FsDatalog

/// Recursive-descent parser for the Datalog surface syntax.
///
///   program   := (clause | query)*
///   clause    := head (':-' body)? '.'
///   query     := '?-' body '.'
///   head      := ident ('(' headArg (',' headArg)* ')')?
///   headArg   := ('count'|'sum'|'min'|'max') '<' Var '>' | term
///   body      := literal (',' literal)*
///   literal   := 'not' atom | atom | expr cmpOp expr
///   expr      := mul (('+'|'-') mul)*
///   mul       := unary (('*'|'/'|'mod') unary)*
///   unary     := '-' unary | term | '(' expr ')'
module Parser =
    type private State(tokens: Token[]) =
        let mutable pos = 0
        let mutable wildcards = 0
        member _.Peek = tokens.[pos]
        member _.PeekAt k = tokens.[min (pos + k) (tokens.Length - 1)]
        member _.Next() =
            let t = tokens.[pos]
            if pos < tokens.Length - 1 then pos <- pos + 1
            t
        member _.FreshWildcard() =
            wildcards <- wildcards + 1
            sprintf "_#%d" wildcards

    let private failAt (t: Token) msg =
        raise (DatalogError(sprintf "line %d, col %d: %s" t.Line t.Col msg))

    let private expect (st: State) tok what =
        let t = st.Next()
        if t.Tok <> tok then failAt t (sprintf "expected %s but found %s" what (Lexer.describe t.Tok))

    let private aggOf name =
        match name with
        | "count" -> Some Count
        | "sum" -> Some Sum
        | "min" -> Some Min
        | "max" -> Some Max
        | _ -> None

    let private varTerm (st: State) name =
        if name = "_" then Var(st.FreshWildcard()) else Var name

    let private term (st: State) : Term =
        let t = st.Next()
        match t.Tok with
        | TVar v -> varTerm st v
        | TIdent s -> Const(Sym s)
        | TStr s -> Const(Sym s)
        | TInt n -> Const(Int n)
        | TOp "-" ->
            match st.Next() with
            | { Tok = TInt n } -> Const(Int(-n))
            | other -> failAt other "expected an integer after '-'"
        | other -> failAt t (sprintf "expected a term but found %s" (Lexer.describe other))

    let private args (st: State) (item: State -> 'a) : 'a list =
        match st.Peek.Tok with
        | TLParen ->
            st.Next() |> ignore
            if st.Peek.Tok = TRParen then failAt st.Peek "empty argument list; write 'p' instead of 'p()'"
            let items = ResizeArray [ item st ]
            while st.Peek.Tok = TComma do
                st.Next() |> ignore
                items.Add(item st)
            expect st TRParen "')'"
            List.ofSeq items
        | _ -> []

    let private atom (st: State) : Atom =
        match st.Next() with
        | { Tok = TIdent p } -> { Pred = p; Args = args st term }
        | t -> failAt t (sprintf "expected a predicate name but found %s" (Lexer.describe t.Tok))

    let rec private expr (st: State) : Expr =
        let mutable left = mul st
        let mutable go = true
        while go do
            match st.Peek.Tok with
            | TOp "+" ->
                st.Next() |> ignore
                left <- EBin(Add, left, mul st)
            | TOp "-" ->
                st.Next() |> ignore
                left <- EBin(Sub, left, mul st)
            | _ -> go <- false
        left

    and private mul (st: State) : Expr =
        let mutable left = unary st
        let mutable go = true
        while go do
            match st.Peek.Tok with
            | TOp "*" ->
                st.Next() |> ignore
                left <- EBin(Mul, left, unary st)
            | TOp "/" ->
                st.Next() |> ignore
                left <- EBin(Div, left, unary st)
            | TIdent "mod" ->
                st.Next() |> ignore
                left <- EBin(Mod, left, unary st)
            | _ -> go <- false
        left

    and private unary (st: State) : Expr =
        match st.Peek.Tok with
        | TOp "-" ->
            match (st.PeekAt 1).Tok with
            | TInt _ -> ETerm(term st)
            | _ ->
                st.Next() |> ignore
                EBin(Sub, ETerm(Const(Int 0L)), unary st)
        | TLParen ->
            st.Next() |> ignore
            let e = expr st
            expect st TRParen "')'"
            e
        | _ -> ETerm(term st)

    let private cmpOp (st: State) =
        let t = st.Next()
        match t.Tok with
        | TOp "=" -> Eq
        | TOp "!=" -> Ne
        | TOp "<" -> Lt
        | TOp "<=" -> Le
        | TOp ">" -> Gt
        | TOp ">=" -> Ge
        | other -> failAt t (sprintf "expected a comparison operator but found %s" (Lexer.describe other))

    let private literal (st: State) : Literal =
        match st.Peek.Tok, (st.PeekAt 1).Tok with
        | TIdent "not", TIdent _ ->
            st.Next() |> ignore
            Neg(atom st)
        | TIdent _, TLParen
        | TIdent _, TComma
        | TIdent _, TDot -> Pos(atom st)
        | _ ->
            let l = expr st
            let op = cmpOp st
            let r = expr st
            Cmp(op, l, r)

    let private body (st: State) =
        let lits = ResizeArray [ literal st ]
        while st.Peek.Tok = TComma do
            st.Next() |> ignore
            lits.Add(literal st)
        List.ofSeq lits

    let private headArg (st: State) : HeadTerm =
        match st.Peek.Tok, (st.PeekAt 1).Tok with
        | TIdent name, TOp "<" when (aggOf name).IsSome ->
            st.Next() |> ignore
            st.Next() |> ignore
            let v =
                match st.Next() with
                | { Tok = TVar v } when v <> "_" -> v
                | t -> failAt t (sprintf "expected a named variable inside %s<...>" name)
            expect st (TOp ">") "'>'"
            HAgg((aggOf name).Value, v)
        | _ -> HTerm(term st)

    type private Stmt =
        | SRule of Rule
        | SQuery of Query

    let private statement (st: State) =
        let first = st.Peek
        match first.Tok with
        | TQuery ->
            st.Next() |> ignore
            let goals = body st
            expect st TDot "'.' at the end of the query"
            SQuery { Goals = goals; QLine = first.Line }
        | TIdent p ->
            st.Next() |> ignore
            let head = { HPred = p; HArgs = args st headArg }
            let b =
                match st.Peek.Tok with
                | TImplies ->
                    st.Next() |> ignore
                    body st
                | _ -> []
            expect st TDot "'.' at the end of the clause"
            SRule { Head = head; Body = b; Line = first.Line }
        | other -> failAt first (sprintf "expected a clause or query but found %s" (Lexer.describe other))

    /// Parses a whole program. Queries are kept in source order.
    let parse (src: string) : Program =
        let st = State(Lexer.tokenize src)
        let rules = ResizeArray()
        let queries = ResizeArray()
        while st.Peek.Tok <> TEof do
            match statement st with
            | SRule r -> rules.Add r
            | SQuery q -> queries.Add q
        { Rules = List.ofSeq rules; Queries = List.ofSeq queries }

    /// Parses a single query body such as `path(1, X), X > 2` (the `?-` and
    /// trailing `.` are optional).
    let parseQuery (src: string) : Query =
        let text = src.Trim()
        let text = if text.StartsWith "?-" then text else "?- " + text
        let text = if text.EndsWith "." then text else text + "."
        match (parse text).Queries with
        | [ q ] -> q
        | _ -> raise (DatalogError "expected exactly one query")
