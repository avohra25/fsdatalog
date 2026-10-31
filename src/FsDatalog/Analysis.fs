namespace FsDatalog

/// Static checks run before evaluation: arity consistency, range restriction
/// (safety) and aggregate well-formedness. Errors raise DatalogError;
/// non-fatal findings are returned as warnings.
module Analysis =
    let private fail line msg = raise (DatalogError(sprintf "line %d: %s" line msg))

    /// The variables a body binds: everything in a positive atom, plus any
    /// variable equated (`X = expr`) to an expression whose variables are bound.
    let boundVars (body: Literal list) : Set<string> =
        let mutable bound =
            body |> List.choose (function Pos a -> Some(Syntax.atomVars a) | _ -> None) |> Set.unionMany
        let mutable changed = true
        while changed do
            changed <- false
            for lit in body do
                match lit with
                | Cmp (Eq, ETerm (Var v), e)
                | Cmp (Eq, e, ETerm (Var v)) when not (bound.Contains v) && Set.isSubset (Syntax.exprVars e) bound ->
                    bound <- bound.Add v
                    changed <- true
                | _ -> ()
        bound

    let private firstMissing (vars: Set<string>) (bound: Set<string>) =
        Set.difference vars bound |> Seq.tryHead

    let private describeVar v = if Names.isWildcard v then "'_'" else sprintf "'%s'" v

    /// Checks that every literal of a body can be evaluated with finitely many
    /// bindings, returning the bound variables.
    let checkBody line (body: Literal list) : Set<string> =
        let bound = boundVars body
        for lit in body do
            match lit with
            | Pos _ -> ()
            | Neg a ->
                let named = Syntax.atomVars a |> Set.filter (Names.isWildcard >> not)
                match firstMissing named bound with
                | Some v ->
                    fail line (sprintf "unsafe rule: variable %s in '%s' must also appear in a positive literal" (describeVar v) (Pretty.literal lit))
                | None -> ()
            | Cmp _ ->
                match firstMissing (Syntax.literalVars lit) bound with
                | Some v ->
                    fail line (sprintf "unsafe rule: variable %s in '%s' is not bound by a positive literal" (describeVar v) (Pretty.literal lit))
                | None -> ()
        bound

    let checkRule (r: Rule) =
        let bound = checkBody r.Line r.Body
        let groupVars = ResizeArray()
        for arg in r.Head.HArgs do
            match arg with
            | HTerm (Var v) when Names.isWildcard v -> fail r.Line "'_' is not allowed in a rule head"
            | HTerm (Var v) ->
                if not (bound.Contains v) then
                    if Syntax.isFact r then
                        fail r.Line (sprintf "fact '%s' must not contain variables (found '%s')" (Pretty.head r.Head) v)
                    else
                        fail r.Line (sprintf "unsafe rule: head variable '%s' is not bound in the body" v)
                groupVars.Add v
            | HTerm (Const _) -> ()
            | HAgg (f, v) ->
                if Syntax.isFact r then fail r.Line "aggregates need a rule body"
                if not (bound.Contains v) then
                    fail r.Line (sprintf "aggregated variable '%s' in %s<%s> is not bound in the body" v (Pretty.aggName f) v)
        for arg in r.Head.HArgs do
            match arg with
            | HAgg (_, v) when groupVars.Contains v ->
                fail r.Line (sprintf "variable '%s' cannot be both aggregated and grouped by" v)
            | _ -> ()

    /// Every predicate with its arity, checking that uses agree.
    let arities (p: Program) : Map<string, int> =
        let mutable seen = Map.empty
        let note pred arity line =
            match Map.tryFind pred seen with
            | Some (a, firstLine) when a <> arity ->
                fail line (sprintf "predicate '%s' is used with %d argument(s) here but with %d on line %d" pred arity a firstLine)
            | Some _ -> ()
            | None -> seen <- seen.Add(pred, (arity, line))
        let noteBody line body =
            for lit in body do
                match lit with
                | Pos a | Neg a -> note a.Pred a.Args.Length line
                | Cmp _ -> ()
        for r in p.Rules do
            note r.Head.HPred r.Head.HArgs.Length r.Line
            noteBody r.Line r.Body
        for q in p.Queries do
            noteBody q.QLine q.Goals
        seen |> Map.map (fun _ (a, _) -> a)

    /// Runs all checks; returns warnings (e.g. predicates that are used but never defined).
    let check (p: Program) : string list =
        let ar = arities p
        p.Rules |> List.iter checkRule
        p.Queries |> List.iter (fun q -> checkBody q.QLine q.Goals |> ignore)
        let defined = p.Rules |> List.map (fun r -> r.Head.HPred) |> Set.ofList
        ar
        |> Map.toList
        |> List.filter (fun (pred, _) -> not (defined.Contains pred))
        |> List.map (fun (pred, a) -> sprintf "predicate '%s/%d' is used but never defined" pred a)
