/// A deliberately simple reference evaluator used for differential testing:
/// substitutions are immutable maps, joins are nested loops over whole
/// relations, and every round re-evaluates every rule from scratch.
module Naive

open FsDatalog

type Subst = Map<string, Value>

let rec evalExpr (s: Subst) e =
    match e with
    | ETerm (Const c) -> Some c
    | ETerm (Var v) -> Map.tryFind v s
    | EBin (op, a, b) ->
        match evalExpr s a, evalExpr s b with
        | Some (Int x), Some (Int y) ->
            match op with
            | Add -> Some(Int(x + y))
            | Sub -> Some(Int(x - y))
            | Mul -> Some(Int(x * y))
            | Div -> if y = 0L then failwith "div by zero" else Some(Int(x / y))
            | Mod -> if y = 0L then failwith "mod by zero" else Some(Int(x % y))
        | Some _, Some _ -> failwith "arith on symbols"
        | _ -> None

let unify (s: Subst) (args: Term list) (t: Value list) : Subst option =
    List.fold2
        (fun acc arg v ->
            match acc with
            | None -> None
            | Some m ->
                match arg with
                | Const c -> if c = v then Some m else None
                | Var x ->
                    match Map.tryFind x m with
                    | Some bound -> if bound = v then Some m else None
                    | None -> Some(Map.add x v m))
        (Some s)
        args
        t

type Db = Map<string, Set<Value list>>

let rel (db: Db) p = Map.tryFind p db |> Option.defaultValue Set.empty

/// All substitutions satisfying a body. Positive atoms are joined in written
/// order; the other literals are applied as soon as they become evaluable.
let solve (db: Db) (body: Literal list) : Subst list =
    let positives = body |> List.choose (function Pos a -> Some a | _ -> None)
    let others = body |> List.filter (function Pos _ -> false | _ -> true)
    let joined =
        positives
        |> List.fold (fun subs a -> subs |> List.collect (fun s -> rel db a.Pred |> Seq.choose (unify s a.Args) |> List.ofSeq)) [ Map.empty ]
    let rec finish (s: Subst) (rest: Literal list) =
        if List.isEmpty rest then Some s
        else
            let ready lit =
                match lit with
                | Neg a -> a.Args |> List.forall (function Var v -> Names.isWildcard v || s.ContainsKey v | _ -> true)
                | Cmp (Eq, ETerm (Var v), e)
                | Cmp (Eq, e, ETerm (Var v)) when not (s.ContainsKey v) -> (evalExpr s e).IsSome
                | Cmp (_, l, r) -> (evalExpr s l).IsSome && (evalExpr s r).IsSome
                | Pos _ -> false
            match List.tryFind ready rest with
            | None -> failwith "naive: cannot finish body"
            | Some lit ->
                let rest' = List.except [ lit ] rest
                match lit with
                | Neg a ->
                    let clash = rel db a.Pred |> Seq.exists (fun t -> (unify s a.Args t).IsSome)
                    if clash then None else finish s rest'
                | Cmp (Eq, ETerm (Var v), e) | Cmp (Eq, e, ETerm (Var v)) when not (s.ContainsKey v) ->
                    finish (s.Add(v, (evalExpr s e).Value)) rest'
                | Cmp (op, l, r) ->
                    let a, b = (evalExpr s l).Value, (evalExpr s r).Value
                    let ok =
                        match op with
                        | Eq -> a = b
                        | Ne -> a <> b
                        | Lt -> a < b
                        | Le -> a <= b
                        | Gt -> a > b
                        | Ge -> a >= b
                    if ok then finish s rest' else None
                | Pos _ -> None
    joined |> List.choose (fun s -> finish s others)

let applyRule (db: Db) (r: Rule) : Set<Value list> =
    let subs = solve db r.Body
    if Syntax.hasAggregate r.Head then
        let projected =
            subs |> List.map (Map.filter (fun k _ -> not (Names.isWildcard k))) |> List.distinct
        projected
        |> List.groupBy (fun s ->
            r.Head.HArgs
            |> List.choose (function
                | HTerm (Var v) -> Some s.[v]
                | HTerm (Const c) -> Some c
                | HAgg _ -> None))
        |> List.map (fun (key, members) ->
            let mutable rest = key
            r.Head.HArgs
            |> List.map (fun arg ->
                match arg with
                | HTerm _ ->
                    let v = List.head rest
                    rest <- List.tail rest
                    v
                | HAgg (Count, _) -> Int(int64 members.Length)
                | HAgg (Sum, v) -> Int(members |> List.sumBy (fun s -> match s.[v] with Int n -> n | _ -> failwith "sum"))
                | HAgg (Min, v) -> members |> List.map (fun s -> s.[v]) |> List.min
                | HAgg (Max, v) -> members |> List.map (fun s -> s.[v]) |> List.max))
        |> Set.ofList
    else
        subs
        |> List.map (fun s ->
            r.Head.HArgs
            |> List.map (function
                | HTerm (Var v) -> s.[v]
                | HTerm (Const c) -> c
                | HAgg _ -> failwith "unreachable"))
        |> Set.ofList

/// Evaluates `program` stratum by stratum (`strata` lists predicate groups in
/// dependency order) using naive iteration.
let evaluate (strata: string list list) (program: Program) : Db =
    let mutable db: Db = Map.empty
    let add p t = db <- db.Add(p, (rel db p).Add t)
    for r in program.Rules do
        if Syntax.isFact r then add r.Head.HPred (r.Head.HArgs |> List.map (function HTerm (Const c) -> c | _ -> failwith "fact"))
    for group in strata do
        let rules = program.Rules |> List.filter (fun r -> not (Syntax.isFact r) && List.contains r.Head.HPred group)
        let mutable changed = not rules.IsEmpty
        while changed do
            changed <- false
            let snapshot = db
            for r in rules do
                for t in applyRule snapshot r do
                    if not ((rel db r.Head.HPred).Contains t) then
                        add r.Head.HPred t
                        changed <- true
    db
