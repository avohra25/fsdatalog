namespace FsDatalog

open System
open System.Collections.Generic
open System.Diagnostics

type EvalStats =
    { Strata: int
      Iterations: int
      Facts: int
      Derived: int
      Elapsed: TimeSpan }

/// The result of evaluating a program: every relation at its least fixpoint.
type Database =
    { Relations: IReadOnlyDictionary<string, Relation>
      Arities: Map<string, int>
      Strata: Stratum list
      Stats: EvalStats
      Warnings: string list }

type Answer =
    { Columns: string list
      Rows: Value[] list }

/// Bottom-up evaluation: strata in dependency order, each solved with
/// semi-naive iteration so a round only joins against the previous round's
/// new tuples.
module Engine =
    let private factTuple (r: Rule) =
        r.Head.HArgs
        |> List.map (function
            | HTerm (Const c) -> c
            | _ -> raise (DatalogError(sprintf "line %d: facts must be ground" r.Line)))
        |> Array.ofList

    /// Evaluates an aggregate rule (its body only reads lower strata).
    /// Each group aggregates over the distinct bindings of the body's named variables.
    let private evalAggregate (rels: Dictionary<string, Relation>) (r: Rule) (emit: Value[] -> unit) =
        let plan = Planner.compileBody r.Body None
        let slotOf = plan.SlotNames |> Array.indexed |> Array.map (fun (i, n) -> n, i) |> dict
        let named = plan.SlotNames |> Array.indexed |> Array.filter (snd >> Names.isWildcard >> not) |> Array.map fst
        let bindings = HashSet<Value[]>(HashIdentity.Structural)
        let distinct = ResizeArray<Value[]>()
        Exec.run plan (fun p _ -> rels.[p]) (fun env ->
            let b = Array.init plan.Slots (fun i -> env.[i])
            for i in 0 .. b.Length - 1 do
                if not (Array.contains i named) then b.[i] <- Int 0L
            if bindings.Add b then distinct.Add b)
        let groupCols =
            r.Head.HArgs
            |> List.choose (function
                | HTerm (Var v) -> Some(CSlot slotOf.[v])
                | HTerm (Const c) -> Some(CConst c)
                | HAgg _ -> None)
            |> Array.ofList
        let groups = Dictionary<Value[], ResizeArray<Value[]>>(HashIdentity.Structural)
        let groupOrder = ResizeArray<Value[]>()
        for b in distinct do
            let key = groupCols |> Array.map (Exec.eval b)
            match groups.TryGetValue key with
            | true, g -> g.Add b
            | _ ->
                groups.[key] <- ResizeArray [ b ]
                groupOrder.Add key
        for key in groupOrder do
            let members = groups.[key]
            let mutable gi = 0
            let tuple =
                r.Head.HArgs
                |> List.map (fun arg ->
                    match arg with
                    | HTerm _ ->
                        gi <- gi + 1
                        key.[gi - 1]
                    | HAgg (f, v) ->
                        let s = slotOf.[v]
                        let values = members |> Seq.map (fun b -> b.[s])
                        match f with
                        | Count -> Int(int64 members.Count)
                        | Min -> Seq.min values
                        | Max -> Seq.max values
                        | Sum ->
                            values
                            |> Seq.fold
                                (fun acc v ->
                                    match acc, v with
                                    | Int a, Int x ->
                                        try
                                            Int(Checked.(+) a x)
                                        with :? OverflowException ->
                                            raise (DatalogError(sprintf "line %d: integer overflow in sum" r.Line))
                                    | _, other ->
                                        raise (DatalogError(sprintf "line %d: sum needs integers but found %s" r.Line (Pretty.value other))))
                                (Int 0L))
                |> Array.ofList
            emit tuple

    /// Solves one stratum to its fixpoint; returns the number of rounds.
    let private evalStratum (rels: Dictionary<string, Relation>) (preds: Set<string>) (rules: Rule list) =
        let newPending () =
            let d = Dictionary<string, Relation>()
            for p in preds do
                d.[p] <- Relation(p, rels.[p].Arity)
            d
        let mutable pending = newPending ()
        let emitter pred (head: CHead[]) =
            let target = rels.[pred]
            fun (env: Value[]) ->
                let t =
                    head
                    |> Array.map (function
                        | HSlot s -> env.[s]
                        | HConst c -> c)
                if not (target.Contains t) then pending.[pred].Add t |> ignore

        let aggRules, plain = rules |> List.partition (fun r -> Syntax.hasAggregate r.Head)

        // Round 0: every rule against the full relations.
        for r in aggRules do
            let target = rels.[r.Head.HPred]
            evalAggregate rels r (fun t -> if not (target.Contains t) then pending.[r.Head.HPred].Add t |> ignore)
        for r in plain do
            let plan, head = Planner.compileRule r None
            Exec.run plan (fun p _ -> rels.[p]) (emitter r.Head.HPred head)

        // One plan per (rule, recursive atom): that atom reads the delta.
        let deltaPlans =
            plain
            |> List.collect (fun r ->
                r.Body
                |> List.indexed
                |> List.choose (fun (i, lit) ->
                    match lit with
                    | Pos a when preds.Contains a.Pred ->
                        let plan, head = Planner.compileRule r (Some i)
                        Some(r.Head.HPred, plan, head)
                    | _ -> None))

        let merge () =
            let delta = pending
            let mutable added = 0
            for KeyValue (p, rel) in delta do
                for t in rel.Rows do
                    if rels.[p].Add t then added <- added + 1
            pending <- newPending ()
            delta, added

        let mutable rounds = 1
        let mutable delta, added = merge ()
        while added > 0 && not deltaPlans.IsEmpty do
            let d = delta
            for (pred, plan, head) in deltaPlans do
                Exec.run plan (fun p src -> if src = Delta then d.[p] else rels.[p]) (emitter pred head)
            let next, n = merge ()
            delta <- next
            added <- n
            rounds <- rounds + 1
        rounds

    /// Evaluates a program to its least (perfect) model.
    let evaluate (program: Program) : Database =
        let sw = Stopwatch.StartNew()
        let warnings = Analysis.check program
        let arities = Analysis.arities program
        let strata = Stratify.stratify program.Rules
        let rels = Dictionary<string, Relation>()
        for KeyValue (p, a) in arities do
            rels.[p] <- Relation(p, a)
        let facts, rules = program.Rules |> List.partition Syntax.isFact
        let mutable factCount = 0
        for f in facts do
            if rels.[f.Head.HPred].Add(factTuple f) then factCount <- factCount + 1
        let mutable rounds = 0
        for s in strata do
            let preds = Set.ofList s.Preds
            match rules |> List.filter (fun r -> preds.Contains r.Head.HPred) with
            | [] -> ()
            | srules -> rounds <- rounds + evalStratum rels preds srules
        let total = rels.Values |> Seq.sumBy (fun r -> r.Count)
        { Relations = (rels :> IReadOnlyDictionary<string, Relation>)
          Arities = arities
          Strata = strata
          Stats =
            { Strata = (strata |> List.map (fun s -> s.Level) |> List.fold max -1) + 1
              Iterations = rounds
              Facts = factCount
              Derived = total - factCount
              Elapsed = sw.Elapsed }
          Warnings = warnings }

    /// Parses and evaluates program text.
    let run (src: string) = evaluate (Parser.parse src)

    /// The tuples of a predicate in sorted order (empty if unknown).
    let tuples (db: Database) (pred: string) : Value[] list =
        match db.Relations.TryGetValue pred with
        | true, r -> r.Sorted()
        | _ -> []

    /// Answers a query against an evaluated database. Columns are the query's
    /// named variables in order of first appearance; rows are distinct and sorted.
    let query (db: Database) (q: Query) : Answer =
        Analysis.checkBody q.QLine q.Goals |> ignore
        for lit in q.Goals do
            match lit with
            | Pos a | Neg a ->
                match Map.tryFind a.Pred db.Arities with
                | Some n when n <> a.Args.Length ->
                    raise (DatalogError(sprintf "line %d: predicate '%s' has %d argument(s) but the query uses %d" q.QLine a.Pred n a.Args.Length))
                | _ -> ()
            | Cmp _ -> ()
        let empty = Dictionary<string, Relation>()
        let relation p =
            match db.Relations.TryGetValue p with
            | true, r -> r
            | _ ->
                match empty.TryGetValue p with
                | true, r -> r
                | _ ->
                    let arity =
                        q.Goals
                        |> List.pick (function
                            | Pos a | Neg a when a.Pred = p -> Some a.Args.Length
                            | _ -> None)
                    let r = Relation(p, arity)
                    empty.[p] <- r
                    r
        let columns =
            q.Goals
            |> List.collect (fun lit ->
                match lit with
                | Pos a | Neg a -> a.Args |> List.choose (function Var v -> Some v | _ -> None)
                | Cmp (_, l, r) -> Set.toList (Syntax.exprVars l) @ Set.toList (Syntax.exprVars r))
            |> List.filter (Names.isHidden >> not)
            |> List.distinct
        let plan = Planner.compileBody q.Goals None
        let slotOf = plan.SlotNames |> Array.indexed |> Array.map (fun (i, n) -> n, i) |> dict
        let outSlots = columns |> List.map (fun c -> slotOf.[c]) |> Array.ofList
        let rows = HashSet<Value[]>(HashIdentity.Structural)
        Exec.run plan (fun p _ -> relation p) (fun env -> rows.Add(Array.map (fun s -> env.[s]) outSlots) |> ignore)
        { Columns = columns; Rows = rows |> Seq.sortWith compare |> List.ofSeq }

    let queryText (db: Database) (text: string) = query db (Parser.parseQuery text)
