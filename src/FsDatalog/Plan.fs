namespace FsDatalog

open System
open System.Collections.Generic

/// Which version of a relation a scan reads during semi-naive evaluation.
type Source =
    | Full
    | Delta

/// An expression over environment slots.
type CExpr =
    | CConst of Value
    | CSlot of int
    | CBin of ArithOp * CExpr * CExpr

/// One step of a compiled rule body. Steps run left to right as nested loops.
type Step =
    /// Iterate the tuples of `pred` matching `key` on the columns in `mask`,
    /// writing columns into slots (`binds`) and checking repeated variables (`checks`).
    | Scan of pred: string * source: Source * mask: int64 * key: CExpr[] * binds: (int * int)[] * checks: (int * int)[]
    | Filter of CmpOp * CExpr * CExpr
    | Bind of slot: int * CExpr
    /// Succeeds when no tuple of `pred` matches (negation as failure).
    | Absent of pred: string * mask: int64 * key: CExpr[]

type CHead =
    | HSlot of int
    | HConst of Value

type Plan =
    { Steps: Step[]
      Slots: int
      /// Variable name for each slot.
      SlotNames: string[] }

/// Orders body literals and compiles them to steps.
///
/// Greedy strategy: filters (comparisons, negations) run as soon as their
/// variables are bound, then assignments that can bind, then the positive
/// atom with the most bound arguments. The delta atom, if any, goes first.
module Planner =
    let rec private compileExpr (slots: Dictionary<string, int>) e =
        match e with
        | ETerm (Const c) -> CConst c
        | ETerm (Var v) -> CSlot slots.[v]
        | EBin (op, a, b) -> CBin(op, compileExpr slots a, compileExpr slots b)

    let compileBody (body: Literal list) (deltaAt: int option) : Plan =
        let slots = Dictionary<string, int>()
        let names = ResizeArray<string>()
        let slotOf v =
            match slots.TryGetValue v with
            | true, s -> s
            | _ ->
                let s = names.Count
                slots.[v] <- s
                names.Add v
                s
        let bound = HashSet<string>()
        let isBound v = bound.Contains v
        let allBound vars = Set.forall isBound vars
        let steps = ResizeArray<Step>()
        let remaining = ResizeArray<int * Literal>(List.indexed body)

        let keyOf (a: Atom) (onlyNamed: bool) =
            let mutable mask = 0L
            let key = ResizeArray<CExpr>()
            a.Args
            |> List.iteri (fun col arg ->
                match arg with
                | Const c ->
                    mask <- mask ||| (1L <<< col)
                    key.Add(CConst c)
                | Var v when isBound v && not (onlyNamed && Names.isWildcard v) ->
                    mask <- mask ||| (1L <<< col)
                    key.Add(CSlot slots.[v])
                | Var _ -> ())
            mask, key.ToArray()

        let scan (a: Atom) source =
            let mask, key = keyOf a false
            let binds = ResizeArray<int * int>()
            let checks = ResizeArray<int * int>()
            let local = HashSet<string>()
            a.Args
            |> List.iteri (fun col arg ->
                match arg with
                | Var v when not (isBound v) ->
                    if local.Add v then binds.Add(col, slotOf v) else checks.Add(col, slots.[v])
                | _ -> ())
            for v in local do
                bound.Add v |> ignore
            steps.Add(Scan(a.Pred, source, mask, key, binds.ToArray(), checks.ToArray()))

        let take i = remaining.RemoveAt i

        match deltaAt with
        | Some d ->
            match List.item d body with
            | Pos a ->
                scan a Delta
                remaining.RemoveAll(fun (i, _) -> i = d) |> ignore
            | _ -> invalidArg "deltaAt" "the delta literal must be a positive atom"
        | None -> ()

        while remaining.Count > 0 do
            let ready =
                remaining
                |> Seq.tryFindIndex (fun (_, lit) ->
                    match lit with
                    | Neg a -> Syntax.atomVars a |> Set.filter (Names.isWildcard >> not) |> allBound
                    | Cmp _ -> allBound (Syntax.literalVars lit)
                    | Pos _ -> false)
            match ready with
            | Some i ->
                match snd remaining.[i] with
                | Neg a ->
                    let mask, key = keyOf a true
                    steps.Add(Absent(a.Pred, mask, key))
                | Cmp (op, l, r) -> steps.Add(Filter(op, compileExpr slots l, compileExpr slots r))
                | Pos _ -> ()
                take i
            | None ->
                let assignment =
                    remaining
                    |> Seq.tryPick (fun (i, lit) ->
                        match lit with
                        | Cmp (Eq, ETerm (Var v), e)
                        | Cmp (Eq, e, ETerm (Var v)) when not (isBound v) && allBound (Syntax.exprVars e) -> Some(i, v, e)
                        | _ -> None)
                match assignment with
                | Some (i, v, e) ->
                    let ce = compileExpr slots e
                    steps.Add(Bind(slotOf v, ce))
                    bound.Add v |> ignore
                    take (remaining.FindIndex(fun (j, _) -> j = i))
                | None ->
                    let candidates =
                        remaining
                        |> Seq.mapi (fun pos (_, lit) -> pos, lit)
                        |> Seq.choose (fun (pos, lit) ->
                            match lit with
                            | Pos a ->
                                let score =
                                    a.Args |> List.sumBy (function Const _ -> 1 | Var v when isBound v -> 1 | _ -> 0)
                                Some(pos, a, score)
                            | _ -> None)
                        |> List.ofSeq
                    match candidates with
                    | [] ->
                        raise (DatalogError("cannot order body literals (unsafe rule): " + String.Join(", ", remaining |> Seq.map (snd >> Pretty.literal))))
                    | _ ->
                        let best = candidates |> List.maxBy (fun (pos, _, score) -> score, -pos)
                        let pos, a, _ = best
                        scan a Full
                        take pos

        { Steps = steps.ToArray(); Slots = names.Count; SlotNames = names.ToArray() }

    /// Compiles a rule body and maps its (non-aggregate) head onto slots.
    let compileRule (r: Rule) (deltaAt: int option) : Plan * CHead[] =
        let plan = compileBody r.Body deltaAt
        let slotIndex = plan.SlotNames |> Array.indexed |> Array.map (fun (i, n) -> n, i) |> dict
        let head =
            r.Head.HArgs
            |> List.map (function
                | HTerm (Const c) -> HConst c
                | HTerm (Var v) -> HSlot slotIndex.[v]
                | HAgg _ -> invalidArg "r" "aggregate heads are compiled separately")
            |> Array.ofList
        plan, head

/// Runs compiled plans.
module Exec =
    let private arith op (a: Value) (b: Value) =
        match a, b with
        | Int x, Int y ->
            try
                match op with
                | Add -> Int(Checked.(+) x y)
                | Sub -> Int(Checked.(-) x y)
                | Mul -> Int(Checked.(*) x y)
                | Div ->
                    if y = 0L then raise (DatalogError "division by zero")
                    Int(x / y)
                | Mod ->
                    if y = 0L then raise (DatalogError "modulo by zero")
                    Int(x % y)
            with :? OverflowException ->
                raise (DatalogError(sprintf "integer overflow computing %d %s %d" x (Pretty.opText op) y))
        | _ ->
            raise (DatalogError(sprintf "arithmetic needs integers but got %s and %s" (Pretty.value a) (Pretty.value b)))

    let rec eval (env: Value[]) e =
        match e with
        | CConst c -> c
        | CSlot s -> env.[s]
        | CBin (op, a, b) -> arith op (eval env a) (eval env b)

    let test op (a: Value) (b: Value) =
        match op with
        | Eq -> a = b
        | Ne -> a <> b
        | Lt -> compare a b < 0
        | Le -> compare a b <= 0
        | Gt -> compare a b > 0
        | Ge -> compare a b >= 0

    /// Enumerates every solution of the plan, calling `emit` with the
    /// (reused, mutable) environment for each.
    let run (plan: Plan) (relation: string -> Source -> Relation) (emit: Value[] -> unit) =
        let steps = plan.Steps
        let env = Array.create plan.Slots (Int 0L)
        let rec go i =
            if i = steps.Length then
                emit env
            else
                match steps.[i] with
                | Scan (pred, source, mask, key, binds, checks) ->
                    let k = Array.map (eval env) key
                    let rows = (relation pred source).Lookup(mask, k)
                    for j in 0 .. rows.Count - 1 do
                        let t = rows.[j]
                        for (col, slot) in binds do
                            env.[slot] <- t.[col]
                        let mutable ok = true
                        for (col, slot) in checks do
                            if t.[col] <> env.[slot] then ok <- false
                        if ok then go (i + 1)
                | Filter (op, l, r) -> if test op (eval env l) (eval env r) then go (i + 1)
                | Bind (slot, e) ->
                    env.[slot] <- eval env e
                    go (i + 1)
                | Absent (pred, mask, key) ->
                    let k = Array.map (eval env) key
                    if (relation pred Full).Lookup(mask, k).Count = 0 then go (i + 1)
        go 0
