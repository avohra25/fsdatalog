namespace FsDatalog

open System.Collections.Generic

/// A strongly connected component of the predicate dependency graph,
/// evaluated as a unit. `Level` is the classic stratum number: the number of
/// negative or aggregate edges on the longest dependency path below it.
type Stratum =
    { Level: int
      Preds: string list
      Recursive: bool }

module Stratify =
    type private EdgeKind =
        | Positive
        | Negative
        | Aggregate

    /// Splits the predicates of a program into strata in evaluation order
    /// (every stratum comes after the strata it depends on). Raises when a
    /// predicate depends on itself through negation or an aggregate.
    let stratify (rules: Rule list) : Stratum list =
        let names = ResizeArray<string>()
        let index = Dictionary<string, int>()
        let node p =
            match index.TryGetValue p with
            | true, i -> i
            | _ ->
                let i = names.Count
                index.[p] <- i
                names.Add p
                i
        let edges = ResizeArray<ResizeArray<int * EdgeKind * int>>()
        let ensure i =
            while edges.Count <= i do
                edges.Add(ResizeArray())
        for r in rules do
            let h = node r.Head.HPred
            ensure h
            let agg = Syntax.hasAggregate r.Head
            for lit in r.Body do
                match lit with
                | Pos a ->
                    let b = node a.Pred
                    ensure b
                    edges.[h].Add(b, (if agg then Aggregate else Positive), r.Line)
                | Neg a ->
                    let b = node a.Pred
                    ensure b
                    edges.[h].Add(b, Negative, r.Line)
                | Cmp _ -> ()

        // Tarjan's algorithm. Components are emitted dependencies-first.
        let n = names.Count
        let order = Array.create n -1
        let low = Array.zeroCreate n
        let onStack = Array.zeroCreate n
        let stack = Stack<int>()
        let comp = Array.create n -1
        let comps = ResizeArray<int list>()
        let mutable counter = 0
        let rec visit v =
            order.[v] <- counter
            low.[v] <- counter
            counter <- counter + 1
            stack.Push v
            onStack.[v] <- true
            for (w, _, _) in edges.[v] do
                if order.[w] < 0 then
                    visit w
                    low.[v] <- min low.[v] low.[w]
                elif onStack.[w] then
                    low.[v] <- min low.[v] order.[w]
            if low.[v] = order.[v] then
                let members = ResizeArray()
                let mutable go = true
                while go do
                    let w = stack.Pop()
                    onStack.[w] <- false
                    comp.[w] <- comps.Count
                    members.Add w
                    if w = v then go <- false
                comps.Add(members |> Seq.sort |> List.ofSeq)
        for v in 0 .. n - 1 do
            if order.[v] < 0 then visit v

        for v in 0 .. n - 1 do
            for (w, kind, line) in edges.[v] do
                if kind <> Positive && comp.[v] = comp.[w] then
                    let cycle = comps.[comp.[v]] |> List.map (fun i -> names.[i]) |> String.concat ", "
                    let how = if kind = Negative then "negatively" else "through an aggregate"
                    raise (
                        DatalogError(
                            sprintf "line %d: program is not stratifiable: '%s' depends %s on '%s' within the recursive cycle {%s}"
                                line names.[v] how names.[w] cycle
                        )
                    )

        let level = Array.zeroCreate comps.Count
        [ for c in 0 .. comps.Count - 1 do
              let members = comps.[c]
              let mutable lvl = 0
              let mutable recursive = false
              for v in members do
                  for (w, kind, _) in edges.[v] do
                      if comp.[w] = c then recursive <- true
                      else lvl <- max lvl (level.[comp.[w]] + (if kind = Positive then 0 else 1))
              level.[c] <- lvl
              yield { Level = lvl; Preds = members |> List.map (fun i -> names.[i]); Recursive = recursive } ]
