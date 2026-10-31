namespace FsDatalog

open System
open System.IO

/// An interactive session: a growing program plus its evaluated database.
/// `Execute` takes a chunk of input (clauses, queries or a `:command`) and
/// returns the lines to print. Adding clauses that make the program invalid
/// (unsafe, wrong arity, unstratifiable, runtime error) leaves the session
/// unchanged.
type Session() =
    let mutable rules: Rule list = []
    let mutable db: Database option = None

    let database () =
        match db with
        | Some d -> d
        | None ->
            let d = Engine.evaluate { Rules = rules; Queries = [] }
            db <- Some d
            d

    let plural n (one: string) = if n = 1 then sprintf "1 %s" one else sprintf "%d %ss" n one

    let formatAnswer (a: Answer) =
        if a.Columns.IsEmpty then
            [ (if a.Rows.IsEmpty then "no." else "yes.") ]
        elif a.Rows.IsEmpty then
            [ "no answers." ]
        else
            [ for row in a.Rows ->
                  List.zip a.Columns (List.ofArray row)
                  |> List.map (fun (c, v) -> c + " = " + Pretty.value v)
                  |> String.concat ", " ]
            @ [ plural a.Rows.Length "answer" + "." ]

    let help =
        [ "Enter clauses (facts and rules) ending in '.', or queries like '?- path(1, X).'"
          "Commands:"
          "  :help            show this help"
          "  :rules           list the program's clauses"
          "  :dump [pred]     print all derived tuples (or those of one predicate)"
          "  :strata          show the evaluation strata"
          "  :stats           show statistics from the last evaluation"
          "  :load <file>     add the clauses and run the queries in a file"
          "  :clear           forget every clause" ]

    member _.Rules = rules
    member _.Database = database ()

    member this.Load(src: string) : string list =
        let program = Parser.parse src
        let candidate = rules @ program.Rules
        let evaluated = Engine.evaluate { Rules = candidate; Queries = [] }
        rules <- candidate
        db <- Some evaluated
        let warnings = evaluated.Warnings |> List.map (fun w -> "warning: " + w)
        let answers = program.Queries |> List.collect (fun q -> formatAnswer (Engine.query evaluated q))
        warnings @ answers

    member this.Command(line: string) : string list =
        let parts = line.Trim().Split([| ' '; '\t' |], 2, StringSplitOptions.RemoveEmptyEntries)
        let arg = if parts.Length > 1 then parts.[1].Trim() else ""
        match parts.[0] with
        | ":help" | ":h" -> help
        | ":rules" -> if rules.IsEmpty then [ "% no clauses" ] else rules |> List.map Pretty.rule
        | ":dump" ->
            let d = database ()
            let preds = if arg = "" then d.Relations.Keys |> Seq.sort |> List.ofSeq else [ arg ]
            [ for p in preds do
                  for t in Engine.tuples d p -> Pretty.tuple p t + "." ]
        | ":strata" ->
            [ for s in (database ()).Strata ->
                  sprintf "%d: %s%s" s.Level (String.concat ", " s.Preds) (if s.Recursive then " (recursive)" else "") ]
        | ":stats" ->
            let s = (database ()).Stats
            [ sprintf "%s, %s, %s, %s, %.1f ms" (if s.Strata = 1 then "1 stratum" else sprintf "%d strata" s.Strata) (plural s.Iterations "round") (plural s.Facts "base fact") (plural s.Derived "derived fact") s.Elapsed.TotalMilliseconds ]
        | ":load" ->
            if arg = "" then raise (DatalogError ":load needs a file name")
            elif not (File.Exists arg) then raise (DatalogError(sprintf "no such file: %s" arg))
            else this.Load(File.ReadAllText arg)
        | ":clear" ->
            rules <- []
            db <- None
            [ "cleared." ]
        | other -> raise (DatalogError(sprintf "unknown command %s (try :help)" other))

    /// Runs one chunk of input. Errors are returned as "error: ..." lines.
    member this.Execute(input: string) : string list =
        try
            let trimmed = input.Trim()
            if trimmed = "" then []
            elif trimmed.StartsWith ":" then this.Command trimmed
            else this.Load trimmed
        with DatalogError msg ->
            [ "error: " + msg ]
