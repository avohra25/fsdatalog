module FsDatalog.Cli.Program

open System
open System.IO
open FsDatalog

let usage =
    """usage: fsdl [options] [file ...]

Loads the given Datalog files, prints the answers to their queries, and
starts an interactive prompt when no file or expression is given.

options:
  -e, --eval <text>   evaluate clauses/queries given on the command line
  -i, --interactive   start the prompt after loading files
  --dump              print every derived relation after loading
  --stats             print evaluation statistics after loading
  -h, --help          show this help"""

type Options =
    { Files: string list
      Exprs: string list
      Interactive: bool
      Dump: bool
      Stats: bool }

let rec parseArgs (opts: Options) args =
    match args with
    | [] -> Ok opts
    | ("-h" | "--help") :: _ -> Error ""
    | ("-e" | "--eval") :: text :: rest -> parseArgs { opts with Exprs = opts.Exprs @ [ text ] } rest
    | ("-e" | "--eval") :: [] -> Error "-e needs an argument"
    | ("-i" | "--interactive") :: rest -> parseArgs { opts with Interactive = true } rest
    | "--dump" :: rest -> parseArgs { opts with Dump = true } rest
    | "--stats" :: rest -> parseArgs { opts with Stats = true } rest
    | flag :: _ when flag.StartsWith "-" && flag <> "-" -> Error(sprintf "unknown option %s" flag)
    | file :: rest -> parseArgs { opts with Files = opts.Files @ [ file ] } rest

/// Reads clauses until a line ends in '.', so rules can span lines.
let repl (session: Session) =
    printfn "fsdl: type :help for help, Ctrl-D to quit"
    let buffer = Text.StringBuilder()
    let mutable fin = false
    while not fin do
        Console.Write(if buffer.Length = 0 then "dl> " else "..> ")
        match Console.ReadLine() with
        | null -> fin <- true
        | line ->
            let t = line.Trim()
            if buffer.Length = 0 && t.StartsWith ":" then
                session.Execute t |> List.iter (printfn "%s")
            else
                buffer.AppendLine line |> ignore
                let text = buffer.ToString().Trim()
                if text = "" then buffer.Clear() |> ignore
                elif text.EndsWith "." then
                    session.Execute text |> List.iter (printfn "%s")
                    buffer.Clear() |> ignore
    printfn ""

[<EntryPoint>]
let main argv =
    match parseArgs { Files = []; Exprs = []; Interactive = false; Dump = false; Stats = false } (List.ofArray argv) with
    | Error msg ->
        if msg <> "" then eprintfn "fsdl: %s" msg
        printfn "%s" usage
        if msg = "" then 0 else 2
    | Ok opts ->
        let session = Session()
        let mutable failed = false
        let emit (lines: string list) =
            for l in lines do
                if l.StartsWith "error: " then
                    failed <- true
                    eprintfn "%s" l
                else
                    printfn "%s" l
        for f in opts.Files do
            if not failed then
                if f <> "-" && not (File.Exists f) then
                    eprintfn "error: no such file: %s" f
                    failed <- true
                else
                    let src = if f = "-" then Console.In.ReadToEnd() else File.ReadAllText f
                    emit (session.Execute src)
        for e in opts.Exprs do
            if not failed then emit (session.Execute e)
        if not failed then
            if opts.Dump then emit (session.Execute ":dump")
            if opts.Stats then emit (session.Execute ":stats")
        if (opts.Files.IsEmpty && opts.Exprs.IsEmpty) || opts.Interactive then repl session
        if failed then 1 else 0
