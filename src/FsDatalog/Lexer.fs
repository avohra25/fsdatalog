namespace FsDatalog

open System
open System.Text

type Tok =
    | TIdent of string
    | TVar of string
    | TInt of int64
    | TStr of string
    | TLParen
    | TRParen
    | TComma
    | TDot
    | TImplies
    | TQuery
    | TOp of string
    | TEof

[<Struct>]
type Token = { Tok: Tok; Line: int; Col: int }

module Lexer =
    let private fail line col msg =
        raise (DatalogError(sprintf "line %d, col %d: %s" line col msg))

    let isIdentStart c = Char.IsLetter c || c = '_'
    let isIdentChar c = Char.IsLetterOrDigit c || c = '_'

    /// Turns source text into tokens. Comments are `% ...`, `// ...` and `/* ... */`.
    let tokenize (src: string) : Token[] =
        let toks = ResizeArray<Token>()
        let mutable i = 0
        let mutable line = 1
        let mutable col = 1
        let peek k = if i + k < src.Length then src.[i + k] else '\000'
        let advance () =
            if src.[i] = '\n' then
                line <- line + 1
                col <- 1
            else
                col <- col + 1
            i <- i + 1
        let emit tok l c = toks.Add { Tok = tok; Line = l; Col = c }
        while i < src.Length do
            let c = src.[i]
            let l, cl = line, col
            if Char.IsWhiteSpace c then
                advance ()
            elif c = '%' || (c = '/' && peek 1 = '/') then
                while i < src.Length && src.[i] <> '\n' do
                    advance ()
            elif c = '/' && peek 1 = '*' then
                advance ()
                advance ()
                while i < src.Length && not (src.[i] = '*' && peek 1 = '/') do
                    advance ()
                if i >= src.Length then fail l cl "unterminated block comment"
                advance ()
                advance ()
            elif Char.IsDigit c then
                let start = i
                while i < src.Length && Char.IsDigit src.[i] do
                    advance ()
                let text = src.Substring(start, i - start)
                match Int64.TryParse text with
                | true, n -> emit (TInt n) l cl
                | _ -> fail l cl (sprintf "integer literal %s is out of range" text)
            elif isIdentStart c then
                let start = i
                while i < src.Length && isIdentChar src.[i] do
                    advance ()
                let text = src.Substring(start, i - start)
                if Char.IsUpper text.[0] || text.[0] = '_' then emit (TVar text) l cl
                else emit (TIdent text) l cl
            elif c = '"' then
                advance ()
                let sb = StringBuilder()
                let mutable closed = false
                while not closed do
                    if i >= src.Length || src.[i] = '\n' then fail l cl "unterminated string literal"
                    match src.[i] with
                    | '"' ->
                        advance ()
                        closed <- true
                    | '\\' ->
                        advance ()
                        if i >= src.Length then fail l cl "unterminated string literal"
                        match src.[i] with
                        | 'n' -> sb.Append '\n' |> ignore
                        | 't' -> sb.Append '\t' |> ignore
                        | '\\' -> sb.Append '\\' |> ignore
                        | '"' -> sb.Append '"' |> ignore
                        | other -> fail line col (sprintf "unknown escape \\%c" other)
                        advance ()
                    | ch ->
                        sb.Append ch |> ignore
                        advance ()
                emit (TStr(sb.ToString())) l cl
            else
                let two = if i + 1 < src.Length then src.Substring(i, 2) else ""
                match two with
                | ":-" | "?-" | "<=" | ">=" | "!=" | "==" ->
                    advance ()
                    advance ()
                    let tok =
                        match two with
                        | ":-" -> TImplies
                        | "?-" -> TQuery
                        | "==" -> TOp "="
                        | op -> TOp op
                    emit tok l cl
                | _ ->
                    advance ()
                    match c with
                    | '(' -> emit TLParen l cl
                    | ')' -> emit TRParen l cl
                    | ',' -> emit TComma l cl
                    | '.' -> emit TDot l cl
                    | '=' | '<' | '>' | '+' | '-' | '*' | '/' -> emit (TOp(string c)) l cl
                    | _ -> fail l cl (sprintf "unexpected character '%c'" c)
        emit TEof line col
        toks.ToArray()

    let describe tok =
        match tok with
        | TIdent s -> sprintf "identifier '%s'" s
        | TVar s -> sprintf "variable '%s'" s
        | TInt n -> sprintf "integer %d" n
        | TStr s -> sprintf "string \"%s\"" s
        | TLParen -> "'('"
        | TRParen -> "')'"
        | TComma -> "','"
        | TDot -> "'.'"
        | TImplies -> "':-'"
        | TQuery -> "'?-'"
        | TOp o -> sprintf "'%s'" o
        | TEof -> "end of input"
