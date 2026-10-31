module TestUtil

open Xunit
open FsDatalog

let ints (xs: int list list) = xs |> List.map (fun r -> r |> List.map (int64 >> Int) |> Array.ofList)

let rows (db: Database) pred = Engine.tuples db pred

let assertRows (expected: Value[] list) (actual: Value[] list) =
    let show (rs: Value[] list) = rs |> List.map (fun r -> "(" + (r |> Array.map Pretty.value |> String.concat ", ") + ")") |> String.concat " "
    Assert.Equal(show expected, show actual)

let failsWith (fragment: string) (f: unit -> 'a) =
    try
        f () |> ignore
        failwithf "expected an error containing '%s'" fragment
    with DatalogError msg -> Assert.Contains(fragment, msg)
