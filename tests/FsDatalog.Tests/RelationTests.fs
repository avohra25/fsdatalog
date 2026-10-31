module RelationTests

open Xunit
open FsDatalog

let t (xs: int list) = xs |> List.map (int64 >> Int) |> Array.ofList

[<Fact>]
let ``set semantics`` () =
    let r = Relation("r", 2)
    Assert.True(r.Add(t [ 1; 2 ]))
    Assert.False(r.Add(t [ 1; 2 ]))
    Assert.True(r.Contains(t [ 1; 2 ]))
    Assert.Equal(1, r.Count)

[<Fact>]
let ``indexes are built lazily and kept current`` () =
    let r = Relation("r", 3)
    for a in 0..9 do
        r.Add(t [ a % 3; a; a * a ]) |> ignore
    Assert.Equal(0, r.IndexCount)
    let mask = 1L // column 0
    Assert.Equal(4, r.Lookup(mask, t [ 0 ]).Count) // 0,3,6,9
    Assert.Equal(1, r.IndexCount)
    r.Add(t [ 0; 100; 1 ]) |> ignore
    Assert.Equal(5, r.Lookup(mask, t [ 0 ]).Count)
    Assert.Equal(0, r.Lookup(mask, t [ 7 ]).Count)
    // columns 0 and 2
    let both = r.Lookup(0b101L, t [ 1; 16 ])
    Assert.Equal(1, both.Count)
    Assert.Equal<Value[]>(t [ 1; 4; 16 ], both.[0])

[<Fact>]
let ``arity is enforced`` () =
    let r = Relation("r", 2)
    Assert.Throws<System.ArgumentException>(fun () -> r.Add(t [ 1 ]) |> ignore) |> ignore

[<Fact>]
let ``sorted puts integers before symbols`` () =
    let r = Relation("r", 1)
    for v in [ Sym "b"; Int 3L; Sym "a"; Int -1L ] do
        r.Add [| v |] |> ignore
    Assert.Equal<Value list>([ Int -1L; Int 3L; Sym "a"; Sym "b" ], r.Sorted() |> List.map (fun x -> x.[0]))
