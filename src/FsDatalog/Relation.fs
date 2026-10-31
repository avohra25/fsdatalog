namespace FsDatalog

open System.Collections.Generic

/// A set of tuples of fixed arity with lazily built hash indexes.
///
/// An index is identified by a bitmask of bound columns; the first lookup
/// with a given mask builds it and every later insertion keeps it current.
type Relation(name: string, arity: int) =
    do if arity > 62 then raise (DatalogError(sprintf "predicate '%s' has %d arguments; at most 62 are supported" name arity))
    static let empty = ResizeArray<Value[]>() :> IReadOnlyList<Value[]>
    let set = HashSet<Value[]>(HashIdentity.Structural)
    let rows = ResizeArray<Value[]>()
    let indexes = Dictionary<int64, int[] * Dictionary<Value[], ResizeArray<Value[]>>>()

    static let columnsOf (mask: int64) =
        [| for c in 0..61 do
               if mask &&& (1L <<< c) <> 0L then yield c |]

    static let project (cols: int[]) (t: Value[]) = Array.init cols.Length (fun i -> t.[cols.[i]])

    static let insert (cols: int[]) (idx: Dictionary<Value[], ResizeArray<Value[]>>) (t: Value[]) =
        let k = project cols t
        match idx.TryGetValue k with
        | true, bucket -> bucket.Add t
        | _ ->
            let bucket = ResizeArray<Value[]>()
            bucket.Add t
            idx.[k] <- bucket

    member _.Name = name
    member _.Arity = arity
    member _.Count = rows.Count
    member _.Rows: IReadOnlyList<Value[]> = rows :> _
    member _.IndexCount = indexes.Count
    member _.Contains(t: Value[]) = set.Contains t

    /// Adds a tuple; returns false if it was already present.
    member _.Add(t: Value[]) =
        if t.Length <> arity then
            invalidArg "t" (sprintf "relation %s has arity %d but got a tuple of length %d" name arity t.Length)
        if set.Add t then
            rows.Add t
            for KeyValue (_, (cols, idx)) in indexes do
                insert cols idx t
            true
        else
            false

    /// All tuples whose columns in `mask` equal `key` (key values in ascending column order).
    member _.Lookup(mask: int64, key: Value[]) : IReadOnlyList<Value[]> =
        if mask = 0L then
            rows :> _
        else
            let cols, idx =
                match indexes.TryGetValue mask with
                | true, entry -> entry
                | _ ->
                    let cols = columnsOf mask
                    let idx = Dictionary<Value[], ResizeArray<Value[]>>(HashIdentity.Structural)
                    for t in rows do
                        insert cols idx t
                    indexes.[mask] <- (cols, idx)
                    (cols, idx)
            ignore cols
            match idx.TryGetValue key with
            | true, bucket -> bucket :> _
            | _ -> empty

    /// Tuples in ascending order (integers before symbols, then lexicographic).
    member _.Sorted() : Value[] list = rows |> Seq.sortWith compare |> List.ofSeq
