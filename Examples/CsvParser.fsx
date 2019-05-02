#r "netstandard"
#I @"..\TeqCrate\bin\Debug\netstandard2.0\publish"
#r "TypeEquality.dll"
#r "HCollections.dll"
#r "TeqCrate.dll"

open HCollections
open System
open System.IO
open TeqCrate
open TeqCrate.Patterns
open TypeEquality

let parseCell<'a> (cell : string) : 'a =
    match tType<'a> with
    | String   teq -> cell |> Teq.castFrom teq
    | Bool     teq -> cell |> Boolean.Parse   |> Teq.castFrom teq
    | Int      teq -> cell |> Int32.Parse     |> Teq.castFrom teq
    | Float    teq -> cell |> Double.Parse    |> Teq.castFrom teq
    | DateTime teq -> cell |> DateTime.Parse  |> Teq.castFrom teq
    | _ -> failwithf "Error - the type %s is not supported" (typeof<'a>.FullName)

let rec makeRowReader<'ts> (ts : 'ts TypeList) (index : int) : string array -> 'ts HList =
    match TypeList.split ts with
    | Choice1Of2 teq ->
        let empty = HList.empty |> Teq.castFrom (HList.cong teq)
        fun _ -> empty
    | Choice2Of2 crate ->
        crate.Apply
            { new TypeListConsEvaluator<_,_> with
                member __.Eval ts teq =
                    let rowReader = makeRowReader ts (index + 1)
                    fun cells ->
                        cells
                        |> rowReader
                        |> HList.cons (cells.[index] |> parseCell)
                        |> Teq.castFrom (HList.cong teq)
            }

let tryParse<'record> (filePath : string) : 'record seq option =
    match tType<'record> with
    | Record crate ->
        crate.Apply
            { new RecordConvEvaluator<_,_> with
                member __.Eval _ ts conv =
                    let rowReader = makeRowReader ts 0
                    File.ReadLines filePath
                    |> Seq.map ((fun s -> s.Split ',') >> rowReader >> conv.From)
                    |> Some
            }
    | _ -> None
