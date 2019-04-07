namespace TeqCrate

open HCollections
open Microsoft.FSharp.Reflection
open System
open TypePatterns

type TupleConvEvaluator<'tuple, 'ret> = abstract member Eval : 'ts TypeList -> Conv<'tuple, 'ts HList> -> 'ret
type 'tuple TupleConvCrate = abstract member Apply : TupleConvEvaluator<'tuple, 'ret> -> 'ret

module TupleConvCrate =

    let make ts conv =
        { new TupleConvCrate<_> with
            member __.Apply e = e.Eval ts conv
        }

    let rec makeUntyped (ts : Type list) : obj list TupleConvCrate =
        match ts with
        | [] ->
            let toF _ = HList.empty
            let fromF _ = []
            Conv.make toF fromF |> make TypeList.empty
        | t::ts ->
            let crate = TypeParameterCrate.makeUntyped t
            crate.Apply
                { new TypeParameterEvaluator<_> with
                    member __.Eval<'t> () = makeUntypedInner<'t> ts
                }

    and makeUntypedInner<'t> (ts : Type list) : obj list TupleConvCrate =
        let crate = makeUntyped ts
        crate.Apply
            { new TupleConvEvaluator<_,_> with
                member __.Eval ts conv =
                    let toF os =
                        let t = os |> List.head |> unbox<'t>
                        os |> List.tail |> conv.To |> HList.cons t
                    let fromF xs =
                        let o = xs |> HList.head |> box
                        let os = xs |> HList.tail |> conv.From
                        o::os
                    Conv.make toF fromF |> make (ts |> TypeList.cons)
            }

    let tryMake () : 'tuple TupleConvCrate option =

        let t = typeof<'tuple>
        match t with
        | Tuple ts ->
            let tupleConv =
                let make = FSharpValue.PreComputeTupleConstructor t
                let reader = FSharpValue.PreComputeTupleReader t
                Conv.make (reader >> Array.toList) (List.toArray >> make >> unbox)

            let crate = makeUntyped ts
            crate.Apply
                { new TupleConvEvaluator<_,_> with
                    member __.Eval ts conv = Conv.compose tupleConv conv |> make ts
                }
            |> Some

        | _ -> None


type RecordConvEvaluator<'record, 'ret> = abstract member Eval : string list -> 'ts TypeList -> Conv<'record, 'ts HList> -> 'ret
type 'record RecordConvCrate = abstract member Apply : RecordConvEvaluator<'record, 'ret> -> 'ret

module RecordConvCrate =

    let make names tl conv =
        { new RecordConvCrate<_> with
            member __.Apply e = e.Eval names tl conv
        }

    let rec makeUntyped<'ts> (names : string list) (tl : 'ts TypeList) (ts : Type list) : obj list RecordConvCrate =
        match ts with
        | [] ->
            let toF _ = HList.empty
            let fromF _ = []
            Conv.make toF fromF |> make names TypeList.empty
        | t::ts ->
            let crate = TypeParameterCrate.makeUntyped t
            crate.Apply
                { new TypeParameterEvaluator<_> with
                    member __.Eval<'t> () = makeUntypedInner<'t, 'ts> names tl ts
                }

    and makeUntypedInner<'t, 'ts> (names : string list) (tl : 'ts TypeList) (ts : Type list) : obj list RecordConvCrate =
        let crate = makeUntyped names tl ts
        crate.Apply
            { new RecordConvEvaluator<_,_> with
                member __.Eval names tl conv =
                    let toF os =
                        let t = os |> List.head |> unbox<'t>
                        os |> List.tail |> conv.To |> HList.cons t
                    let fromF xs =
                        let o = xs |> HList.head |> box
                        let os = xs |> HList.tail |> conv.From
                        o::os
                    Conv.make toF fromF |> make names (tl |> TypeList.cons)
            }

    let tryMake () : 'record RecordConvCrate option =

        let t = typeof<'record>
        match t with
        | Record ts ->
            let recordConv =
                let make = FSharpValue.PreComputeRecordConstructor t
                let reader = FSharpValue.PreComputeRecordReader t
                Conv.make (reader >> Array.toList) (List.toArray >> make >> unbox)

            let names, ts = ts |> List.unzip
            let crate = makeUntyped names TypeList.empty ts
            crate.Apply
                { new RecordConvEvaluator<_,_> with
                    member __.Eval names tl conv =
                        Conv.compose recordConv conv |> make names tl
                }
            |> Some

        | _ -> None


type UnionConvEvaluator<'union, 'ret> = abstract member Eval : string list -> 'ts TypeList -> Conv<'union, 'ts HUnion> -> 'ret
type 'union UnionConvCrate = abstract member Apply : UnionConvEvaluator<'union, 'ret> -> 'ret

module UnionConvCrate =

    let make names ts conv =
        { new UnionConvCrate<_> with
            member __.Apply e = e.Eval names ts conv
        }

    let rec makeUntyped (names : string list) (ts : Type list) : (int * obj) UnionConvCrate =
        match ts with
        | [] -> failwith "Cannot create UnionConvCrate - the list of union cases must not be empty"
        | t::ts ->
            let crate = TypeParameterCrate.makeUntyped t
            crate.Apply
                { new TypeParameterEvaluator<_> with
                    member __.Eval<'t> () = makeUntypedInner<'t> names ts
                }

    and makeUntypedInner<'t> (names : string list) (ts : Type list) : (int * obj) UnionConvCrate =
        match ts with
        | [] ->
            let toF (_, o) = o |> unbox<'t> |> HUnion.make TypeList.empty
            let fromF xs = 0, HUnion.getSingleton xs |> box
            let ts = TypeList.empty |> TypeList.cons
            Conv.make toF fromF |> make names ts
        | _ ->
            let crate = makeUntyped names ts
            crate.Apply
                { new UnionConvEvaluator<_,_> with
                    member __.Eval names ts conv =
                        let toF (i, o) =
                            if i = 0 then o |> unbox<'t> |> HUnion.make ts
                            else (i - 1, o) |> conv.To |> HUnion.extend
                        let fromF (xs : ('t -> 'a) HUnion) : int * obj =
                            match xs |> HUnion.split with
                            | Choice1Of2 x -> 0, x |> box
                            | Choice2Of2 xs ->
                                let (i, x) = conv.From xs
                                i + 1, x
                        let ts = ts |> TypeList.cons
                        Conv.make toF fromF |> make names ts
                }

    let tryMake () : 'union UnionConvCrate option =

        let t = typeof<'union>
        match t with
        | Union cases ->

            let makeConverter =
                function
                | [] ->
                    let conv = Conv.make (fun _ -> () |> box) (fun _ -> [||])
                    typeof<unit>, conv
                | [t] ->
                    let conv = Conv.make Array.head Array.singleton
                    t, conv
                | ts ->
                    let t = ts |> Array.ofList |> FSharpType.MakeTupleType
                    let maker = FSharpValue.PreComputeTupleConstructor t
                    let reader = FSharpValue.PreComputeTupleReader t
                    let conv = Conv.make maker reader
                    t, conv

            let makeCaseConv case : Conv<obj, obj array> =
                let reader = FSharpValue.PreComputeUnionReader case
                let maker = FSharpValue.PreComputeUnionConstructor case
                Conv.make reader maker

            let bitsForCase (case : UnionCaseInfo) =
                let ts = case.GetFields () |> Array.map (fun pi -> pi.PropertyType) |> List.ofArray
                let t, conv = makeConverter ts
                t, Conv.compose (makeCaseConv case) conv

            let ts, convs = cases |> List.map bitsForCase |> List.unzip
            let convs = convs |> Array.ofList

            let unionConv : Conv<'union, int * obj> =
                let getTag = FSharpValue.PreComputeUnionTagReader t
                let toF u = let i = getTag u in i, convs.[i].To u
                let fromF (i, o) = o |> convs.[i].From |> unbox
                Conv.make toF fromF

            let names = cases |> List.map (fun case -> case.Name)
            let crate = makeUntyped names ts
            crate.Apply
                { new UnionConvEvaluator<_,_> with
                    member __.Eval names ts conv =
                        Conv.compose unionConv conv |> make names ts
                }
            |> Some

        | _ -> None
