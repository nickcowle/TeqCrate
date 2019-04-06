namespace TeqCrate

open HCollections
open Microsoft.FSharp.Reflection
open System
open TeqCrate.TypePatterns


type private ObjListHListConvCrate = abstract member Apply : ObjListHListConvCrateEvaluator<'ret> -> 'ret
and private ObjListHListConvCrateEvaluator<'ret> = abstract member Eval : Conv<obj list, 'a HList> -> 'ret

[<RequireQualifiedAccess>]
module private ObjListHListConvCrate =

    let make (conv : Conv<obj list, 'a HList>) : ObjListHListConvCrate =
        { new ObjListHListConvCrate with
            member __.Apply e = e.Eval conv
        }

    let rec makeUntyped (ts : Type list) : ObjListHListConvCrate =
        match ts with
        | [] ->
            Conv.make (fun _ -> HList.empty) (fun _ -> []) |> make
        | t::ts ->
            Reflection.invokeStaticMethod <@ makeUntypedInner @> [| t |] [| ts |]
            |> unbox

    and private makeUntypedInner<'t> (ts : Type list) : ObjListHListConvCrate =
        (makeUntyped ts).Apply
            { new ObjListHListConvCrateEvaluator<_> with
                member __.Eval conv =
                    let toF os =
                        match os with
                        | [] -> raise Unreachable
                        | o::os -> conv.To os |> HList.cons (o |> unbox<'t>)
                    let fromF hList =
                        let o = hList |> HList.head
                        let os = hList |> HList.tail |> conv.From
                        (box o)::os
                    Conv.make toF fromF |> make
            }


type private ObjListTupleConvCrate = abstract member Apply : ObjListTupleConvCrateEvaluator<'ret> -> 'ret
and private ObjListTupleConvCrateEvaluator<'ret> = abstract member Eval : Conv<obj list, 'tuple> -> 'ret

[<RequireQualifiedAccess>]
module private ObjListTupleConvCrate =

    let make (conv : Conv<obj list, 'tuple>) : ObjListTupleConvCrate =
        { new ObjListTupleConvCrate with
            member __.Apply e = e.Eval conv
        }

    let rec makeUntyped (ts : Type list) : ObjListTupleConvCrate =

        match ts with
        | [] ->
            let toF (os : obj list) : unit = ()
            let fromF (t : unit) : obj list = []
            Conv.make toF fromF |> make
        | [ t ] ->
            Reflection.invokeStaticMethod <@ makeUntypedInnerSingle @> ts [| |]
            |> unbox
        | _ ->
            let tupleType = ts |> Array.ofList |> FSharpType.MakeTupleType
            Reflection.invokeStaticMethod <@ makeUntypedInner @> [| tupleType |] [| |]
            |> unbox

    and makeUntypedInnerSingle<'a> () : ObjListTupleConvCrate =
        let toF os = os |> List.exactlyOne |> unbox<'a>
        let fromF a = [ box a ]
        Conv.make toF fromF |> make

    and makeUntypedInner<'tuple> () : ObjListTupleConvCrate =
        let tupleType = typeof<'tuple>
        let reader = FSharpValue.PreComputeTupleReader tupleType
        let constructor = FSharpValue.PreComputeTupleConstructor tupleType
        let toF = Array.ofList >> constructor >> unbox<'tuple>
        let fromF = box >> reader >> List.ofArray
        Conv.make toF fromF |> make


type 'tuple TupleConvCrate = abstract member Apply : TupleConvCrateEvaluator<'tuple, 'ret> -> 'ret
and TupleConvCrateEvaluator<'tuple, 'ret> = abstract member Eval : Conv<'tuple, 'b HList> -> 'ret

[<RequireQualifiedAccess>]
module TupleConvCrate =

    let make (conv : Conv<'a, 'b HList>) : 'a TupleConvCrate =
            { new TupleConvCrate<_> with
                member __.Apply e = e.Eval conv
            }

    let tryMake () : 'a TupleConvCrate option =

        let t = typeof<'a>

        match t with
        | Tuple ts ->
            let crate = ts |> ObjListHListConvCrate.makeUntyped
            crate.Apply
                { new ObjListHListConvCrateEvaluator<_> with
                    member __.Eval conv =

                        let reader = FSharpValue.PreComputeTupleReader t
                        let toF (tuple : 'a) : _ HList =
                            tuple |> reader |> List.ofArray |> conv.To

                        let constructor = FSharpValue.PreComputeTupleConstructor t
                        let fromF (hList : _ HList) : 'a =
                            hList |> conv.From |> Array.ofList |> constructor |> unbox

                        Conv.make toF fromF |> make |> Some
                }
        | _ ->
            None


type 'a RecordConvCrate = abstract member Apply : RecordConvCrateEvaluator<'a, 'ret> -> 'ret
and RecordConvCrateEvaluator<'a, 'ret> = abstract member Eval : string list -> Conv<'a, 'b HList> -> 'ret

[<RequireQualifiedAccess>]
module RecordConvCrate =

    let make (names : string list) (conv : Conv<'a, 'b HList>) : 'a RecordConvCrate =
            { new RecordConvCrate<_> with
                member __.Apply e = e.Eval names conv
            }

    let tryMake () : 'a RecordConvCrate option =

        let t = typeof<'a>

        match t with
        | Record fs ->
            let crate = fs |> List.map snd |> ObjListHListConvCrate.makeUntyped
            crate.Apply
                { new ObjListHListConvCrateEvaluator<_> with
                    member __.Eval conv =

                        let reader = FSharpValue.PreComputeRecordReader(t, true)
                        let toF (record : 'a) : _ HList =
                            record |> reader |> List.ofArray |> conv.To

                        let constructor = FSharpValue.PreComputeRecordConstructor(t, true)
                        let fromF (hList : _ HList) : 'a =
                            hList |> conv.From |> Array.ofList |> constructor |> unbox

                        make (fs |> List.map fst) (Conv.make toF fromF)
                }
                |> Some
        | _ ->
            None

type private UnionCaseConvCrate = abstract member Apply : UnionCaseConvCrateEvaluator<'ret> -> 'ret
and private UnionCaseConvCrateEvaluator<'ret> = abstract member Eval : Conv<UnionCaseInfo * obj list, 'a HUnion> -> 'a HUnionTail -> 'ret

[<RequireQualifiedAccess>]
module private UnionCaseConvCrate =

    let make conv tail =
        { new UnionCaseConvCrate with
            member __.Apply e = e.Eval conv tail
        }

    let convCrateForCase (case : UnionCaseInfo) =
        let caseFields = case.GetFields () |> Seq.map (fun f -> f.PropertyType) |> List.ofSeq
        ObjListTupleConvCrate.makeUntyped caseFields

    let rec makeUntyped (cases : UnionCaseInfo list) : UnionCaseConvCrate =
        match cases with
        | [] -> raise Unreachable
        | [case] ->
            (convCrateForCase case).Apply
                { new ObjListTupleConvCrateEvaluator<_> with
                    member __.Eval conv =
                        let toF (_, os) = os |> conv.To |> HUnion.make HUnionTail.empty
                        let fromF union =
                            match HUnion.split union with
                            | Choice1Of2 tuple -> case, conv.From tuple
                            | Choice2Of2 _ -> raise Unreachable
                        make (Conv.make toF fromF) (HUnionTail.empty |> HUnionTail.extend)
                }
        | case::cases ->
            (convCrateForCase case).Apply
                { new ObjListTupleConvCrateEvaluator<_> with
                    member __.Eval caseConv =
                        let crate = makeUntyped cases
                        crate.Apply
                            { new UnionCaseConvCrateEvaluator<_> with
                                member __.Eval unionConv tail =
                                    let toF (c, os) =
                                        if c = case then
                                            os |> caseConv.To |> HUnion.make tail
                                        else
                                            (c, os) |> unionConv.To |> HUnion.extend
                                    let fromF union =
                                        match HUnion.split union with
                                        | Choice1Of2 tuple -> case, caseConv.From tuple
                                        | Choice2Of2 union -> unionConv.From union
                                    make (Conv.make toF fromF) (tail |> HUnionTail.extend)
                            }
                }


type 'a UnionConvCrate = abstract member Apply : UnionConvCrateEvaluator<'a, 'ret> -> 'ret
and UnionConvCrateEvaluator<'a, 'ret> = abstract member Eval : string list -> Conv<'a, 'b HUnion> -> 'ret

[<RequireQualifiedAccess>]
module UnionConvCrate =

    let make (names : string list) (conv : Conv<'a, 'b HUnion>) : 'a UnionConvCrate =
            { new UnionConvCrate<_> with
                member __.Apply e = e.Eval names conv
            }

    let tryMake () : 'a UnionConvCrate option =

        let t = typeof<'a>

        match t with
        | Union cases ->
            let crate = cases |> UnionCaseConvCrate.makeUntyped
            crate.Apply
                { new UnionCaseConvCrateEvaluator<_> with
                    member __.Eval conv tail =

                        let toF (union : 'a) : _ HUnion =
                            let case, os = FSharpValue.GetUnionFields(union, t, true)
                            conv.To (case, os |> List.ofSeq)

                        let fromF (hList : _ HUnion) : 'a =
                            let case, os = hList |> conv.From
                            FSharpValue.MakeUnion(case, os |> Array.ofList) |> unbox

                        let names = cases |> Seq.map (fun c -> c.Name) |> List.ofSeq

                        make names (Conv.make toF fromF)
                }
                |> Some
        | _ ->
            None
