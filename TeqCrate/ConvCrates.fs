namespace TeqCrate

open Microsoft.FSharp.Reflection
open System
open TeqCrate.TypePatterns

[<NoComparison>]
[<NoEquality>]
type Conv<'a, 'b> =
    {
        To : 'a -> 'b
        From : 'b -> 'a
    }

[<RequireQualifiedAccess>]
module private Conv =

    let make toF fromF =
        { To = toF ; From = fromF }


type private ObjListConvCrate = abstract member Apply : ObjListConvCrateEvaluator<'ret> -> 'ret
and private ObjListConvCrateEvaluator<'ret> = abstract member Eval : Conv<obj list, 'a HList> -> 'ret

[<RequireQualifiedAccess>]
module private ObjListConvCrate =

    let make (conv : Conv<obj list, 'a HList>) : ObjListConvCrate =
        { new ObjListConvCrate with
            member __.Apply e = e.Eval conv
        }

    let rec makeUntyped (ts : Type list) : ObjListConvCrate =
        match ts with
        | [] ->
            Conv.make (fun _ -> HList.empty) (fun _ -> []) |> make
        | t::ts ->
            Reflection.invokeStaticMethod <@ makeUntypedInner @> [| t |] [| ts |]
            |> unbox

    and makeUntypedInner<'t> (ts : Type list) : ObjListConvCrate =
        (makeUntyped ts).Apply
            { new ObjListConvCrateEvaluator<_> with
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


type 'a TupleConvCrate = abstract member Apply : TupleConvCrateEvaluator<'a, 'ret> -> 'ret
and TupleConvCrateEvaluator<'a, 'ret> = abstract member Eval : Conv<'a, 'b HList> -> 'ret

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
            let crate = ts |> ObjListConvCrate.makeUntyped
            crate.Apply
                { new ObjListConvCrateEvaluator<_> with
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
            let crate = fs |> List.map snd |> ObjListConvCrate.makeUntyped
            crate.Apply
                { new ObjListConvCrateEvaluator<_> with
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
        ObjListConvCrate.makeUntyped caseFields

    let rec makeUntyped (cases : UnionCaseInfo list) : UnionCaseConvCrate =
        match cases with
        | [] -> raise Unreachable
        | [case] ->
            (convCrateForCase case).Apply
                { new ObjListConvCrateEvaluator<_> with
                    member __.Eval conv =
                        let toF (_, os) = os |> conv.To |> HUnion.make HUnionTail.empty
                        let fromF union =
                            match HUnion.split union with
                            | Choice1Of2 hList -> case, conv.From hList
                            | Choice2Of2 _ -> raise Unreachable
                        make (Conv.make toF fromF) (HUnionTail.empty |> HUnionTail.extend)
                }
        | case::cases ->
            (convCrateForCase case).Apply
                { new ObjListConvCrateEvaluator<_> with
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
                                        | Choice1Of2 hList -> case, caseConv.From hList
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
