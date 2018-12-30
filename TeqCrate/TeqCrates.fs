namespace TeqCrate

open System.Collections.Generic
open TeqCrate.TypePatterns
open TypeEquality


type 'a ArrayTeqCrate = abstract member Apply : ArrayTeqCrateEvaluator<'a, 'ret> -> 'ret
and ArrayTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b array> -> 'ret

[<RequireQualifiedAccess>]
module ArrayTeqCrate =

    let make () =
            { new ArrayTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Array e ->
            let c = Reflection.invokeStaticMethod <@ make @> [| e |] [| |]
            c |> unbox<'a ArrayTeqCrate> |> Some
        | _ ->
            None


type 'a ListTeqCrate = abstract member Apply : ListTeqCrateEvaluator<'a, 'ret> -> 'ret
and ListTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b list> -> 'ret

[<RequireQualifiedAccess>]
module ListTeqCrate =

    let make () =
            { new ListTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<_ list> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a ListTeqCrate> |> Some
        | _ -> None


type 'a SeqTeqCrate = abstract member Apply : SeqTeqCrateEvaluator<'a, 'ret> -> 'ret
and SeqTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b seq> -> 'ret

[<RequireQualifiedAccess>]
module SeqTeqCrate =

    let make () =
            { new SeqTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<_ seq> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a SeqTeqCrate> |> Some
        | _ -> None


type 'a SetTeqCrate = abstract member Apply : SetTeqCrateEvaluator<'a, 'ret> -> 'ret
and SetTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b Set> -> 'ret

[<RequireQualifiedAccess>]
module SetTeqCrate =

    let make<'a when 'a : comparison> () =
            { new SetTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<_ Set> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a SetTeqCrate> |> Some
        | _ -> None


type 'a MapTeqCrate = abstract member Apply : MapTeqCrateEvaluator<'a, 'ret> -> 'ret
and MapTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, Map<'k, 'v>> -> 'ret

[<RequireQualifiedAccess>]
module MapTeqCrate =

    let make<'k, 'v when 'k : comparison> () =
            { new MapTeqCrate<Map<'k, 'v>> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<Map<_,_>> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a MapTeqCrate> |> Some
        | _ -> None


type 'a DictionaryTeqCrate = abstract member Apply : DictionaryTeqCrateEvaluator<'a, 'ret> -> 'ret
and DictionaryTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, Dictionary<'k, 'v>> -> 'ret

[<RequireQualifiedAccess>]
module DictionaryTeqCrate =

    let make () =
            { new DictionaryTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<Dictionary<_,_>> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a DictionaryTeqCrate> |> Some
        | _ -> None


type 'a ResizeArrayTeqCrate = abstract member Apply : ResizeArrayTeqCrateEvaluator<'a, 'ret> -> 'ret
and ResizeArrayTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b ResizeArray> -> 'ret

[<RequireQualifiedAccess>]
module ResizeArrayTeqCrate =

    let make () =
            { new ResizeArrayTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Generic (t, ts) when t = typedefof<_ ResizeArray> ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a ResizeArrayTeqCrate> |> Some
        | _ -> None


type 'a FunTeqCrate = abstract member Apply : FunTeqCrateEvaluator<'a, 'ret> -> 'ret
and FunTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b -> 'c> -> 'ret

[<RequireQualifiedAccess>]
module FunTeqCrate =

    let make () =
            { new FunTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let ofTeq teq =
        { new FunTeqCrate<_> with
            member __.Apply e = e.Eval teq
        }

    let tryMake () =
        match typeof<'a> with
        | Fun (dom, ran) ->
            Reflection.invokeStaticMethod <@ make @> [| dom ; ran |] [| |]
            |> unbox<'a FunTeqCrate> |> Some
        | _ -> None


type 'a PairTeqCrate = abstract member Apply : PairTeqCrateEvaluator<'a, 'ret> -> 'ret
and PairTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b * 'c> -> 'ret

[<RequireQualifiedAccess>]
module PairTeqCrate =

    let make () =
            { new PairTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Tuple ts when ts.Length = 2 ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a PairTeqCrate> |> Some
        | _ -> None


type 'a TripleTeqCrate = abstract member Apply : TripleTeqCrateEvaluator<'a, 'ret> -> 'ret
and TripleTeqCrateEvaluator<'a, 'ret> = abstract member Eval : Teq<'a, 'b * 'c * 'd> -> 'ret

[<RequireQualifiedAccess>]
module TripleTeqCrate =

    let make () =
            { new TripleTeqCrate<_> with
                member __.Apply e = e.Eval Teq.refl
            }

    let tryMake () =
        match typeof<'a> with
        | Tuple ts when ts.Length = 3 ->
            Reflection.invokeStaticMethod <@ make @> ts [| |]
            |> unbox<'a TripleTeqCrate> |> Some
        | _ -> None
