namespace TeqCrate

open HCollections

type TupleConvEvaluator<'tuple, 'ret> = abstract member Eval : 'ts TypeList -> Conv<'tuple, 'ts HList> -> 'ret
type 'tuple TupleConvCrate = abstract member Apply : TupleConvEvaluator<'tuple, 'ret> -> 'ret

module TupleConvCrate =

    val tryMake : unit -> 'tuple TupleConvCrate option


type RecordConvEvaluator<'record, 'ret> = abstract member Eval : string list -> 'ts TypeList -> Conv<'record, 'ts HList> -> 'ret
type 'record RecordConvCrate = abstract member Apply : RecordConvEvaluator<'record, 'ret> -> 'ret

module RecordConvCrate =

    val tryMake : unit -> 'record RecordConvCrate option


type UnionConvEvaluator<'union, 'ret> = abstract member Eval : string list -> 'ts TypeList -> Conv<'union, 'ts HUnion> -> 'ret
type 'union UnionConvCrate = abstract member Apply : UnionConvEvaluator<'union, 'ret> -> 'ret

module UnionConvCrate =

    val tryMake : unit -> 'union UnionConvCrate option


type SumOfProductsConvEvaluator<'union, 'ret> = abstract member Eval : string list -> 'tss TypeListList -> Conv<'union, 'tss SumOfProducts> -> 'ret
type 'union SumOfProductsConvCrate = abstract member Apply : SumOfProductsConvEvaluator<'union, 'ret> -> 'ret

module SumOfProductsConvCrate =

    val tryMake : unit -> 'union SumOfProductsConvCrate option
