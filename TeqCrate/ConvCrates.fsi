namespace TeqCrate

open HCollections

type 'tuple TupleConvCrate = abstract member Apply : TupleConvCrateEvaluator<'tuple, 'ret> -> 'ret
and TupleConvCrateEvaluator<'tuple, 'ret> = abstract member Eval : 'ts TypeList -> Conv<'tuple, 'ts HList> -> 'ret

module TupleConvCrate =

    val tryMake : unit -> 'tuple TupleConvCrate option


type 'record RecordConvCrate = abstract member Apply : RecordConvCrateEvaluator<'record, 'ret> -> 'ret
and RecordConvCrateEvaluator<'record, 'ret> = abstract member Eval : string list -> 'ts TypeList -> Conv<'record, 'ts HList> -> 'ret

module RecordConvCrate =

    val tryMake : unit -> 'record RecordConvCrate option


type 'union UnionConvCrate = abstract member Apply : UnionConvCrateEvaluator<'union, 'ret> -> 'ret
and UnionConvCrateEvaluator<'union, 'ret> = abstract member Eval : string list -> 'ts TypeList -> Conv<'union, 'ts HUnion> -> 'ret

module UnionConvCrate =

    val tryMake : unit -> 'union UnionConvCrate option
