﻿namespace TeqCrate

open HCollections

/// The type of values that act on an TupleConvCrate.
/// An encoding of a universally quantified function that takes a TypeList and a converter between
/// first type parameter 'tuple and a 'ts HList for any 'ts and returns a value of type 'ret
type TupleConvEvaluator<'tuple, 'ret> = abstract member Eval : 'ts TypeList -> Conv<'tuple, 'ts HList> -> 'ret

/// An encoding of an existentially quantified converter between 'tuple and 'ts HList for some 'ts.
/// Given a TupleConvEvaluator, it will invoke it with the TypeList and HList that it holds and will return the result.
type 'tuple TupleConvCrate = abstract member Apply : TupleConvEvaluator<'tuple, 'ret> -> 'ret

/// An encoding of an existentially quantified converter between 'tuple and 'ts HList for some 'ts.
/// Given a TupleConvEvaluator, it will invoke it with the TypeList and HList that it holds and will return the result.
module TupleConvCrate =

    /// For any type 'tuple, checks to see if 'tuple is actually a tuple type 'a1 * 'a1 ... * 'an for some 'a1 ... 'an.
    /// If it is, creates a converter Conv<'tuple, 'a1 * 'a2 * ... 'an> and then wraps it in a crate.
    /// Otherwise, returns None.
    val tryMake : unit -> 'tuple TupleConvCrate option


/// The type of values that act on an RecordConvCrate.
/// An encoding of a universally quantified function that takes a list of strings corresponding to
/// the names of the fields of the record, a TypeList and a converter between
/// first type parameter 'record and a 'ts HList for any 'ts and returns a value of type 'ret
type RecordConvEvaluator<'record, 'ret> = abstract member Eval : string list -> 'ts TypeList -> Conv<'record, 'ts HList> -> 'ret

/// An encoding of an existentially quantified converter between 'record and 'ts HList for some 'ts.
/// Given a RecordConvEvaluator, it will invoke it with the field names, TypeList and HList that it holds and will return the result.
type 'record RecordConvCrate = abstract member Apply : RecordConvEvaluator<'record, 'ret> -> 'ret

/// An encoding of an existentially quantified converter between 'record and 'ts HList for some 'ts.
/// Given a RecordConvEvaluator, it will invoke it with the field names, TypeList and HList that it holds and will return the result.
module RecordConvCrate =

    /// For any type 'record, checks to see if 'record is actually an F# record type.
    /// If it is, creates a converter Conv<'record, 'ts HList> and then wraps it in a crate.
    /// Otherwise, returns None.
    val tryMake : unit -> 'record RecordConvCrate option


/// The type of values that act on an UnionConvCrate.
/// An encoding of a universally quantified function that takes a list of strings corresponding to
/// the names of the union cases of the discriminated union, a TypeList and a converter between
/// first type parameter 'union and a 'ts HUnion for any 'ts and returns a value of type 'ret
type UnionConvEvaluator<'union, 'ret> = abstract member Eval : string list -> 'ts TypeList -> Conv<'union, 'ts HUnion> -> 'ret

/// An encoding of an existentially quantified converter between 'union and 'ts HUnion for some 'ts.
/// Given a UnionConvEvaluator, it will invoke it with the case names, TypeList and HUnion that it holds and will return the result.
type 'union UnionConvCrate = abstract member Apply : UnionConvEvaluator<'union, 'ret> -> 'ret

/// An encoding of an existentially quantified converter between 'union and 'ts HUnion for some 'ts.
/// Given a UnionConvEvaluator, it will invoke it with the case names, TypeList and HUnion that it holds and will return the result.
module UnionConvCrate =

    /// For any type 'union, checks to see if 'union is actually an F# discriminated union type.
    /// If it is, creates a converter Conv<'union, 'ts HUnion> and then wraps it in a crate.
    /// Otherwise, returns None.
    val tryMake : unit -> 'union UnionConvCrate option


/// The type of values that act on an SumOfProductsConvCrate.
/// An encoding of a universally quantified function that takes a list of strings corresponding to
/// the names of the union cases of the discriminated union, a TypeListList and a converter between
/// first type parameter 'union and a 'tss SumOfProducts for any 'tss and returns a value of type 'ret
type SumOfProductsConvEvaluator<'union, 'ret> = abstract member Eval : string list -> 'tss TypeListList -> Conv<'union, 'tss SumOfProducts> -> 'ret

/// An encoding of an existentially quantified converter between 'union and 'tss SumOfProducts for some 'tss.
/// Given a SumOfProductsConvEvaluator, it will invoke it with the case names, TypeList and SumOfProducts that it holds and will return the result.
type 'union SumOfProductsConvCrate = abstract member Apply : SumOfProductsConvEvaluator<'union, 'ret> -> 'ret

/// An encoding of an existentially quantified converter between 'union and 'tss SumOfProducts for some 'tss.
/// Given a SumOfProductsConvEvaluator, it will invoke it with the case names, TypeList and SumOfProducts that it holds and will return the result.
module SumOfProductsConvCrate =

    /// For any type 'union, checks to see if 'union is actually an F# discriminated union type.
    /// If it is, creates a converter Conv<'union, 'tss SumOfProducts> and then wraps it in a crate.
    /// Otherwise, returns None.
    val tryMake : unit -> 'union SumOfProductsConvCrate option
