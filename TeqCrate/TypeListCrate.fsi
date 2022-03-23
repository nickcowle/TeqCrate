﻿namespace TeqCrate

open HCollections
open System

/// A universally quantified function that takes a TypeList and returns a value of type 'ret
type TypeListEvaluator<'ret> =
    abstract member Eval: 'ts TypeList -> 'ret

/// An encoding of an existentially quantified TypeList.
/// Given a TypeListEvaluator, it will invoke it with the TypeList
/// that it holds and will return the result.
type TypeListCrate =
    abstract member Apply: TypeListEvaluator<'ret> -> 'ret

/// An encoding of an existentially quantified TypeList.
/// Given a TypeListEvaluator, it will invoke it with the TypeList
/// that it holds and will return the result.
module TypeListCrate =

    /// Given a TypeList, creates a TypeListCrate that holds the TypeList
    val make: 'ts TypeList -> TypeListCrate

    /// Given a list of runtime Types, creates the corresponding
    /// TypeList and then wraps it in a TypeListCrate to hide the
    /// generic type parameter.
    val makeUntyped: Type list -> TypeListCrate
