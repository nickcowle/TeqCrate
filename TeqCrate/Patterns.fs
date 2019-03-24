namespace TeqCrate

open TypeEquality

module Patterns =

    type 'a TType = TType of unit

    let tType<'a> : 'a TType = TType ()

    let (|Teq|_|) (_ : 'b TType) (_ : 'a TType) : Teq<'a, 'b> option =
        Teq.tryRefl<'a, 'b>

    let (|Array|_|) (_ : 'a TType) : 'a ArrayTeqCrate option =
        ArrayTeqCrate.tryMake ()

    let (|List|_|) (_ : 'a TType) : 'a ListTeqCrate option =
        ListTeqCrate.tryMake ()

    let (|Seq|_|) (_ : 'a TType) : 'a SeqTeqCrate option =
        SeqTeqCrate.tryMake ()

    let (|Set|_|) (_ : 'a TType) : 'a SetTeqCrate option =
        SetTeqCrate.tryMake ()

    let (|Map|_|) (_ : 'a TType) : 'a MapTeqCrate option =
        MapTeqCrate.tryMake ()

    let (|Dictionary|_|) (_ : 'a TType) : 'a DictionaryTeqCrate option =
        DictionaryTeqCrate.tryMake ()

    let (|ResizeArray|_|) (_ : 'a TType) : 'a ResizeArrayTeqCrate option =
        ResizeArrayTeqCrate.tryMake ()

    let (|Fun|_|) (_ : 'a TType) : 'a FunTeqCrate option =
        FunTeqCrate.tryMake ()

    let (|Pair|_|) (_ : 'a TType) : 'a PairTeqCrate option =
        PairTeqCrate.tryMake ()

    let (|Triple|_|) (_ : 'a TType) : 'a TripleTeqCrate option =
        TripleTeqCrate.tryMake ()

    let (|Tuple|_|) (_ : 'a TType) : 'a TupleConvCrate option =
        TupleConvCrate.tryMake ()

    let (|Record|_|) (_ : 'a TType) : 'a RecordConvCrate option =
        RecordConvCrate.tryMake ()

    let (|Union|_|) (_ : 'a TType) : 'a UnionConvCrate option =
        UnionConvCrate.tryMake ()
