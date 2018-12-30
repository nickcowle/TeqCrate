namespace TeqCrate

open TypeEquality

module Patterns =

    type 'a Typ = Typ of unit

    let typ<'a> : 'a Typ = Typ ()

    let (|Teq|_|) (_ : 'b Typ) (_ : 'a Typ) : Teq<'a, 'b> option =
        Teq.tryMake

    let (|Array|_|) (_ : 'a Typ) : 'a ArrayTeqCrate option =
        ArrayTeqCrate.tryMake ()

    let (|List|_|) (_ : 'a Typ) : 'a ListTeqCrate option =
        ListTeqCrate.tryMake ()

    let (|Seq|_|) (_ : 'a Typ) : 'a SeqTeqCrate option =
        SeqTeqCrate.tryMake ()

    let (|Set|_|) (_ : 'a Typ) : 'a SetTeqCrate option =
        SetTeqCrate.tryMake ()

    let (|Map|_|) (_ : 'a Typ) : 'a MapTeqCrate option =
        MapTeqCrate.tryMake ()

    let (|Dictionary|_|) (_ : 'a Typ) : 'a DictionaryTeqCrate option =
        DictionaryTeqCrate.tryMake ()

    let (|ResizeArray|_|) (_ : 'a Typ) : 'a ResizeArrayTeqCrate option =
        ResizeArrayTeqCrate.tryMake ()

    let (|Fun|_|) (_ : 'a Typ) : 'a FunTeqCrate option =
        FunTeqCrate.tryMake ()

    let (|Pair|_|) (_ : 'a Typ) : 'a PairTeqCrate option =
        PairTeqCrate.tryMake ()

    let (|Triple|_|) (_ : 'a Typ) : 'a TripleTeqCrate option =
        TripleTeqCrate.tryMake ()

    let (|Tuple|_|) (_ : 'a Typ) : 'a TupleConvCrate option =
        TupleConvCrate.tryMake ()

    let (|Record|_|) (_ : 'a Typ) : 'a RecordConvCrate option =
        RecordConvCrate.tryMake ()

    let (|Union|_|) (_ : 'a Typ) : 'a UnionConvCrate option =
        UnionConvCrate.tryMake ()
