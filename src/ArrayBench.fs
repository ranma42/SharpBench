// Benchmark for non-inline ArrayModule functions which accept functions as arguments.
// The list has been found with:
// grep val src/fsharp/FSharp.Core/array.fsi | grep -v inline | grep '('
//
// The Impl.Array module implements inline and/or improved versions of
// most of these functions.
//
// The Bench module exercises these functions on simple arrays of ints

namespace ArrayBench
  open SharpBench.Core

  module Impl =
    module Array =
      let inline permute indexMap (arr : _[]) =
        let len = arr.Length
        let res = Array.zeroCreate len
        let inv = Array.zeroCreate len
        for i = 0 to len-1 do
          let j = indexMap i
          if j < 0 || j >= len then
            failwith "doh"
          else if inv.[j] <> 0uy then
            failwith "doh2"
          else
            inv.[j] <- 1uy
            res.[j] <- arr.[i]
        res

      let inline mapi f (arr : _[]) =
        let len = arr.Length
        let res = Array.zeroCreate len
        for i = 0 to len-1 do
          res.[i] <- f i arr.[i]
        res

      let inline mapi2 f (a1 : _[]) (a2 : _[]) =
        let len = a1.Length
        if len <> a2.Length then failwith "doh"
        let res = Array.zeroCreate len
        for i = 0 to len-1 do
          res.[i] <- f i a1.[i] a2.[i]
        res

      let inline map2 f (a1 : _[]) (a2 : _[]) =
        let len = a1.Length
        if len <> a2.Length then failwith "doh"
        let res = Array.zeroCreate len
        for i = 0 to len-1 do
          res.[i] <- f a1.[i] a2.[i]
        res

      let inline partition f (array: _[]) =
        let res1 = new System.Collections.Generic.List<_>() // ResizeArray
        let res2 = new System.Collections.Generic.List<_>() // ResizeArray
        let len = array.Length
        for i = 0 to len-1 do
          let x = array.[i]
          if f x then
            res1.Add(x)
          else
            res2.Add(x)
        res1.ToArray(), res2.ToArray()

      let inline filter f (array: _[]) =
        let res = new System.Collections.Generic.List<_>()
        let len = array.Length
        for i = 0 to len-1 do
          let x = array.[i]
          if f x then
            res.Add(x)
        res.ToArray()

      let inline iter2 f (a1: _[]) (a2: _[]) =
        let len = a1.Length
        if len <> a2.Length then failwith "doh"
        for i = 0 to len-1 do
          f a1.[i] a2.[i]

      let inline iteri f (a1: _[]) =
        let len = a1.Length
        for i = 0 to len-1 do
          f i a1.[i]

      let inline iteri2 f (a1: _[]) (a2: _[]) =
        let len = a1.Length
        if len <> a2.Length then failwith "doh"
        for i = 0 to len-1 do
          f i a1.[i] a2.[i]

      let inline scan<'T,'State> f (acc:'State) (array : 'T[]) =
        let mutable state = acc
        let len = array.Length
        let res = Array.zeroCreate (len+1)
        for i = 0 to len-1 do
          res.[0] <- state
          state <- f state array.[i]
        res.[len] <- state
        res

      let inline scanBack<'T,'State> f (array : 'T[]) (acc:'State) =
        let mutable state = acc
        let len = array.Length
        let res = Array.zeroCreate (len+1)
        res.[len-1] <- state
        for i = len-1 downto 0 do
          state <- f array.[i] state
          res.[i] <- state
        res

      let inline private internalFindIndex f (array: _[]) =
        let len = array.Length
        let mutable i = 0
        let mutable r = -1
        while i < len do
          if f array.[i] then
            r <- i
            i <- len
          else
            i <- i + 1
        r

      let inline exists2 f (a1: _[]) (a2: _[]) =
        let len = a1.Length
        if len <> a2.Length then failwith "doh"
        let mutable i = 0
        let mutable r = false
        while i < len do
          if f a1.[i] a2.[i] then
            r <- true
            i <- len
          else
            i <- i + 1
        r

      let inline forall f (a1: _[]) =
        let len = a1.Length
        let mutable i = 0
        let mutable r = true
        while i < len do
          if f a1.[i] then
            i <- i + 1
          else
            r <- false
            i <- len
        r

      let inline forall2 f (a1: _[]) (a2: _[]) =
        let len = a1.Length
        if len <> a2.Length then failwith "doh"
        let mutable i = 0
        let mutable r = true
        while i < len do
          if f a1.[i] a2.[i] then
            i <- i + 1
          else
            r <- false
            i <- len
        r

      let inline exists f (array: _[]) =
        let i = internalFindIndex f array
        i >= 0

      let inline findIndex f (array: _[]) =
        let i = internalFindIndex f array
        if i < 0 then
          failwith "doh"
        else
          i

      let inline find f (array: _[]) =
        array.[findIndex f array]

      let inline tryPick f (array: _[]) =
        let len = array.Length
        let mutable i = 0
        let mutable r = None
        while i < len do
          let p = f array.[i]
          match p with
          | Some _ -> r <- p
                      i <- len
          | None ->  i <- i + 1
        r

      let inline pick f (array: _[]) =
        match tryPick f array with
        | None -> failwith "doh"
        | Some x -> x

      let inline tryFindIndex f (array: _[]) =
        let i = internalFindIndex f array
        if i < 0 then
          None
        else
          i |> Some

      let inline tryFind f (array: _[]) =
        let i = internalFindIndex f array
        if i < 0 then
          None
        else
          array.[i] |> Some

      let inline fold2 f acc (a1:_[]) (a2:_[]) =
        let len = a1.Length
        if len <> a2.Length then failwith "doh"
        let mutable state = acc
        for i = 0 to len - 1 do
            state <- f state a1.[i] a2.[i]
        state

      let inline foldBack2 f (a1:_[]) (a2:_[]) acc =
        let len = a1.Length
        if len <> a2.Length then failwith "doh"
        let mutable state = acc
        for i = len-1 downto 0 do
            state <- f a1.[i] a2.[i] state
        state

      let inline foldFrom<'T,'State> (f : 'State -> 'T -> 'State) (acc: 'State) (array:'T[]) start =
        let mutable state = acc
        let len = array.Length
        for i = start to len - 1 do
            state <- f state array.[i]
        state

      let inline foldBackFrom<'T,'State> (f : 'T -> 'State -> 'State) (acc: 'State) (array:'T[]) start =
        let mutable state = acc
        for i = start downto 0 do
            state <- f array.[i] state
        state

      let inline fold<'T,'State> f (acc: 'State) (array:'T[]) =
        foldFrom f acc array 0

      let inline foldBack<'T,'State> f (array:'T[]) (acc: 'State) =
        foldBackFrom f acc array (array.Length-1)

      let inline choose f (array: _[]) =
        let s = new System.Collections.Generic.List<_>()
        let len = array.Length
        for i = 0 to len-1 do
          match f array.[i] with
          | None -> ()
          | Some b -> s.Add(b)
        s.ToArray()

      let inline reduce f (array : _[]) =
        let len = array.Length
        if len = 0 then
          failwith "invalid argument"
        else
          foldFrom f array.[0] array 1

      let inline reduceBack f (array : _[]) =
        let len = array.Length
        if len = 0 then
          failwith "invalid argument"
        else
          foldBackFrom f array.[len-1] array (len-2)

  module Bench =
    let private smallLen = 128
    let private midLen = 1024
    let private bigLen = 16384
    let private a1 = Array.create bigLen 1
    let private a2 = Array.init bigLen (fun x -> x % 3)
    let private smalla = Array.create smallLen 1
    let private aa = Array.init midLen (fun _ -> a1)
#if CORE
#else
    open Impl
#endif

    let inline private opt x =
      if x > 100 then
        Some x
      else
        None
    let inline private op x y = Checked.(+) x y
    let inline private op3 s x y = x * y |> Checked.(+) s
    let inline private ignore2 x y = ()
    let inline private ignore3 x y z = ()

    [<Bench("Dummy")>]
    let init () =
      a2.[a2.Length - 1] <- 10000
      a1.[0] + a2.[0] + aa.[0].[0] + smalla.[0] |> ignore

    [<Bench("Array.choose")>]
    let ArrayChoose() =
      Array.choose opt a2
      |> ignore

    [<Bench("Array.collect")>]
    let ArrayCollect() =
      Array.collect id aa
      |> ignore

    [<Bench("Array.exists")>]
    let ArrayExists() =
      Array.exists ((>) 0) a2
      |> ignore

    [<Bench("Array.exists2")>]
    let ArrayExists2() =
      Array.exists2 (<>) a2 a2
      |> ignore

    [<Bench("Array.filter")>]
    let ArrayFilter() =
      Array.filter ((<>) 0) a2
      |> ignore

    [<Bench("Array.find")>]
    let ArrayFind() =
      Array.find ((<) 100) a2
      |> ignore

    [<Bench("Array.findIndex")>]
    let ArrayFindIndex() =
      Array.findIndex ((<) 100) a2
      |> ignore

    [<Bench("Array.fold")>]
    let ArrayFold() =
      Array.fold op LanguagePrimitives.GenericZero<_> a2
      |> ignore

    [<Bench("Array.fold2")>]
    let ArrayFold2() =
      Array.fold2 op3 LanguagePrimitives.GenericZero<_> a1 a2
      |> ignore

    [<Bench("Array.foldBack")>]
    let ArrayFoldBack() =
      Array.foldBack op a2 LanguagePrimitives.GenericZero<_>
      |> ignore

    [<Bench("Array.foldBack2")>]
    let ArrayFoldBack2() =
      Array.foldBack2 op3 a1 a2 LanguagePrimitives.GenericZero<_>
      |> ignore

    [<Bench("Array.forall")>]
    let ArrayForAll() =
      Array.forall ((<=) 0) a2
      |> ignore

    [<Bench("Array.forall2")>]
    let ArrayForAll2() =
      Array.forall2 (=) a2 a2
      |> ignore

    [<Bench("Array.iter2")>]
    let ArrayIter2() =
      Array.iter2 ignore2 a1 a2
      |> ignore

    [<Bench("Array.iteri")>]
    let ArrayIterI() =
      Array.iteri ignore2 a2
      |> ignore

    [<Bench("Array.iteri2")>]
    let ArrayIterI2() =
      Array.iteri2 ignore3 a1 a2
      |> ignore

    [<Bench("Array.map2")>]
    let ArrayMap2() =
      Array.map2 op a1 a2
      |> ignore

    [<Bench("Array.mapi")>]
    let ArrayMapI() =
      Array.mapi op a2
      |> ignore

    [<Bench("Array.mapi2")>]
    let ArrayMapI2() =
      Array.mapi2 op3 a1 a2
      |> ignore

    [<Bench("Array.partition")>]
    let ArrayPartition() =
      Array.partition ((<>) 1) a2
      |> ignore

    [<Bench("Array.permute")>]
    let ArrayPermute() =
      Array.permute (fun i -> (i + 17) % bigLen) a2
      |> ignore

    [<Bench("Array.pick")>]
    let ArrayPick() =
      Array.pick opt a2
      |> ignore

    [<Bench("Array.reduce")>]
    let ArrayReduce() =
      Array.reduce op a2
      |> ignore

    [<Bench("Array.reduceBack")>]
    let ArrayReduceBack() =
      Array.reduceBack op a2
      |> ignore

    [<Bench("Array.scan")>]
    let ArrayScan() =
      Array.scan op LanguagePrimitives.GenericZero<_> a2
      |> ignore

    [<Bench("Array.scanBack")>]
    let ArrayScanBack() =
      Array.scanBack op a2 LanguagePrimitives.GenericZero<_>
      |> ignore

    [<Bench("Array.tryFind")>]
    let ArrayTryFind() =
      Array.tryFind ((<) 100) a2
      |> ignore

    [<Bench("Array.tryFindIndex")>]
    let ArrayTryFindIndex() =
      Array.tryFindIndex ((<) 100) a2
      |> ignore

    [<Bench("Array.tryPick")>]
    let ArrayTryPick() =
      Array.tryPick opt a2
      |> ignore
