namespace Exercises

  open System

  module MySeq =

    let sample =
      seq { for i in 1 .. 10 -> i }

    let sample2 =
      seq { for i in 11 .. 20 -> i }

    let printSeq (print: 'a -> unit) (s: seq<'a>) =
      s
      |> Seq.iter(fun x -> print x) // use a generic print function so that we can reuse printSeq anyway we like
      s // make sure to put s here otherwise a type unit will be returned

    let filterSeq (f: int -> int -> bool) v (s: seq<int>) =
      s
      |> Seq.filter(fun x -> f x v)

    let appendToSeq (a: seq<int>) (s: seq<int>) =
      s
      |> Seq.append(a) 

    let createTuples (s: seq<int>) =
      s
      |> Seq.map(fun x -> // map can transform a sequence to a different seq
        (x,x)
      )

    let chooseFirstColInTupleFromSeq (s: seq<int*int>) =
      s
      |> Seq.choose(fun (x,_) -> Some x)
    
    let reduceSeq (s: seq<int>) =
      s
      |> Seq.reduce(fun x -> (+) x) // this will add up all values in seq

  module SeqTest =

    let test =
      MySeq.sample
      |> MySeq.filterSeq(fun (x: int) -> (<) x) 6
      |> MySeq.appendToSeq(MySeq.sample2)
      |> MySeq.filterSeq(fun (x: int) -> (>) x) 15
      |> MySeq.printSeq(fun x -> printfn "%d" x)
      |> MySeq.createTuples
      |> MySeq.printSeq(fun x -> printfn "(%d,%d)" <|| x)
      |> MySeq.chooseFirstColInTupleFromSeq
      |> MySeq.reduceSeq
      |> printfn "%d"