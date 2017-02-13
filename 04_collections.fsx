
module FSharpCollections =

  let arrayFold (arr:(int*int)[]) =
    let fold (a:int*int) (b:int*int) =
      let a1,a2 = a
      let b1,b2 = b
      a1+b1,a2+b2
    [| arr |> Array.fold fold (0,0) |]

  let listIncr l v =
    l
    |> List.map(fun x -> x + v)

  let printCollection (c:obj) =
    let printEle e = printfn "%d" e
    match c with
    | :? array<int*int> as a -> a |> Array.iter(fun (a,b) -> printEle a)
    | :? list<int> as l -> l |> List.iter printEle
    | :? seq<int> as s -> s |> Seq.iter printEle
    | _ -> printfn "No match"

FSharpCollections.arrayFold [|(5,6);(7,8)|]
|> FSharpCollections.printCollection
FSharpCollections.listIncr [ for i in 0..9 -> i ]
|> FSharpCollections.printCollection
FSharpCollections.printCollection (seq { for i in 0..9 -> i })