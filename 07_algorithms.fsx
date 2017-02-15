

module Algorithms =

  let nextPermutation (arr:int[]) =
    let arr = arr |> Array.distinct // eliminate duplicates
    let getPivot n arr = // nested functions are allowed
      let k,_ =
        arr
        |> Array.mapi(fun i x -> if i+1<n && arr.[i+1] < x then (i,x) else (-1,-1) )
        |> Array.filter(fun (i,_) -> i <> -1)
        |> Array.maxBy(fun (i,_) -> i)
      n-k
    let getSwap n k arr =
      let j,_ = 
        arr
        |> Array.mapi(fun i x ->
          if i < k then
            (i, x - arr.[k])
          else
            (-1, -1)
        )
        |> Array.filter(fun (i,_) -> i <> -1)
        |> Array.minBy(fun (_,x) -> x)
      n-j

    let rArr = Array.rev arr
    let n = Array.length arr
    let pivot = getPivot n rArr
    let swap = getSwap n pivot rArr
    let t = arr.[pivot]
    arr.[pivot] <- arr.[swap]
    arr.[swap] <- t
    let arr = Array.rev rArr
    Array.append arr.[..pivot] (Array.rev arr.[(pivot+1)..])

  let rec fib n i j = // rec allows for function recurse
    if n <= 0 then
      [| i + j |]
    else
      Array.append [| i |] (fib (n-1) j (i+j) )

printfn "%A" (Algorithms.nextPermutation [| 1; 2; 3; 4 |])
printfn "%A" (Algorithms.fib 5 0 1)

