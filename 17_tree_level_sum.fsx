// This tutorial is pulled from a code challenge
//   here: https://codefights.com/interview/oqQqEcGhoB2wHswx6 .
// This has been used as an interview question.
// The goal is, given a string representation of a binary tree
//   as such: (3()()), sum all nodes at the kth level.
module TreeLevelSummation =

  let loadTrees fileName =
    System.IO.File.ReadAllLines fileName

  let treeLevelSum (tree:string) (k:int) =
    let mutable nums = [||] : string[]
    let mutable index = -1
    let mutable level = -1
    (tree.ToCharArray())
    |> Array.iteri(fun i c ->
      if c = '(' then
        level <- level + 1
      elif c = ')' then
        level <- level - 1
      else
        if level = k then
          if tree.[i-1] = '(' || tree.[i-1] = ')' then index <- index + 1
          if index >= (Array.length nums) then nums <- Array.append nums [| "" |]
          nums.[index] <- nums.[index] + (string c)
    )
    nums |> Array.sumBy(int)

let inputs = TreeLevelSummation.loadTrees "./support/treeLevelSum.txt"

inputs
|> Array.iter(fun input ->
  let tree,k,expected =
    let res = (input.Split(' '))
    res.[0], int(res.[1]), int(res.[2])
  let actual = TreeLevelSummation.treeLevelSum tree k
  printfn "Expected: %d, Actual: %d" expected actual
)