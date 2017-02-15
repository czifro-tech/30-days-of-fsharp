// This shows how to create a suffix array from the BWT of any sequence
// This uses the sequence { m, i, s, s, i, s, s, i, p, p, i }
// For the sake of ease, a '$' is appended to the sequence so that we
//  know where the end is.
// This code starts with the BWT already generated.
// Note: This code is not optimized. It is just loosely
//  implementing BWT concepts, so complexity of this
//  versus complexity of actual algorithm will differ

let bwt = ("ipssm$pissii").ToCharArray()
// Get column F of the BW Matrix
let f = bwt |> Array.sort
let n = Array.length bwt
let alpha = Array.distinct f // since f is in ascending, so will this

// In an optimized implementation this would not be needed
//  however, this is just to see how things work so we don't care
// Do not factor into the actual complexity of the algorithm
let col c' = alpha |> Array.findIndex(fun c -> c = c')

// This module contains a rank function that utilizes a wavelet tree of
//  the BWT
module WaveletTree =

  // bitSeq would not be an int[] in the optimized version
  //  instead this pairs each character with its bit value
  //  representation as a tuple. Its easier to use a tuple[]
  //  than it is two arrays
  type Node =
    {
      isLeaf    : bool;
      bitSeq    : (int*char)[];
      left      : Node option;
      right     : Node option;
      character : char;
      count     : int;
    }

    static member New bitSeq' =
      {
        isLeaf = false;
        bitSeq = bitSeq';
        left = None;
        right = None;
        character = ' ';
        count = 0;
      }

  let createWaveletTree() =

    let getBitSeq (seq':char[]) =
      let alpha' = seq' |> Array.distinct |> Array.sort
      if Array.isEmpty alpha' then
        [||]  // base case, empty array
      elif Array.length alpha' = 1 then
        seq'
        |> Array.map(fun c -> 0,c) // map each char to match the tuple schema in Node.bitSeq
      else
        // get middle position of alpha'
        let mid =
          if (Array.length alpha') % 2 = 0 then
            (Array.length alpha') / 2
          else
            int(ceil(float(Array.length alpha') / 2.0))
        seq' // map each char to match the tuple schema in Node.bitSeq
        |> Array.map(fun c ->
          if c < alpha'.[mid] then  // left gets 0
            0,c
          else  // right gets 1
            1,c
        )
  
    let rec gen (bitSeq:(int*char)[]) =
      let node = Node.New bitSeq
      // get sub sequence of 0s that go to left
      let leftSeq =
        bitSeq
        |> Array.filter(fun (i,_) -> i = 0) // filters out 1s
        // since 0s are from parent node, strip them off
        //  so that these characters can be remapped to a new
        //  bit sequence
        |> Array.map(fun (_,c) -> c)
      // apply same thing to 1s
      let rightSeq =
        bitSeq
        |> Array.filter(fun (i,_) -> i = 1)
        |> Array.map(fun (_,c) -> c)
      // base case is that no more characters are
      //  going to the right
      if Array.isEmpty rightSeq then
        {
          node with
            isLeaf = true;
            character = leftSeq.[0];
            count = Array.length bitSeq; // bitSeq should be all one char
        }
      else
        {
          node with
            // recurse left
            left = leftSeq |> getBitSeq |> gen |> Some
            // recurse right
            right = rightSeq |> getBitSeq |> gen |> Some
        }

    bwt |> getBitSeq |> gen

  let waveletTree = createWaveletTree()

  // gets the rank of c using wavelet tree
  let rank i c =
    let mutable cur = waveletTree
    let mutable j = i
    while not cur.isLeaf do
      // only interested in bit rep of char
      //  since Node is not optimized, do not factor this in
      //  time cost, treat as constant time
      let B = cur.bitSeq |> Array.map(fun (i,_) -> i)
      if B.[j] = 0 then
        // update j with rank of B[j] in B
        j <-
          let mutable k = 0
          for h in 0..j do
            if B.[h] = 0 then k <- k + 1
          k
        cur <- cur.left.Value
      else
        j <-
          let mutable k = 0
          for h in 0..j do
            if B.[h] = 1 then k <- k + 1
          k
        cur <- cur.right.Value
      j <- j - 1
    j + 1

  let createCArray() =
    // since the leaf nodes of the wavelet tree
    //  have the character and the count of the char
    //  copy that into a dictionary
    let dict = System.Collections.Generic.Dictionary<char,int>()

    let rec getNodesAsTupleArray (node:Node) =
      if node.isLeaf then
        [| (node.count,node.character) |]
      else
        Array.append (getNodesAsTupleArray node.left.Value) (getNodesAsTupleArray node.right.Value)
      
    let tuples = getNodesAsTupleArray waveletTree

    tuples
    |> Array.sortBy(fun (_,c) -> c)
    |> Array.map(fun (_,c) ->
      let numCharsLessThanC =
        tuples
        |> Array.filter(fun (_,c') -> c' < c)
        |> Array.sumBy(fun (i,_) -> i)
      numCharsLessThanC,c
    )
    |> Array.iter(fun (n,c) ->
      dict.Add(c,n)
    )
    dict

  let C = createCArray()

// This module contains a rank function that utilizes a
//  letter frequency table (matrix) of the BWT
module LFTable =
  
  let createLFTable() =
    let lf = [|
      for i in 0..n ->
        [| for j in 0..((Array.length alpha)-1) -> 0 |] // initialize matrix to 0
    |]

    for i in 1..n do // explicitly skipping row 0
      // if alpha were an array of ints and bwt was changed
      //  accordingly, this would not by necessary
      let j = col bwt.[i-1]
      for k in 0..((Array.length alpha)-1) do
        lf.[i].[k] <- lf.[i-1].[k]
      lf.[i].[j] <- lf.[i].[j] + 1
    lf

  let lfTable = createLFTable()

  let rank i c =
    let j = col bwt.[i] // if this were optimized, this would not be needed
    lfTable.[i+1].[j] // offset by one

  let createCArray() =
    let dict = System.Collections.Generic.Dictionary<char,int>()

    alpha
    |> Array.iteri(fun i c ->
      let numCharsLessThanC =
        lfTable.[n]
        |> Array.mapi(fun i' n -> i',n)
        |> Array.filter(fun (i',n) -> i' < i)
        |> Array.sumBy(fun (_,n) -> n)
      dict.Add(c,numCharsLessThanC)
    )
    dict
  let C = createCArray()

module SuffixArray =

  // rank param is a function
  // the reason for passing in rank and C
  // is so that custom implementations can be used
  // complexity in should be:
  //  Wavelet Tree => Time Cost: O(nlog|alpha|), Space Cost: O(|alpha| + nlog|alpha|)
  //  LF Table     => Time Cost: O(n), Space Cost: O(|alpha| + n * |alpha|)
  let createSA (bwt:char[]) (rank:int->char->int) (C:System.Collections.Generic.Dictionary<char,int>) =
    let sa = [| for i in 0..n-1 -> 0 |]
    let mutable j = 0
    for i in n-2 .. -1 .. 0 do
      sa.[j] <- i + 1
      let r = rank j bwt.[j]
      j <- C.[bwt.[j]] + r - 1
    sa

printfn "Creating suffix array with wavelet tree..."
printfn " SA     BWT"
(SuffixArray.createSA bwt WaveletTree.rank WaveletTree.C)
|> Array.iteri(fun i n ->
  printfn " %2d      %c" n bwt.[i]
)

printfn "Creating suffix array with lf table..."
printfn " SA     BWT"
(SuffixArray.createSA bwt LFTable.rank LFTable.C)
|> Array.iteri(fun i n ->
  printfn " %2d      %c" n bwt.[i]
)
