
(*
Network flow algorithms can be used for many applications
In this tutorial, network flow will be used to play a 
tile cut game. Consider the following grids:

   WINW
   NNNI
   IIIN
   WWwN

   NINWN
   INIWI
   WWWIW
   NNNNN
   IWINN

The goal is to produce as many unique sequences of {W,I,N}.
This can be done with network flow if the capacity of an 
edge is set to one. This will gaurantee that no two
sequences share the same letters.

For the above grids, both should result in 5 unique 
sequences.

grid1           1    2      1         2      1    2
              - w -- w ---  i  -----  i  --- n -- n -
           /                              /            \
            --- w -- w ---  i  -----  i  --- n -- n ---
         /                                               \
source -  ----- w -- w ---  i  -----  i  --- n -- n -----  - sink
         \                                X              /
            --- w -- w ---  i  -----  i  --- n -- n ---
           \                                           /
              - w -- w ---  i  -----  i  --- n -- n -
                                                    /
                                             n -- n 

Key: X, /, \ mean that there is a cross over.

Notice the extra edges. We need to treat each letter as an edge,
to ensure that letters are not reused. So we will split a letter
as W1 and W2, etc. The two parts will be connected by an edge.
This mean a letter can be represented as such:
  (W1)-[WE]->(W2)
If we have already used a letter the corresponding edge
will have a flow of 1 and a residual of 0.
*)

module NetworkFlow =

  module Graph =

    type Vertex =
      {
        label : char;
        in' : Edge ref [] // references to edges coming into vertex
        out' : Edge ref [] // references to edges going out of vertex
        xy : int*int        // the x,y coord in original grid, used for querying
      }
    and Edge =
      {
        capacity : int;
        flow : int;
        residual : int;
        s : Vertex ref; // the source for the edge
        t : Vertex ref; // the terminal/sink for the edge
      }
    // our graph data structure will only contain references to source and sink
    type G =
      {
        source : Vertex ref
        sink : Vertex ref;
      }

    let constructGraph rawGrid =
      let m,n = (Array.length rawGrid),(Array.length rawGrid.[0])
      let g =
        {
          source = ref { label = '^'; in' = [||]; out' = [||]; xy = -1,-1 };
          sink = ref { label = '$'; in' = [||]; out' = [||]; xy = -2,-2 };
        }
      // Before we can place 'I' into graph, we need to connect the 
      // 'W' to source and 'N' to sink
      rawGrid
      |> Array.iteri(fun i row ->
        row
        |> Array.iteri(fun j c ->
          match c with
          | 'W' ->
            // create: (W1)-[WE]->(W2)
            let w1 = ref { label = c; in' = [||]; out' = [||]; xy = i,j }
            let w2 = ref { label = c; in' = [||]; out' = [||]; xy = i,j }
            let we = ref { capacity = 1; residual = 1; flow = 0; s = w1; t = w2 }
            // create edge between (source) and (W1)
            let swe = ref { capacity = 1; residual = 1; flow = 0; s = g.source; t = w1 }
            // connect vertices to edges
            w1 := { (!w1) with in' = [| swe |]; out' = [| we |] }
            w2 := { (!w2) with in' = [| we |] }
            g.source := { (!g.source) with out' = Array.append (!g.source).out' [| swe |] }
          | 'N' ->
            // same as W
            let n1 = ref { label = c; in' = [||]; out' = [||]; xy = i,j }
            let n2 = ref { label = c; in' = [||]; out' = [||]; xy = i,j }
            let ne = ref { capacity = 1; residual = 1; flow = 0; s = n1; t = n2 }
            let nse = ref { capacity = 1; residual = 1; flow = 0; s = n2; t = g.sink }
            n1 := { (!n1) with out' = [| ne |] }
            n2 := { (!n2) with out' = [| nse |]; in' = [| ne |] }
            g.sink := { (!g.sink) with in' = Array.append (!g.sink).in' [| nse |] }
          | _ -> ignore 0
        )
      )
      // this uses xy 
      let findIndexInG r c useSource =
        if useSource then
          (!g.source).out'
          |> Array.tryFindIndex(fun e -> (!(!e).t).xy = (r,c))
        else
          (!g.sink).in'
          |> Array.tryFindIndex(fun e -> (!(!e).s).xy = (r,c))
      // now we can add I to graph
      let dr = [| 1; 0; -1; 0 |] // consider this :    N
      let dc = [| 0; 1; 0; -1 |] //                   WIN
      rawGrid                    //                    W
      |> Array.iteri(fun i row ->
        row
        |> Array.iteri(fun j c ->
          match c with
          | 'I' ->
            // same as W and N
            let i1 = ref { label = c; in' = [||]; out' = [||]; xy = i,j }
            let i2 = ref { label = c; in' = [||]; out' = [||]; xy = i,j }
            let ie = ref { capacity = 1; residual = 1; flow = 0; s = i1; t = i2 }
            i1 := { (!i1) with out' = [|ie|] }
            i2 := { (!i2) with in' = [|ie|] }
            // we need to see if neighbors are W or N
            for k in 0..3 do
              if i+dr.[k] >= 0 && i+dr.[k] < m && j+dc.[k] >= 0 && j+dc.[k] < n then
                match rawGrid.[i+dr.[k]].[j+dc.[k]] with
                | 'W' -> 
                  // need to find correct W in g.source to connect to
                  let indexOfW = findIndexInG (i+dr.[k]) (j+dc.[k]) true
                  if indexOfW.IsSome then
                    // create (W2)-[WIE]->(I1)
                    let w2 = (!(!(!(!g.source).out'.[indexOfW.Value]).t).out'.[0]).t
                    let wie = ref { capacity = 1; residual = 1; flow = 0; s = w2; t = i1 }
                    w2 := { (!w2) with out' = Array.append (!w2).out' [| wie |] }
                    i1 := { (!i1) with in' = Array.append (!i1).in' [| wie |] }
                | 'N' ->
                  // need to find correct N in g.sink
                  let indexOfN = findIndexInG (i+dr.[k]) (j+dc.[k]) false
                  if indexOfN.IsSome then
                    // create (I2)-[INE]->(N1)
                    let n1 = (!(!(!(!g.sink).in'.[indexOfN.Value]).s).in'.[0]).s
                    let ine = ref { capacity = 1; residual = 1; flow = 0; s = i2; t = n1 }
                    n1 := { (!n1) with in' = Array.append (!n1).in' [| ine |] }
                    i2 := { (!i2) with out' = Array.append (!i2).out' [| ine |] }
                | _ -> ignore 0
          | _ -> ignore 0
        )
      )
      g

    // This is for debugging, make sure correct graph was created
    let printG g =
      let rec printG' v prefix =
        let v = (!v)
        let x,y = v.xy
        if v.label = '^' then printfn "source -"
        else printfn "%s-V(%c,(%d,%d))" prefix v.label x y
        if Array.isEmpty v.out' then
          if v.label = '$' then printfn "%s- sink" prefix
          else printfn "%s- x" prefix
        else
          let handleEdge er =
            let e = (!er)
            printfn "%s--E(%d/%d)" (prefix) (e.flow) (e.capacity)
            (printG' e.t (prefix + " "))
          v.out'
          |> Array.iter handleEdge
      printG' g.source ""
      printfn "\n"
      g

    let getMaxFlow g =
      let rec augment v =
        if (!v).label = '$' then
          printfn "Hit sink!"
          1
        elif (!v).label = '^' then // in case we backwards propagated too far
          printfn "Hit source!"
          0
        else
          let indexOfE = 
            (!v).out'
            |> Array.tryFindIndex(fun er -> (!er).flow = 0)
          if indexOfE.IsSome then
            let edge = (!v).out'.[indexOfE.Value]
            let nv = (!edge).t
            if (!nv).xy = (!v).xy then // the vertex we're going to is ((W/I/N)2), keep going before recursive call
              let indexOfE2 = 
                (!nv).out'
                |> Array.tryFindIndex(fun er -> (!er).flow = 0)
              if indexOfE2.IsNone then // the ((W/I/N)2) is a dead end, return 0
                printfn "Could not find forward edge!"
                0
              else
                let nedge = (!nv).out'.[indexOfE2.Value] // get an out edge from ((W/I/N)2)
                let nnv = (!nedge).t // we want the terminal of the edge
                let ret = augment nnv // recurse
                if ret > 0 then // if we got something > 0, update edge and nedge to have flow
                  edge := { (!edge) with flow = 1; residual = 0 }
                  nedge := { (!nedge) with flow = 1; residual = 0 }
                ret
            else
              let ret = augment nv
              if ret > 0 then
                edge := { (!edge) with flow = 1; residual = 0 } // update to have flow
              ret
          else
            printfn "No available out edge, trying to propagate backwards using available residual..."
            if (!v).in' |> Array.isEmpty |> not then // if we can backwards propagate, try
              let indexOfE2 =
                (!v).in'
                |> Array.tryFindIndex(fun er -> (!er).residual = 0)
              if indexOfE2.IsNone then
                printfn "Backwards propagation failed!"
                0
              else
                let nedge = (!v).in'.[indexOfE2.Value] // get in edge
                let nv = (!nedge).s // get source of in edge
                if (!nv).xy = (!v).xy then // in case we went from ((W/I/N)2) to ((W/I/N)1)
                  let indexOfE3 =
                    (!nv).in'
                    |> Array.tryFindIndex(fun er -> (!er).residual = 0)
                  if indexOfE3.IsNone then
                    printfn "Backwards propagation could not find forward edge!"
                    0
                  else
                    let nnedge = (!nv).in'.[indexOfE3.Value] // get in edge for ((W/I/N)1)
                    let nnv = (!nnedge).s // get source for in edge
                    let ret = augment nnv
                    if ret > 0 then
                      // only need 
                      nnedge := { (!nnedge) with flow = 1; residual = 0 }
                    ret
                else
                  let ret = augment nv
                  if ret > 0 then
                    nedge := { (!nedge) with flow = 1; residual = 0 }
                  ret
            else
              printfn "No available in edge for backwards propagation!"
              0
            
      let ret = 
        (!g.source).out' // try each out edge from source
        |> Array.map(fun er ->
          er := { (!er) with flow = 1; residual = 0 }
          augment (!er).t
        )
        |> Array.sum
      //ignore <| printG g
      ret

let readInGrids filename =
  filename
  |> System.IO.File.ReadAllLines
  |> Array.map(fun line -> line.ToCharArray())

let separateGrids (grids:_[][]) =
  let mutable nGrids = Array.empty
  let mutable tgrid = Array.empty
  grids
  |> Array.iter(fun row ->
    if row |> Array.isEmpty |> not then
      tgrid <- Array.append tgrid [| row |]
    else
      nGrids <- Array.append nGrids [| tgrid |]
      tgrid <- Array.empty
  )
  nGrids <- Array.append nGrids [| tgrid |]
  nGrids

let rawGrids = (readInGrids >> separateGrids) @"support/tile_cut.txt"
let printResult res = printfn "\n%d" res
let printAndSolve = NetworkFlow.Graph.constructGraph >> NetworkFlow.Graph.printG >> NetworkFlow.Graph.getMaxFlow >> printResult
let solve = NetworkFlow.Graph.constructGraph >> NetworkFlow.Graph.getMaxFlow >> printResult
let printGraph = NetworkFlow.Graph.constructGraph >> NetworkFlow.Graph.printG >> ignore

rawGrids |> Array.iter( solve )
