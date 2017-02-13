
module SudokuBoard =

  let sumTo n =
    ((n*n)/2) + n/2

  let rec doColsSolved (board:_[][]) index =
    match index with
    | i when i >= 0 -> 
      let col = 
        board
        |> Array.map(fun x -> x.[index])
      col |> Array.sum = sumTo (Array.length board) && doColsSolved board (index-1)
    | _ -> true

  let colsSolved board =
    doColsSolved board ((Array.length board)-1)

  let rec rowsSolved board =
    match board with
    | [||] -> true
    | [| _ |] -> board.[0] |> Array.sum = sumTo (Array.length board.[0])
    | _ -> board.[0] |> Array.sum = sumTo (Array.length board.[0]) && rowsSolved board.[1..]

  let solved board =
    rowsSolved board && colsSolved board

let board1 = [| [|1;2|]; [|2;1|] |]
let board2 = [| [|1;1|]; [|2;1|] |]
let board3 = [| [|2;1|]; [|2;1|] |]

printfn "board: %A, solved: %b" board1 (SudokuBoard.solved board1)
printfn "board: %A, solved: %b" board2 (SudokuBoard.solved board2)
printfn "board: %A, solved: %b" board3 (SudokuBoard.solved board3)
