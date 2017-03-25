
module SudokuBoard =

  let private sumTo n =
    ((n*n)/2) + n/2

  let private colsSolved (board:int list list) =
    let n = board |> List.length
    let tail = board |> List.tail
    if tail |> List.isEmpty then true
    else
      board
      |> List.head
      |> List.mapi(fun i v ->
        let colSum =
          tail
          |> List.sumBy (fun row ->
            row
            |> List.mapi(fun j c -> if i = j then c else 0)
            |> List.sum
          )
        (v + colSum) = (sumTo n) + 1
      ) |> List.reduce (&&)
  let private rowsSolved board =
    let n = board |> List.length
    board
    |> List.map(fun row ->
      row |> List.sum = (sumTo n) + 1
    ) |> List.reduce ( && )

  let solved board =
    rowsSolved board && colsSolved board

// We are loading this script into another script
// To avoid having tests run every time, we are
//  putting it into a module.
// So to run, load script into fsi and
// run `SudokuBoardTester.test()`
module SudokuBoardTester =

  let board1 = [ [1;2]; [2;1] ]
  let board2 = [ [1;1]; [2;1] ]
  let board3 = [ [2;1]; [2;1] ]

  let test() =
    printfn "board: %A, solved: %b" board1 (SudokuBoard.solved board1)
    printfn "board: %A, solved: %b" board2 (SudokuBoard.solved board2)
    printfn "board: %A, solved: %b" board3 (SudokuBoard.solved board3)
