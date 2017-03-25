#load "./10_sudoku_board.fsx"
open ``10_sudoku_board``

/// This is based on the original elixir solution: https://github.com/kblake/30-days-of-elixir/blob/master/11-sudoku-solver.exs
/// To recap:
///
/// This is a brute force method and does not work for boards 9x9 or larger
///
/// F# does not have a keyword `nil` that is type safe with integers.
/// So the following will accommodate:
///
///   fsi> let nil = -1
///
/// Small board:
///
///   fsi> let board =
///          [[  1; nil; 3  ];
///            [  3; nil; 2  ];
///            [ nil; 3; nil ]]
///
/// The `solve` function accepts the board and returns the solution
///
///   fsi> SudokuSolver.solve(board)
///   [|[| 1; 2; 3 |];
///     [| 3; 1; 2|];
///     [| 2; 3; 1|]|]
///
/// For more on the mechanics of the solution, go to the original elixir script
module SudokuSolver =

  let (--) l r =
    l
    |> List.filter(fun x -> r |> List.contains x |> not)

  let nil = -1

  let private permutations (l:int list) =
    let n = List.length l
    l
    |> List.mapi(fun i _ ->
      l |> List.permute(fun j -> (j + i) % n)
    )

  let rec combinations (l:int list list) =
    match l with
    | [_] -> permutations (l |> List.head)
    | [] -> []
    | h::t ->
      let crest = combinations t
      permutations h
      |> List.collect(fun p ->
        crest |> List.map(fun c -> p@c)
      )

  let rec possibles (board:int list list) =
    let n = board |> List.length
    board
    |> List.map(fun row ->
      [1..n] -- row
    )

  let solutions (board:int list list) =
    board |> (possibles >> combinations)

  let applySolutions (board:int list list) (solutions':int list list) =
    solutions'
    |> List.map(fun solution ->
      let mutable solution = solution
      board
      |> List.map(fun row ->
        row
        |> List.map(fun c ->
          if c = nil then
            let s = solution |> List.head
            solution <- List.tail solution
            s
          else c
        )
      )
    )

  let solve (board:int list list) =
    board
    |> solutions
    |> applySolutions board
    |> List.filter SudokuBoard.solved

let nil = -1

let board1 = 
  [
    [  1; nil; 3  ];
    [  3; nil; 2  ];
    [ nil; 3; nil ]
  ]

printfn "%A" (SudokuSolver.solve board1)