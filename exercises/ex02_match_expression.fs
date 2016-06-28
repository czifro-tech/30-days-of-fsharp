namespace Exercises

  open System

  /// <summary>
  ///
  /// </summary>
  module Match =

    /// <summary>
    /// Matches type of v
    /// </summary>
    let printType (v: obj) =
      match v with
      | :? string as s -> printfn "Type is string"
      | :? int as i -> printfn "Type is integer"
      | :? float as f -> printfn "Type is float"
      | :? bool as b -> printfn "Type is bool"
      | _ -> printfn "Type is other"

    /// <summary>
    /// A recursive function to print each item in a string list
    /// Keyword rec allows function to be called recursively
    /// </summary>
    let rec printEachElement arr = 
      match arr with
      | head :: tail -> printfn "%s" head; printEachElement tail // head is first element, tail is remainder of list
      | [] -> printfn "Array is empty"

    /// <summary>
    /// Does a match on the length of array
    /// </summary>
    let matchArrayLength arr =
      let printLength l = (printfn "Length: %d" l)
      match arr with
      | [] -> printLength 0
      | [ _ ] -> printLength 1
      | [ _; _ ] -> printLength 2
      | _ -> arr |> List.length |> printLength

    let detectZeroOr point =
      match point with
      | (0, 0) | (0, _) | (_, 0) -> printf "Zero found"
      | _ -> printfn "Both nonzero"

    let detectZeroAND point =
      match point with
      | (0, 0) -> printfn "Both values zero."
      | (var1, var2) & (0, _) -> printfn "First value is 0 in (%d, %d)" var1 var2
      | (var1, var2)  & (_, 0) -> printfn "Second value is 0 in (%d, %d)" var1 var2
      | _ -> printfn "Both nonzero."

  module MatchTest =
    
    let test =
      Match.detectZeroOr (0,0)
      Match.detectZeroOr (1,1)
      Match.detectZeroAND (0,1)
      Match.printEachElement ["H"; "E"; "L"; "L"; "O"]
      Match.printType "string"
      Match.printType 6
      Match.printType (new Object())
      Match.matchArrayLength [1; 1]
      Match.matchArrayLength [1; 1; 1]