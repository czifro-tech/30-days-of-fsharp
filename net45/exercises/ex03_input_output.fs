namespace Exercises

  open System

  module CowInterrogator =

    let getName =
      printf "What is your name? "
      Console.ReadLine()

    let getCowLove =
      printf "Do you like cows? "
      Console.ReadLine()

    let readInCow = 
      try
        IO.File.ReadLines @"support/cow.txt"
      with
      | ex -> failwithf "Error: cow.txt file not found\n"

    let cowArt =
      String.Join("\n", readInCow)
  
    let interrogate =
      let name = getName

      match getCowLove.ToLower() with
      | "y" -> 
            printfn "Great! Here's a cow for you %s" name
            printfn "%s" cowArt
      | "n" ->
            printfn "That's a shame, %s" name
      | _ -> printfn "You should have entered 'Y' or 'N'."
      
  module TestCowInterrogator =

    let test =
      CowInterrogator.interrogate