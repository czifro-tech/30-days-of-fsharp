
module CowInterrogator =
  
  let getName() =
    printf "What is your name? "
    System.Console.ReadLine()

  let getCowLove() =
    printf "Do you like cows? "
    System.Console.ReadLine()

  let readInCow() =
    try
      System.IO.File.ReadLines @"support/cow.txt"
    with
    | _ -> failwithf "Error: cow.txt file not found\n"

  let cowArt = System.String.Join("\n", readInCow())

  let interrogate() =
    let name = getName()

    match (getCowLove()).ToLower() with
    | "y" ->
      printfn "Great! Here's a cow for you %s" name
      printfn "%s" cowArt
    | "n" -> printfn "That's a shame, %s" name
    | _ -> printfn "You should have entered 'Y' or 'N'."

CowInterrogator.interrogate()