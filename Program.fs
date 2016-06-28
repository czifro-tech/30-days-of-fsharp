// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

namespace Exercises

  open System
  open System.Threading
  open Exercises

  module Main =

    let exercise op =
      match op with
      | 1 -> HelloWorld.hello
      | 2 -> MatchTest.test
      | 3 -> TestCowInterrogator.test
      | 4 -> SeqTest.test
      | 13 -> printfn "Deck -> %A" DeckTest.test
      | 22 ->
           let disposable = WebServer.Server.Start(port = 8090)
           printfn "Enter a key to quit..."; Console.Read |> ignore // wait for keystroke to stop server
           disposable.Dispose()
      | _ -> failwithf "Invalid Option: %d. Valid Options are 1-30" op

    [<EntryPoint>]
    let main argv =
        exercise (int argv.[0])
        0 // return an integer exit code