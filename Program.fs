// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

namespace Exercises

  open System.Threading
  open Exercises

  module Main =

    let exercise op =
      match op with
      | 1 -> HelloWorld.hello
      | 2 -> MatchTest.test
      | 3 -> TestCowInterrogator.test
      | 13 -> printfn "Deck -> %A "DeckTest.test
      | 22 ->
           let disposable = WebServer.Server.Start(port = 8090)
           Thread.Sleep(15000) // wait 15 sec to close server
           disposable.Dispose()
      | _ -> failwithf "Invalid Option: %d. Valid Options are 1-30" op

    [<EntryPoint>]
    let main argv =
        let adderGen num = (+) num
        let num = adderGen 1 5
        printfn "%d" num
        let op (v:int) = (<) v
        let b = op 5 6
        printfn "%b" b
        //exercise (int argv.[0])
        0 // return an integer exit code