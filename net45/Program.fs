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
      | 23 ->
           printf "Enter 'S' to start as server or 'C' to start as client: "
           let c = Console.ReadLine()
           let mutable tTok = null : IDisposable
           match c.ToLower() with
           | "s" -> 
                 tTok <- ChatClient.Session.createSession(false) 
                 |> ChatClient.Session.runSession (ChatClient.ServerAndClient.receive) (ChatClient.ServerAndClient.send)
           | "c" -> 
                 tTok <- ChatClient.Session.createSession(true) 
                 |> ChatClient.Session.runSession (ChatClient.ServerAndClient.send) (ChatClient.ServerAndClient.receive)
           | _ -> failwithf "Invalid Option: %s" c
           let ct = tTok
           printfn "Running session for 120 sec"
           Thread.Sleep(120*1000)
           ct.Dispose()
      | _ -> failwithf "Invalid Option: %d. Valid Options are 1-30" op

    [<EntryPoint>]
    let main argv =
        exercise (int argv.[0])
        0 // return an integer exit code