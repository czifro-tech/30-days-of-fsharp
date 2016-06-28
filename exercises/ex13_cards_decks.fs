namespace Exercises

  module Deck =

    let rand = new System.Random()

    let newDeck =
      [|"Hearts";"Clubs";"Diamonds";"Spades"|]
      |> Seq.map(fun suit ->
        [|"2";"3";"4";"5";"6";"7";"8";"9";"10";"J";"Q";"K";"A"|]
        |> Seq.map(fun face ->
          (suit, face)
        )
        |> Seq.toArray
      )
      |> Seq.toArray
      |> Array.concat

    let shuffle (deck:(string*string)[]) =
      let swap (a: _[]) x y =
        let temp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- temp
      deck |> Array.iteri(fun i _ -> swap deck i (rand.Next(i, deck |> Array.length)))
      deck

    let rec deal (deck:(string*string)[]) (players:(string*(string*string)[])[]) dealFun di pi =
      if di >= Array.length(deck) then
        players
      else
        players.[pi] <- dealFun(deck, players.[pi], di)
        deal deck players dealFun (di+1) ((pi + 1) % Array.length(players))

  module DeckTest = 

    let test = 
      let hand = [||]:(string*string)[]
      let mutable deck = Deck.newDeck
      deck <- Deck.shuffle deck
      let mutable players = [|("Tim", hand); ("Brennan", hand); ("Hannah", hand); ("Jenna", hand)|]
      let dealFun = (fun (cards:(string*string)[], (name:string, pHand:(string*string)[]), i) -> 
        (name, (pHand |> Array.append [|cards.[i]|]))
      )
      players <- Deck.deal deck players dealFun 0 0
      printfn "%A" players
