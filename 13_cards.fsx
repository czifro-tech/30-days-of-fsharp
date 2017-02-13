
module Game =

  type CardSuit = 
    | Hearts
    | Clubs
    | Diamonds
    | Spades
  type Card =
    {face: string;
      suit: CardSuit;}
  type Player =
    {name: string;
      hand: Card[];}
  let newDeck() =
    let suits = [| CardSuit.Hearts; CardSuit.Clubs; CardSuit.Diamonds; CardSuit.Spades |]
    let faces = [|"2";"3";"4";"5";"6";"7";"8";"9";"10";"J";"Q";"K";"A"|]
    suits
    |> Array.collect (fun suit' ->
      faces
      |> Array.map(fun face' -> { face = face'; suit = suit' })
    )
  let shuffle (deck:Card[]) =
    let rand = System.Random()
    let swap (a:_[]) x y =
      let t = a.[x]
      a.[x] <- a.[y]
      a.[y] <- a.[x]
    Array.iteri(fun i _ -> swap deck i (rand.Next(i, 52))) deck
    deck
  let rec deal (deck:Card[]) (players:Player[]) dealFun =
    match deck with
    | [||] -> players
    | _ ->
      players.[0] <- dealFun deck players.[0]
      deal deck.[1..] (Array.append players.[1..] [| players.[0] |]) dealFun

let deck = (() |> Game.newDeck |> Game.shuffle)
let mutable players =
  [|
    { Game.Player.name = "Will"; Game.Player.hand = [||] }
    { Game.Player.name = "Tim"; Game.Player.hand = [||] }
    { Game.Player.name = "Brennan"; Game.Player.hand = [||] }
  |]

let dealFun = 
  fun (d:_[]) p -> { p with Game.Player.hand = Array.append p.hand [| d.[0] |]}

players <- Game.deal deck players dealFun

players
|> Array.iter(fun p -> printfn "Name: %s, Deck: %A" p.name p.hand)
