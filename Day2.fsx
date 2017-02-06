

module Match =

  let printType (v: obj) =
    match v with
    | :? string -> printfn "Type is string"
    | :? int -> printfn "Type is int"
    | :? float -> printfn "Type is float"
    | :? bool -> printfn "Type is bool"
    | _ -> printfn "Type is other" // _ acts as wild card, used similarly to "default" in standard switch in this scenario

  let rec printList list' =
    match list' with
    | h :: t -> printfn "%s" h; printList t // head is first element, tail is remainder of list
    | [] -> printfn "List is empty"

  let detectListLength l =
    let printLength i = printfn "Length: %d" i
    match l with
    | [] -> printLength 0 // [] represents empty list
    | [ _ ] -> printLength 1 // _ is wild card
    | [ _; _ ] -> printLength 2
    | _ -> l |> List.length |> printLength

  let findZero xy = 
    match xy with
    | 0,0 -> printfn "both zero"
    | 0,_ -> printfn "first is zero"
    | _,0 -> printfn "second is zero"
    | _ -> printfn "neither is zero"

Match.findZero (0,0)
Match.findZero (1,1)
Match.findZero (0,1)
Match.findZero (1,0)
Match.printList [ "h"; "e"; "l"; "l"; "o"]
Match.printType "string"
Match.printType 6
Match.printType (new System.Object())
Match.detectListLength [1; 1]
Match.detectListLength [1; 1; 1]
