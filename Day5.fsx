
module Types =

  type Class() =
    let mutable privateField = 5

    member x.PublicField
      with get() = privateField
      and set(value) = privateField <- value

  type Enum =
    | IsInteger
    | IsString
    | IsOther

  type Record =
    {
      id: int;
      value: string;
    }

  let typeCheck (v:obj) =
    match v with
    | :? int -> IsInteger
    | :? string -> IsString
    | _ -> IsOther

let c = Types.Class()
c.PublicField <- 9
let r = { Types.Record.id = 2; Types.Record.value = "string" }

match (r.id |> Types.typeCheck) with
| Types.Enum.IsInteger -> printfn "is int"
| _ -> printfn "not int"
