
module Pattern =

  let private (|TString|TInt|TFloat|TBool|TOther|) (v:obj) =
    if v.GetType() = typeof<string> then TString
    elif v.GetType() = typeof<int> then TInt
    elif v.GetType() = typeof<float> then TFloat
    elif v.GetType() = typeof<bool> then TBool
    else TOther

  let printType (v: obj) =
    match v with
    | TString -> printfn "Type is string"
    | TInt -> printfn "Type is int"
    | TFloat -> printfn "Type is float"
    | TBool -> printfn "Type is bool"
    | TOther -> printfn "Type is other"

  let (|ValidDateTime|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex(pattern).Match(input)
    if m.Success then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None

  let tryPrintDate str =
    let date =
      match str with
      | ValidDateTime "(\\d{1,2})/(\\d{1,2})/(\\d{1,2})$" [ m; d; y]
        -> System.DateTime(int y + 2000, int m, int d)
      | ValidDateTime "(\\d{1,2})/(\\d{1,2})/(\\d{3,4})" [ m; d; y]
        -> System.DateTime(int y, int m, int d)
      | ValidDateTime "(\\d{1,2})-(\\d{1,2})-(\\d{1,2})$" [ m; d; y]
        -> System.DateTime(int y, int m, int d)
      | _ -> System.DateTime()
    printfn "%s" (date.ToString())

Pattern.printType "string"
Pattern.printType 6
Pattern.printType (new System.Object())
Pattern.tryPrintDate "12/22/08"
Pattern.tryPrintDate "1/8/2009"
Pattern.tryPrintDate "10-23-15"
Pattern.tryPrintDate "bad date"
