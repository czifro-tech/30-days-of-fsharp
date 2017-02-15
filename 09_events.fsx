
module Events =

  type ClassWithCLIEvent() =

    let event = new Event<_>()

    // C# => public event EventHandler Event;
    [<CLIEvent>]
    member this.Event = event.Publish

    member this.TriggerEvent(arg) =
      event.Trigger(this,arg)

let c = Events.ClassWithCLIEvent()

// For C# devs, typically this would look like
//   `c.Event += (sender,arg) => Console.WriteLine("Event occurred! Object data: {0}", arg)`
// F# cannot use `+=`. That is syntactic sugar for C#. Since `Event` belongs to C#,
//   F# has to use the traditional method call
c.Event.Add(fun (sender, arg) -> printfn "Event occurred! Object data: %s" arg)

c.TriggerEvent("Hello World!")