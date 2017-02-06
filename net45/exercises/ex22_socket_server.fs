namespace Exercises

  open System
  open System.IO
  open System.Net
  open System.Net.Sockets
  open System.Threading

  /// <summary>
  /// Code came from https://gist.github.com/panesofglass/765088/c70233754497802eeabc5f3654cabedcd009254f
  /// </summary>
  module WebServer =

    /// <summary>
    /// Extends System.Net.Sockets.Socket
    /// Adds methods that wrap BeginXxx/EndXxx async model into single method call
    /// </summary>
    type Socket with
      member socket.AsyncAccept() = Async.FromBeginEnd(socket.BeginAccept, socket.EndAccept)
      member socket.AsyncReceive(buffer:byte[], ?offset, ?count) =
        let offset = defaultArg offset 0
        let count = defaultArg count buffer.Length
        let beginReceive(b,o,c,cb,s) = socket.BeginReceive(b,o,c,SocketFlags.None,cb,s)
        Async.FromBeginEnd(buffer, offset, count, beginReceive, socket.EndReceive)
      member socket.AsyncSend(buffer:byte[], ?offset, ?count) =
        let offset = defaultArg offset 0
        let count = defaultArg count buffer.Length
        let beginSend(b,o,c,cb,s) = socket.BeginSend(b,o,c,SocketFlags.None,cb,s)
        Async.FromBeginEnd(buffer, offset, count, beginSend, socket.EndSend)

    /// <summary>
    /// A static server that asynchronously processes http requests
    /// </summary>
    type Server() =
      static member Start(hostname:string, ?port) =
        let ipAddress = Dns.GetHostEntry(hostname).AddressList.[0]
        Server.Start(ipAddress, ?port = port)

      static member Start(?ipAddress, ?port) =
        let ipAddress = defaultArg ipAddress IPAddress.Any
        let port = defaultArg port 80
        let endpoint = IPEndPoint(ipAddress, port)
        let cts = new CancellationTokenSource()
        let listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        listener.Bind(endpoint)
        listener.Listen(int SocketOptionName.MaxConnections)
        printfn "Started listening on port %d" port

        // create an async func to process requests
        // equivalent to an action that is executed using Task.Run(...)
        let rec loop() = async {
          printfn "Waiting for request..."
          // the equivalent C# code: var socket = await listener.AcceptAsync();
          let! socket = listener.AsyncAccept()
          printfn "Received request"
          // Add code here to process request
          let response = [|
            "HTTP/1.1 200 OK\r\n"B // string gets converted to array of bytes
            "Content-Type: text/plain\r\n"B
            "\r\n"B
            "Hello World!"B |] |> Array.concat // byte [] [] -> byte []
          try
            try
              let! bytesSent = socket.AsyncSend(response)
              printfn "Sent response"
            with e -> printfn "An error occurred: %s" e.Message
          finally
            socket.Shutdown(SocketShutdown.Both)
            socket.Close()
          
          // do! someFunc == await someMethod();

          // equivalent to return await loop();
          return! loop() }
        
        // equivalent to Task.Run(...)
        Async.Start(loop(), cancellationToken = cts.Token)
        // create and return a new disposable that can stop the async task
        { new IDisposable with member x.Dispose() = cts.Cancel(); listener.Close() }
