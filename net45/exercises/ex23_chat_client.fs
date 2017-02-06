namespace Exercises

  open System
  open System.IO
  open System.Net
  open System.Net.Sockets
  open System.Threading

  module ChatClient =

    /// <summary>
    /// Extends System.Net.Sockets.Socket
    /// Adds methods that wrap BeginXxx/EndXxx async model into single method call
    /// </summary>
    type Socket with
      member socket.AsyncAccept() = Async.FromBeginEnd(socket.BeginAccept, socket.EndAccept)
      member socket.AsyncConnect(remote:EndPoint) =
        let beginConnect(ip,cb,s) = socket.BeginConnect(ip,cb,s)
        Async.FromBeginEnd(remote, beginConnect, socket.EndConnect)
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

    module ServerAndClient = 
      let listen (port:int) = async {
        let ipAddress = IPAddress.Any
        let port = port
        let endpoint = IPEndPoint(ipAddress, port)
        let listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        listener.Bind(endpoint)
        listener.Listen(int SocketOptionName.MaxConnections)
        return! listener.AsyncAccept() }

      let connect (ip:string) (port:int) = async {
        let hostInfo = Dns.GetHostEntry(ip)
        let ipAddresses = hostInfo.AddressList
        let hostEndPoint = new IPEndPoint(ipAddresses.[0], port) // 4369
        let s = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
        do! s.AsyncConnect(hostEndPoint)
        return s }

      let send (s:Socket) = async {
        printf "Say something: "
        let msg = Console.ReadLine()
        let bytes (str:string) = System.Text.Encoding.ASCII.GetBytes(str)
        try
          let! bytesSent = s.AsyncSend(bytes(msg))
          printfn "Sent message. Waiting for response..."
        with e ->
          printfn "An error occurred: %s" e.Message
          s.Shutdown(SocketShutdown.Both)
          s.Close()
        }

      let receive (s:Socket) = async {
        let buf = [| for b in 0 .. 1023 -> byte 0 |] // create a zeroed out byte []
        let toString (bytes:byte[]) = System.Text.Encoding.ASCII.GetString(bytes)
        try
          let! res = s.AsyncReceive(buf)
          printfn "Received message: %s" (toString buf)
        with e ->
          printfn "An error occurred: %s" e.Message
          s.Shutdown(SocketShutdown.Both)
          s.Close()
        }

    module Session =

      let createSession (asClient:bool) =
        if asClient then
          (ServerAndClient.connect "127.0.0.1" 50000) |> Async.RunSynchronously
        else
          (ServerAndClient.listen 50000) |> Async.RunSynchronously

      let runSession (fun1: Socket -> Async<unit>) (fun2: Socket -> Async<unit>) (s:Socket) = 
        let cts = new CancellationTokenSource()

        let rec loop() = async {
          do! fun1 s
          do! fun2 s

          return! loop() }
        
        // equivalent to Task.Run(...)
        Async.Start(loop(), cancellationToken = cts.Token)
        // create and return a new disposable that can stop the async task
        { new IDisposable with member x.Dispose() = cts.Cancel() }