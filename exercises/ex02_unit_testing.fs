namespace Exercises

  open System
  open NUnit.Framework

  [<TestFixture>]
  module MyTest =

    [<Test>]
    let test_assert =
      Assert.IsTrue((1 + 1) = 2)
      // Assert.IsTrue((1 + 1) = 3)

    [<Test>]
    let assertThrow =
      let testFun x = (fun x -> x / 0) |> ignore
      Assert.Throws<ArithmeticException>(fun _ -> (testFun 5)) |> ignore