module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Fact>]
let ``Check Problem 16`` () =
    Problem16.solutions
    |> List.iter (fun solution ->
        let actual = solution 1000 // вызываем решение, чтобы получить результат
        Assert.Equal("1366", actual))
