module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Fact>]
let ``Check Problem 15`` () =
    Problem15.solutions
    |> List.iter (fun solution ->
        let actual = solution 20 // вызываем решение, чтобы получить результат
        Assert.Equal("137846528820", actual))


[<Fact>]
let ``Check Problem 16`` () =
    Problem16.solutions
    |> List.iter (fun solution ->
        let actual = solution 1000 // вызываем решение, чтобы получить результат
        Assert.Equal("1366", actual))
