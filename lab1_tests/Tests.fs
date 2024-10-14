module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Fact>]
let ``Check Problem 15`` () =
    Problem15.solutions
    |> List.iter (fun solution ->
        let res: int = solution ()
        Assert.Equal(res, 137846528820I))

[<Fact>]
let ``Check Problem 16`` () =
    Problem16.solutions
    |> List.iter (fun solution ->
        let res: int = solution ()
        Assert.Equal(res, 1366))