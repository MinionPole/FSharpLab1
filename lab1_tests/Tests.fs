module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Fact>]
let ``Check Problem 15`` () =
    Problem15.solutions
    |> List.iter (fun res -> Assert.Equal<bigint>(res, 137846528820I))


[<Fact>]
let ``Check Problem 16`` () =
    Problem16.solutions |> List.iter (fun res -> Assert.Equal(res, 1366))
