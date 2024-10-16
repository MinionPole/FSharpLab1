﻿module Problem15

let solveWithFor (x: int) =
    let mutable results = []

    for i in 0..x do
        if i = 0 then
            let onesList = List.replicate (x + 1) 1I
            results <- onesList :: results
        else
            let previousList = List.head results

            let newList =
                List.fold
                    (fun (acc: bigint list) value ->
                        let sum = (if List.isEmpty acc then 0I else List.head acc) + value
                        sum :: acc)
                    []
                    (previousList)
                |> List.rev


            results <- newList :: results

    string ((List.rev results).[x].[x])


let rec factorial (n: bigint) : bigint =
    if n = 0I then 1I else n * factorial (n - 1I)

let solveWithFact (n: int) =
    let nBigInt = bigint n
    string ((factorial (2I * nBigInt)) / (factorial nBigInt) / (factorial nBigInt))

let factorialTailRec (n: bigint) : bigint =
    let rec aux acc n =
        if n = 0I then acc else aux (acc * n) (n - 1I)

    aux 1I n

let numerator = Seq.initInfinite (fun i -> factorialTailRec (bigint (i) * 2I))
let denominator = Seq.initInfinite (bigint >> factorialTailRec)

let getAnsFromLazyCollections (x: int) =
    let numeratorOb = numerator |> Seq.take (x + 1) |> Seq.toList
    let denominatorOb = denominator |> Seq.take (x + 1) |> Seq.toList
    string (numeratorOb.[x] / denominatorOb.[x] / denominatorOb.[x])

let solutions = [ getAnsFromLazyCollections; solveWithFor; solveWithFact ]
