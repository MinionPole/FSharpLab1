module Problem15
let solveWithFor x =
    let mutable results = []

    for i in 0 .. x do
        if i = 0 then
            let onesList = List.replicate (x+1) 1I
            results <- onesList :: results
        else
            let previousList = List.head results
            let newList =
                          List.fold (fun (acc: bigint list) value ->
                            let sum = (if List.isEmpty acc then 0I else List.head acc) + value
                            sum :: acc
                          ) [] (previousList)
                          |> List.rev

            
            results <- newList :: results

    (List.rev results).[x].[x]


let rec factorial (n: bigint) : bigint =
    if n = 0I then 1I
    else n * factorial (n - 1I)

let solveWithFact n =
  (factorial (2I*n)) / factorial n / factorial n

let factorialTailRec (n: bigint) : bigint =
    let rec aux acc n =
        if n = 0I then acc
        else aux (acc * n) (n - 1I)
    aux 1I n    

let numerator = Seq.initInfinite (fun i -> factorial (bigint (i) * 2I))
let denominator = Seq.initInfinite (bigint >> factorial)

let getAnsFromLazyCollections x =
  let numeratorOb = numerator |> Seq.take (x + 1) |> Seq.toList
  let denominatorOb = denominator |> Seq.take (x+1) |> Seq.toList
  numeratorOb.[x]/denominatorOb.[x]/denominatorOb.[x]

let solutions = [solveWithFor 20, solveWithFact 20I, factorialTailRec 20I, getAnsFromLazyCollections 20]