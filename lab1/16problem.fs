module Problem16
// For more information see https://aka.ms/fsharp-console-apps

let rec power (aaa: bigint) (exponent: bigint) : bigint =
    if exponent = 0I then
        1I
    else
        aaa * power aaa (exponent - 1I)

let getAnsWithMap (n: bigint) =
    let number = power 2I n

    number.ToString().ToCharArray()
    |> Array.map (fun c -> (int c - int '0'))
    |> Array.filter (fun elem -> elem > 0)
    |> Array.sum

let rec powerTail (aaa: bigint) (exponent: bigint) (acc: bigint) : bigint =
    if exponent = 0I then
        acc
    else
        powerTail aaa (exponent - 1I) (acc * aaa)

let sumOfDigitsUsingWhile (n: bigint) =
    let mutable value = powerTail 2I n 1I
    let mutable sum = 0

    while value > 0I do
        sum <- sum + (int)(value % 10I)
        value <- value / 10I

    sum

let solutions = [ sumOfDigitsUsingWhile 1000, getAnsWithMap 1000 ]
