## Задача 15: Сетка решётки

Сколько различных маршрутов существует в сетке размером 20 на 20, двигаясь только вправо и вниз?

### Условия

- Вы начинаете в верхнем левом углу сетки.
- Вам нужно добраться до нижнего правого угла.
- Движение разрешено только вправо и вниз.

### Решение
Есть два подхода.

* Первый подразумевает решение через динамическое программирование, когда сумма путей в ячейке равна сумме путей из верхней и левой ячейки.
* Решение формулой. Решение данной задачи = $ (2x)!/(x!)^2$

## Задача 16: Сумма цифр степени

Найдите сумму цифр числа $ 2^{1000} $

### Решение
Ничего удивительного нет, используем встроенный тип большой ёмкости, вычисляем значение выражения и считаем сумму.


# Problem 15 solutions

1. Рекурсия:
```f#
let rec factorial (n: bigint) : bigint =
    if n = 0I then 1I else n * factorial (n - 1I)

let solveWithFact (n: int) =
    let nBigInt = bigint n
    string ((factorial (2I * nBigInt)) / (factorial nBigInt) / (factorial nBigInt))
```

2. Использование циклов + fold:
```f#
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
```

3. Ленивый список
```f#
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
```

4. Результаты в тесте:
```f#
// in Problem15.fs
let solutions = [ getAnsFromLazyCollections; solveWithFor; solveWithFact ]

[<Fact>]
let ``Check Problem 15`` () =
    Problem15.solutions
    |> List.iter (fun solution ->
        let actual = solution 20 // вызываем решение, чтобы получить результат
        Assert.Equal("137846528820", actual))
```

# Problem 16 Solutions

1. Рекурсия + модульное:
```f#
let rec power (aaa: bigint) (exponent: bigint) : bigint =
    if exponent = 0I then
        1I
    else
        aaa * power aaa (exponent - 1I)

let getAnsWithMap (n: bigint) =
    let number = power 2I n

    string (
        number.ToString().ToCharArray()
        |> Array.map (fun c -> (int c - int '0'))
        |> Array.filter (fun elem -> elem > 0)
        |> Array.sum
    )

```

2. Использование циклов + хвостовая рекурсия:
```f#
let rec powerTail (aaa: bigint) (exponent: bigint) (acc: bigint) : bigint =
    if exponent = 0I then
        acc
    else
        powerTail aaa (exponent - 1I) (acc * aaa)

let sumOfDigitsUsingWhile (n: bigint) =
    let mutable value = powerTail 2I n 1I
    let mutable sum = 0

    while value > 0I do
        sum <- sum + (int) (value % 10I)
        value <- value / 10I

    string sum
```

4. Результаты в тесте:
```f#
// in Problem16.fs
let solutions = [ sumOfDigitsUsingWhile; getAnsWithMap ]

[<Fact>]
let ``Check Problem 16`` () =
    Problem16.solutions
    |> List.iter (fun solution ->
        let actual = solution 1000 // вызываем решение, чтобы получить результат
        Assert.Equal("1366", actual))

```
