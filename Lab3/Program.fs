open System
open Lab3
open MatrixArithmetic

let formatOutput f (v: Vector, iterCount) =
    let args = v.ToString("F4", ",")
    $"достигнуто минимальное значение f({args})={f v} за {iterCount} итераций"

let f (v: Vector) =
    let x, y = v.[0], v.[1]

    29. * x ** 2. + 18. * x * y + 3. * y ** 2.
    - 24. * x
    - 12. * y
    + 31.

let fp (v: Vector) =
    let x, y = v.[0], v.[1]
    let v = Vector(2)
    v.[0] <- -24. + 58. * x + 18. * y
    v.[1] <- 6. * (-2. + 3. * x + y)
    v

let eps = 0.001
let betta = 3.
let lambda = 0.5

let FragmentationResult =
    Fragmentation.argmin f fp eps betta lambda

Console.WriteLine(formatOutput f FragmentationResult)
