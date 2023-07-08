open Lab3
open MatrixArithmetic

let f (v: Vector) =
    let x, y = v.[0], v.[1]

    29. * x ** 2. + 18. * x * y + 3. * y ** 2. - 24. * x - 12. * y + 31.

let fp (v: Vector) =
    let x, y = v.[0], v.[1]
    let v = Vector(2)
    v.[0] <- -24. + 58. * x + 18. * y
    v.[1] <- 6. * (-2. + 3. * x + y)
    v

let eps = 0.001
let betta = 3.
let lambda = 0.5

let v, iterCount = Fragmentation.argmin f fp eps betta lambda

printfn $"""достигнуто минимальное значение f({v.ToString("F4", ",")})={f v} за {iterCount} итераций"""
