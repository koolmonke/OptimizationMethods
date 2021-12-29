module Lab3.FastDown

open MatrixArithmetic
open VectorNorm

let argmin (fp: Vector -> Vector) (a: Matrix) (eps: float) =
    let mutable xkp = Vector(2)
    let mutable counter = 0

    let mutable h = fp xkp

    while vectorNorm h >= eps do
        h <- fp xkp
        let alpha = (vectorNorm h) ** 2 / ((a * h) * h)
        xkp <- xkp - (alpha * h)
        counter <- counter + 1

    xkp, counter
