module Lab3.Fragmentation

open MatrixArithmetic
open VectorNorm

let argmin (f: Vector -> float) (fp: Vector -> Vector) (eps: float) (betta: float) (lambda: float) =
    let mutable xk = Vector(2)
    let mutable xkp = Vector(2)
    xkp.[0] <- 1
    xkp.[1] <- 1

    let mutable counter = 0

    while vectorNorm (xk - xkp) >= eps do
        xk <- xkp
        let mutable alpha = betta
        let h = -1. * fp xkp

        while f (xkp + alpha * h) >= (f xkp) do
            alpha <- alpha * lambda

        xkp <- xkp + (alpha * h)
        counter <- counter + 1

    xk, counter
