module Lab3.Fragmentation

open MatrixArithmetic
open VectorNorm

let argmin (f: Vector -> float) (fp: Vector -> Vector) (eps: float) (betta: float) (lambda: float) =
    let xk = Vector(2)
    let xkp = Vector([ 1.; 1. ])

    let rec rArgmin xk xkp counter =
        if vectorNorm (xk - xkp) >= eps then
            let h = -1. * fp xkp

            let rec innerLoop alpha =
                if f (xkp + alpha * h) >= f xkp then
                    innerLoop (alpha * lambda)
                else
                    alpha

            let alpha = innerLoop betta
            rArgmin xkp (xkp + alpha * h) (counter + 1)
        else
            xk, counter

    rArgmin xk xkp 0
