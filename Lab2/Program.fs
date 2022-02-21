open Functions.Expressions
open Lab2

let range = (-7., 3.)

let x = Var "x"

let expr =
    x ** Val 4. + Val 8. * x ** Val 3.
    - Val 8. * x ** Val 2.
    - Val 96. * x
    + Val 1.


let f = evalf expr "x"
let fp = evalf (diff expr "x") "x"
let actual = -143.
let n = 22
let accuracy = 1e-10
let xStarPoly, iterCount = Polyline.argmin range accuracy f fp
let xStarEnum = Enumeration.argmin range n f


printfn $"x* для метода перебора {xStarEnum}"
printfn $"Фактическая точность {abs (actual - xStarEnum)}"
printfn $"Теоретическая точность {(snd range - fst range) / float (2 * n)}"
printfn $"x* для метода ломанных {xStarPoly}"
printfn $"Фактическая точность {abs (actual - xStarPoly)}"
printfn $"Теоретическая точность {accuracy}"
printfn $"Количество итераций для метода ломаных {iterCount}"
