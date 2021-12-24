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
let polylineResult = Polyline.argmin range accuracy f fp
let xStarPoly = fst polylineResult
let xStarEnum = Enumeration.argmin range n f


printfn $"x* для метода перебора {xStarEnum}" //-141,8501425790588
printfn $"Фактическая точность {abs (actual - xStarEnum)}" // 1,1498574209412027
printfn $"Теоретическая точность {(snd range - fst range) / float (2 * n)}" // 0,22727272727272727
printfn $"x* для метода ломанных {xStarPoly}" // -143
printfn $"Фактическая точность {abs (actual - xStarPoly)}" // 0
printfn $"Теоретическая точность {accuracy}" // 1E-10
printfn $"Количество итераций для метода ломаных {snd polylineResult}" // 42
