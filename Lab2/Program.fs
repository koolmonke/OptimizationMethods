open OptimizationMethods
open Functions.Expressions
open Functions.Operators

let range = (-7., 3.)

let x = Var "x"

let expr =
    x ** 4. + Val 8. * x ** 3.
    - Val 8. * x ** 2.
    - Val 96. * x
    + Val 1.

[<EntryPoint>]
let main _ =
    printfn $"expr = %s{show expr}"

    let f = call expr
    let fp = call (diff expr "x")

    let x_star_poly = Polyline.argmin range 1e-10 f fp
    let x_star_enum = Enumeration.argmin range 22 f
    
    printfn $"x_star для метода ломанных %f{x_star_poly}"
    printfn $"x_star для метода перебора %f{x_star_enum}"

    0
