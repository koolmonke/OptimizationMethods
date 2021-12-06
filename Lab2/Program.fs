open Functions.Expressions
open Functions.Operators
open Lab2

let range = (-7., 3.)

let x = Var "x"

let expr =
    x ** Val 4. + Val 8. * x ** Val 3.
    - Val 8. * x ** Val 2.
    - Val 96. * x
    + Val 1.

[<EntryPoint>]
let main _ =
    let f = evalf expr "x"
    let fp = evalf (diff expr "x") "x"

    let x_star_poly = Polyline.argmin range 1e-10 f fp
    let x_star_enum = Enumeration.argmin range 22 f

    printfn $"x_star для метода ломанных {x_star_poly}"
    printfn $"x_star для метода перебора {x_star_enum}"

    0
