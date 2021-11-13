// Learn more about F# at http://fsharp.org

open Optimization
open Functions.Expressions
open Functions.Operators

let range = (-7., 3.)

let a, b = range

let delta = 1e-10

let x = Var "x"

let f =
    x ** 4. + Val 8. * x ** 3.
    - Val 8. * x ** 2.
    - Val 96. * x
    + Val 1.

[<EntryPoint>]
let main _ =
    printfn $"expr = %s{show f}"

    let f_1 = call f
    let fp = call (diff f "x")

    let x_star_poly = Polyline.argmin range delta fp f_1
    let x_star_enum = Enumeration.argmin 22 range f_1

    printfn $"%f{x_star_poly}"
    printfn $"%f{x_star_enum}"


    0
