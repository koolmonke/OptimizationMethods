module Functions.Expressions

open System


type Expr =
    | Val of float
    | Var of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Pow of Expr * float

let rec diff f x =
    match f with
    | Var y when x = y -> Val 1.0
    | Val _
    | Var _ -> Val 0.0
    | Add (f, g) -> Add(diff f x, diff g x)
    | Sub (f, g) -> Sub(diff f x, diff g x)
    | Mul (f, g) -> Add(Mul(f, diff g x), Mul(g, diff f x))
    | Pow (f, 2.)  -> Mul(Val 2., f)
    | Pow (f, g) -> Mul(Val g, Pow(f, g - 1.))

let rec call expr x =
    match expr with
    | Val number -> number
    | Var "x" -> x
    | Var var -> failwith $"Variable '%s{var}' not allowed."
    | Add (a, b) -> (call a x) + (call b x)
    | Sub (a, b) -> (call a x) - (call b x)
    | Mul (a, b) -> (call a x) * (call b x)
    | Pow (a, b) -> Math.Pow(call a x, b)

let rec show expr =
    match expr with
    | Val number -> string number
    | Var var -> var
    | Add (a, b) -> $"{show a}+{show b}"
    | Sub (a, b) -> $"{show a}-{show b}"
    | Mul (a, b) -> $"{show a}*{show b}"
    | Pow (a, b) -> $"{show a}**{b}"
