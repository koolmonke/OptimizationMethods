module Functions.Expressions

open System.Collections.Generic


type Expr =
    | Val of float
    | Var of string
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Pow of Expr * Expr
    | Ln of Expr
    | Cos of Expr
    | Sin of Expr
    | Exp of Expr


let rec diff f x =
    match f with
    | Var y when x = y -> Val 1.0
    | Val _
    | Var _ -> Val 0.0
    | Add (f, g) -> Add(diff f x, diff g x)
    | Sub (f, g) -> Sub(diff f x, diff g x)
    | Mul (f, g) -> Add(Mul(f, diff g x), Mul(g, diff f x))
    | Div (f, g) -> Div(Sub(Mul(diff f x, g), Mul(diff g x, f)), Pow(g, Val 2.))
    | Pow (f, Val 2.) -> Mul(Val 2., f)
    | Pow (f, Val k) -> Mul(Val k, Pow(f, Val(k - 1.)))
    | Pow (f, g) -> Mul(Add(Mul(g, diff f x), Mul(Mul(f, Ln f), diff g x)), Pow(f, Sub(g, Val 1.0)))
    | Ln arg -> Div(diff arg x, arg)
    | Sin arg -> Mul(Cos arg, diff arg x)
    | Cos arg -> Mul(Mul(Val -1.0, Sin arg), diff arg x)
    | Exp arg -> Mul(Exp arg, diff arg x)


let rec evalfFromDict expr (vars: IDictionary<string, float>) =
    match expr with
    | Val number -> number
    | Var var ->
        match vars.TryGetValue(var) with
        | true, value -> value
        | false, _ -> failwith $"Переменная '%s{var}' не разрешена."
    | Add (a, b) -> (evalfFromDict a vars) + (evalfFromDict b vars)
    | Sub (a, b) -> (evalfFromDict a vars) - (evalfFromDict b vars)
    | Mul (a, b) -> (evalfFromDict a vars) * (evalfFromDict b vars)
    | Div (a, b) -> (evalfFromDict a vars) / (evalfFromDict b vars)
    | Pow (a, b) -> (evalfFromDict a vars) ** (evalfFromDict b vars)
    | Ln arg -> log (evalfFromDict arg vars)
    | Sin arg -> sin (evalfFromDict arg vars)
    | Cos arg -> cos (evalfFromDict arg vars)
    | Exp arg -> exp (evalfFromDict arg vars)

let evalf expr varName value =
    [ (varName, value) ] |> dict |> evalfFromDict expr

let rec show expr =
    match expr with
    | Val number -> string number
    | Var var -> var
    | Add (a, b) -> $"({show a}+{show b})"
    | Sub (a, b) -> $"({show a}-{show b})"
    | Mul (a, b) -> $"({show a}*{show b})"
    | Div (a, b) -> $"({show a}/{show b})"
    | Pow (a, b) -> $"{show a}**{show b}"
    | Ln arg -> $"Ln({show arg})"
    | Sin arg -> $"Sin({show arg})"
    | Cos arg -> $"Cos({show arg})"
    | Exp arg -> $"Exp({show arg})"
