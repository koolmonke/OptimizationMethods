module Functions.Operators

open Functions.Expressions

let (~-) (expr: Expr) = Mul(Val -1.0, expr)
let (+) (left: Expr) (right: Expr) = Add(left, right)
let (-) (left: Expr) (right: Expr) = Sub(left, right)
let (*) (left: Expr) (right: Expr) = Mul(left, right)
let ( ** ) (left: Expr) (right: Expr) = Pow(left, right)
