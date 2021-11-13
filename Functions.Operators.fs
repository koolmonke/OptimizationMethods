module Functions.Operators

open Functions.Expressions
let (+) (left: Expr) (right: Expr) = Add(left, right)
let (-) (left: Expr) (right: Expr) = Sub(left, right)
let (*) (left: Expr) (right: Expr) = Mul(left, right)
let ( ** ) (left: Expr) (right: float) = Pow(left, right)
