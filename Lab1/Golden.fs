module Lab1.Golden

let bigger_root = (1.0 + sqrt 5.0) / 2.0

let smaller_root = (1.0 - sqrt 5.0) / 2.0

let x_new x left right = left + (right - left) * x

let argmin range n f =
    let mutable left, right = range
    let mutable x_right = x_new -smaller_root left right
    let mutable x_left = x_new (1.0 + smaller_root) left right

    for i in 0 .. (n - 2) do
        let len = x_right - x_left

        if f x_left <= f x_right then
            right <- x_right
            x_right <- x_left
            x_left <- left + len
        else
            left <- x_left
            x_left <- x_right
            x_right <- right - len

    (left + right) / 2.0
