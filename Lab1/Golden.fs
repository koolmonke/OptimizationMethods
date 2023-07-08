module Lab1.Golden

let biggerRoot = (1.0 + sqrt 5.0) / 2.0

let smallerRoot = (1.0 - sqrt 5.0) / 2.0

let xNew x left right = left + (right - left) * x

let argmin range n f =
    let mutable left, right = range
    let mutable xRight = xNew -smallerRoot left right
    let mutable xLeft = xNew (1.0 + smallerRoot) left right

    for i in 0 .. (n - 2) do
        let len = xRight - xLeft

        if f xLeft <= f xRight then
            right <- xRight
            xRight <- xLeft
            xLeft <- left + len
        else
            left <- xLeft
            xLeft <- xRight
            xRight <- right - len

    (left + right) / 2.0
