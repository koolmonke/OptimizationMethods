module Optimization.Enumeration

let methodName = "Метод перебора"

let private segments n start finish : seq<float> =
    seq {
        for i in 1 .. 2 .. 2 * n ->
            start + (float i) * (finish - start) / float (2 * n)
    }

let argmin n ((start, finish): float * float) (func: float -> float): float =
    segments n start finish
    |> Seq.map func
    |> Seq.min