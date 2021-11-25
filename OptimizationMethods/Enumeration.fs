module OptimizationMethods.Enumeration

let argmin (start, finish) n func =
    seq {
        for i in 1 .. 2 .. 2 * n ->
            start + (float i) * (finish - start) / float (2 * n)
    }
    |> Seq.map func
    |> Seq.min