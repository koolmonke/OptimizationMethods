module OptimizationMethods.Passive

let argmin (start, finish) n (f: float -> float) : float =
    let step = (finish - start) / float (n / 2 + 1)

    let points =
        seq { for i in 0 .. n + 2 -> start + step * (float i) } |> Seq.toList

    let pairs = points |> Seq.map (fun x -> (x, f x))

    let min_index =
        pairs
        |> Seq.indexed
        |> Seq.minBy (snd >> snd)
        |> fst

    let x_star =
        (points.[min_index - 1] + points.[min_index + 1])
        / 2.0

    x_star
