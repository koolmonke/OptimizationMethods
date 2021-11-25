module OptimizationMethods.Passive

let argmin (start, finish) n f =
    let step = (finish - start) / float (n / 2 + 1)

    let points = [ start .. step .. float (n + 2) ]

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
