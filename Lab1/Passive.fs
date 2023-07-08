module Lab1.Passive

open System.Diagnostics

let argmin (start, finish) n f =
    let k = n / 2
    let step = (finish - start) / float (k + 1)

    let delta = step / 2.0 * 10e-2

    let points =
        let p = Array.zeroCreate (n + 2)
        p.[0] <- start
        p.[n + 1] <- finish
        p

    for i in 1..k do
        points.[2 * i] <- start + step * float i
        points.[2 * i - 1] <- points.[2 * i] - delta

    Debug.Assert(points.[0] = start)
    Debug.Assert(Array.last points = finish)
    Debug.Assert(points.Length = n + 2)

    let minIndex =
        points.[1..n]
        |> Seq.map (fun x -> (x, f x))
        |> Seq.indexed
        |> Seq.minBy (snd >> snd)
        |> fst
        |> (+) 1

    (points.[minIndex - 1] + points.[minIndex + 1]) / 2.0
