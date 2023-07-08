module Lab2.Polyline


let argmin (left, right) accuracy f fp =
    let L = [ left; right ] |> List.map (fp >> abs) |> List.max

    let rec rArgmin (left, right) iterCount =
        let fLeft = f left
        let fRight = f right

        let x0 = (1.0 / (2.0 * L)) * (fLeft - fRight + L * (left + right))

        let p0 = 0.5 * (fLeft + fRight + L * (left - right))

        let d = (1.0 / (2.0 * L)) * (f x0 - p0)
        let x1p = x0 - d
        let x1pp = x0 + d
        let ld = 2.0 * L * d
        let y1p = fp x1p
        let y1pp = fp x1pp

        match ld > accuracy, abs y1p < abs y1pp with
        | true, true -> rArgmin (left, x0) (iterCount + 1)
        | true, false -> rArgmin (x0, right) (iterCount + 1)
        | false, _ -> x0, iterCount

    let rArgmin, iterCount = rArgmin (left, right) 0
    f rArgmin, iterCount
