open System.Collections.Generic
open Lab1

let memoization f =
    let dict = Dictionary<_, _>()

    fun x ->
        match dict.TryGetValue(x) with
        | true, value -> value
        | false, _ ->
            let tmp = f x
            dict.Add(x, tmp)
            tmp

let n = 22

let range = (0., 4.)

let f =
    let inner_f x =
        4.0 * x ** 2.0 - 8.0 * x
        + 8.0
        + 19.0 * abs (x - 3.0)

    memoization inner_f

let actual = 3.0

let xStarPassive = Passive.argmin range n f
let xStarGolden = Golden.argmin range n f


printfn $"x* для пассивного метода {xStarPassive}"
printfn $"Теоретическая точность {(snd range - fst range) / float (n / 2 + 1)}"
printfn $"Фактическая точность {abs (actual - xStarPassive)}"

printfn $"x* для метода золотого сечения {xStarGolden}"

printfn
    $"Теоретическая точность {(fst range - snd range)
                              / (2.0 * pown Golden.biggerRoot (n - 1))}"

printfn $"Фактическая точность {abs (actual - xStarGolden)}"
