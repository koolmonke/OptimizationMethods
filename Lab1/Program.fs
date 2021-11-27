open System.Collections.Generic
open OptimizationMethods

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

[<EntryPoint>]
let main _ =
    let x_star_passive = Passive.argmin range n f
    let x_star_golden = Golden.argmin range n f


    printfn $"x_star для пассивного метода %f{x_star_passive}"
    printfn $"Теоретическая точность %f{(fst range - snd range) / float (n / 2 + 1)}"
    printfn $"Фактическая точность %f{abs (3.0 - x_star_passive)}"

    printfn $"x_star для метода золотого сечения %f{x_star_golden}"

    printfn
        $"Теоретическая точность %f{(fst range - snd range)
                                    / (2.0 * pown Golden.bigger_root (n - 1))}"

    printfn $"Фактическая точность %f{abs (3.0 - x_star_golden)}"

    0
