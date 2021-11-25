open System.Collections.Generic
open OptimizationMethods

let range = (0., 4.)


let memoization f x =
    let dict = Dictionary<_, _>()

    match dict.TryGetValue(x) with
    | true, value -> value
    | false, _ ->
        let tmp = f x
        dict.Add(x, tmp)
        tmp

let f x =
    4.0 * x * x - 8.0 * x + 8.0 + 19.0 * abs (x - 3.0)

let f_memo = memoization f

[<EntryPoint>]
let main _ =
    let n = 22
    let x_star_passive = Passive.argmin range n f_memo
    let x_star_golden = Golden.argmin range n f_memo


    printfn $"x_star для пассивного метода %f{x_star_passive}"
    printfn $"Теоретическая точность %f{(fst range - snd range) / float (n / 2 + 1)}"
    printfn $"Фактическая точность %f{abs (3.0 - x_star_passive)}"

    printfn $"x_star для метода золотого сечения %f{x_star_golden}"
    printfn $"Теоретическая точность %f{(fst range - snd range) / (2.0 * pown Golden.bigger_root (n-1))}"
    printfn $"Фактическая точность %f{abs (3.0 - x_star_golden)}"

    0
