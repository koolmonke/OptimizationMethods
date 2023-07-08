module Lab2.Enumeration

let argmin (start, finish) n func =
    seq { for i in 1..2 .. 2 * n -> func (start + (float i) * (finish - start) / float (2 * n)) }
    |> Seq.min
