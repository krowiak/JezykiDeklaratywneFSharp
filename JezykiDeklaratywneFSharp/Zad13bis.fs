module Zad13bis

let f = 
    let mutable f_m3 = bigint(1)
    let mutable f_m2 = bigint(2)
    let mutable f_m1 = bigint(3)
    seq {
        yield bigint(1)
        yield bigint(2)
        yield bigint(3)
        while true do
            let f_0 = f_m1 + f_m2 + f_m3
            yield f_0
            f_m3 <- f_m2
            f_m2 <- f_m1
            f_m1 <- f_0
    }

let rozwiazanie () = f |> Seq.take 20 |> Seq.iter (printfn "%A")
