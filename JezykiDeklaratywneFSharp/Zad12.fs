module Zad12

//1, 2, 4, 8, . . .
let a = Seq.initInfinite (fun x -> pown 2 x)

//1, −1, 2, −2, . . .
let b =
    let nast i = if i < 0 then -i + 1 else -i
    let rec b' i = seq {
        yield i
        yield! b' (nast i)
    }
    b' 1
    
//1, 0, 3, 0, 5, . . .
let c = Seq.initInfinite (fun x -> if (x + 1) % 2 = 0 then 0 else x + 1)

//10, 20, . . .
let d = Seq.initInfinite (fun x -> (x + 1) * 10)