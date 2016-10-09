module Zad13

let rec f n =
    match n with
    | 0 -> 1
    | 1 -> 2
    | 2 -> 3
    | _ -> f (n-1) + f (n-2) + f (n-3)

let odp = Seq.initInfinite f |> Seq.take 20
// Odpowiedź: [1; 2; 3; 6; 11; 20; 37; 68; 125; 230; 423; 778; 1431; 2632; 4841; 8904; 16377; 30122; 55403; 101902]