module Zad3

// Bez adnotacji typu działa nie tylko dla list, więc żeby nie było
let f (xs:list<int>) x = 
    xs |> Seq.zip (Seq.initInfinite (fun i -> i)) 
    |> Seq.filter (fun y -> snd y = x) 
    |> Seq.map fst 
    |> Seq.toList