module Zad1

//[1;2;3;4;5;6;7;8;10;11;12;13;14;15]
let a = List.filter ((<>) 9) [1..15]

//[2;-3;4;-5;6;-7;8;-9;10;-11;12;-13;14;-15]
let b = List.map (fun x -> if x % 2 = 0 then x else -x) [2..15]