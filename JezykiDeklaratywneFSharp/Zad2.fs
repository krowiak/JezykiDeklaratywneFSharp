module Zad2

let rec r = function
    | 0 -> []
    | x -> List.append (r (x - 1)) (List.replicate x x)