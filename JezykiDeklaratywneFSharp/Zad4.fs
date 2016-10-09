module Zad4

let rec zawiera xs ys =
    match xs with
    | [] -> true
    | x::zs -> (List.contains x ys) && (zawiera zs ys)