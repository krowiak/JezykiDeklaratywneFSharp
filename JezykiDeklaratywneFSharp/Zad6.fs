module Zad6

type Drzewo =
| Wierzcholek of int * Drzewo * Drzewo
| Nil

let rec wywazone xs =
    match xs with
    | [] -> Nil
    | x::ys -> 
        let lewa, prawa = ys |> List.splitAt ((ys |> List.length) / 2)
        Wierzcholek (x, (wywazone lewa), (wywazone prawa))