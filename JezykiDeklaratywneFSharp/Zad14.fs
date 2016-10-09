module Zad14

// Trochę brzydki klon Haskella, ale powinien działać.
// BONUS: odpowiedzią na zad.6 z Haskella w ZPO była kompozycja, yay

let rec kompozycje m n =
    if m > n then Seq.empty
    else 
        if m = 1 then [[n]] |> List.toSeq
        else seq {
            for i in [1..n-m+1] do
                for xs in kompozycje (m-1) (n-i) do
                    yield i::xs
        }
