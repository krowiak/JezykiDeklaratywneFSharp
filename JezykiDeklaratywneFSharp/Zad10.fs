module Zad10

// Szybka konwersja kodu z Haskella, który zapomniałem, jak działa, więc trzeba być czujnym
let stopien xs =
    let rowne xs' = Seq.truncate 10 xs' |> Seq.pairwise |> Seq.forall (fun (a,b) -> a = b)
    let roznice xs' = Seq.pairwise xs' |> Seq.map (fun (a,b) -> a - b)
    let rec stopien' xs' acc =
        if rowne xs'
        then List.length acc
        else 
            stopien' (Seq.skip 1 xs' |> roznice) (xs'::acc)
    stopien' xs []