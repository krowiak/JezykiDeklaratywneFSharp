module Zad8

// https://stackoverflow.com/questions/286427/calculating-permutations-in-f
let rec permutations list taken = 
  seq { if Set.count taken = List.length list then yield [] else
        for l in list do
          if not (Set.contains l taken) then 
            for perm in permutations list (Set.add l taken)  do
              yield l::perm }

let permutuj lista = permutations lista Set.empty

// Do podglądu:
// for x in permutuj [1..3] do printfn "%A" x