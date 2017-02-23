module Zad15
open System

let rec cfrac (x:decimal) : seq<bigint> = 
    let (n, ux) = (bigint x, x - Math.Truncate(x))
    seq {
        yield n
        if n <> bigint (Math.Ceiling(x)) then
            yield! cfrac (1.0M/ux)
    }

let floatFrac = decimal >> cfrac
let a = 2.0 |> sqrt |> floatFrac
let b = 3.0 |> sqrt |> floatFrac
let c = 5.0 |> sqrt |> floatFrac
let d = 19.0 |> sqrt |> floatFrac
let e = (415.0/99.0) |> floatFrac
let f = Math.E |> floatFrac
let g = 1.0 |> Math.Tan |> floatFrac
let pokaz ile cfracs id =
    printf "%s): " id 
    for cf in Seq.take ile cfracs do
        printf "%A, " cf
    printfn ";"

let pokazuj () =
    let ile = 20
    pokaz ile a "a" |> ignore
    pokaz ile b "b" |> ignore
    pokaz ile c "c" |> ignore
    pokaz ile d "d" |> ignore
    pokaz ile e "e" |> ignore
    pokaz ile f "f" |> ignore
    pokaz ile g "g" |> ignore
