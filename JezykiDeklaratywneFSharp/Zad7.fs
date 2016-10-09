module Zad7

type Kolejka<'a> =
| Q of List<'a> * List<'a>

type Kolejka<'a> with
member this.dodaj(x : 'a) =
    match this with
    | Q ([], []) -> Q ([x], [])
    | Q (xs, ys) -> Q (xs, x :: ys)
member this.usun() =
    match this with
    | Q ([], ys) -> Q (List.tail (List.rev ys), [])
    | Q (x :: xs, ys) -> Q (xs, ys)
member this.pierwszy() =
    match this with
    | Q ([], ys) -> List.head (List.rev ys)
    | Q (x :: xs, ys) -> x
member this.czyPusty() =
    match this with
    | Q ([], []) -> true
    | Q _ -> false
// Wydaje się działać, ale nie całkiem przetestowane...
member this.prawieCzysc() =
    match this with
    | Q ([], []) -> Q ([], [])
    | Q (x::[], []) -> Q ([x], [])
    | Q _ -> this.usun().prawieCzysc()


