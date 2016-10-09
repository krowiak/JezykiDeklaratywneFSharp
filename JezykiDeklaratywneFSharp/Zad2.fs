module Zad2

// Jeżeli x zawsze >= 0, inaczej trzeba pomyśleć, ale inaczej chyba zadanie nie ma sensu
let rec r x = 
    match x with
    | 0 -> []
    | _ -> List.append (r (x - 1)) (List.replicate x x)