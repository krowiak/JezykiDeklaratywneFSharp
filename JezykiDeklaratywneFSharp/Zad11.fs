module GenProg

open System;

[<Literal>]
let rozmiar_turnieju = 5  // nie mniej niz 3

[<Literal>]
let rozmiar_populacji = 200

[<Literal>]
let poczatkowa_wysokosc_drzew = 3

[<Literal>]
let liczba_iteracji = 10000

[<Literal>]
let duzy_blad = 99999.0

let N =  // "nonterminals" czyli funkcje
  [| (+); (-); (*); (/) |]

let T =  // "terminals" czyli stale (zmiennych nie wpisujemy)
  [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
  
let Dane = array2D [| [| 1.0; 0.368 |];  // x0, x1, f(x0, x1)
                      [| 2.0; 0.432 |];
                      [| 3.0; 0.504 |];
                      [| 4.0; 0.561 |];
                      [| 5.0; 0.606 |];
                      [| 6.0; 0.642 |];
                      [| 7.0; 0.671 |];
                      [| 8.0; 0.695 |];
                      [| 9.0; 0.716 |];
                      [| 10.0; 0.733 |];
                      [| 20.0; 0.830 |];
                      [| 50.0; 0.913 |] |]

let liczba_obiektow  = Array2D.length1 Dane
let liczba_zmiennych = Array2D.length2 Dane - 1
let BLAD_MAX = 0.02

let wartosci = [| for i in 0 .. liczba_obiektow - 1 -> Dane.[i, liczba_zmiennych] |]

type Wyrazenie<'a> =
  | Operator of ('a -> 'a -> 'a) * Wyrazenie<'a> * Wyrazenie<'a>
  | Zmienna of int
  | Stala of int

let MAX_OPERATOROW = 6
let rec ileOperatorow wyrazenie =
    match wyrazenie with
    | Operator(_, lewe, prawe) -> 1 + ileOperatorow lewe + ileOperatorow prawe
    | _ -> 0

let rec oblicz (i : int) = function
  | Operator(f, lewe, prawe) -> f (oblicz i lewe) (oblicz i prawe)
  | Zmienna(j) -> Dane.[i,j]
  | Stala(k) -> T.[k]  

let rec pisz = function
  | Operator(f, lewe, prawe) -> 
      match f with
      | _ when f.ToString() = N.[0].ToString() -> 
            "(" + pisz lewe + "+" + pisz prawe + ")"
      | _ when f.ToString() = N.[1].ToString() -> 
            "(" + pisz lewe + "-" + pisz prawe + ")"
      | _ when f.ToString() = N.[2].ToString() -> 
            "(" + pisz lewe + "*" + pisz prawe + ")"
      | _ when f.ToString() = N.[3].ToString() -> 
            "(" + pisz lewe + "/" + pisz prawe + ")"
  | Zmienna(j) -> "x" + string j
  | Stala(k) -> string (T.[k]) 

// Tak by³o od pocz¹tku
let podstawowaOcena = (fun (f, g) -> pown (f - g) 2)
// Próba przyœpieszenia osi¹gania efektu dla którego wszystkie oceny w zakresie 0,02
let watpliwaPoprawkaOceny = (fun (f, g) -> if (f - g) < BLAD_MAX then (pown (f - g) 2)*0.5 else pown (f - g) 2)
let FUN_OCENY = watpliwaPoprawkaOceny

let ocena (wyr : Wyrazenie<'a>)  =
  if ileOperatorow wyr > MAX_OPERATOROW
  then duzy_blad
  else
    try
    [| for i in 0 .. liczba_obiektow - 1 -> oblicz i wyr |]
    |> Array.zip wartosci
    |> Array.map FUN_OCENY
    |> Array.sum
    with
    | :? OverflowException -> duzy_blad

let los = new Random()  // generator liczb pseudolosowych

let rec generuj (wys : int) : Wyrazenie<'a> =
  if wys > 0 then
    Operator( N.[los.Next(N.Length)], generuj (wys - 1), generuj (wys - 1) )  
  else
    let r = los.Next(2)
    if r = 1 then
      Zmienna( los.Next(liczba_zmiennych) )
    else
      Stala( los.Next(T.Length) )

let rec wybierzFragment (pr : float) (wyr : Wyrazenie<'a>) =
  match wyr with
  | Operator(f, lewe, prawe) ->
      if los.NextDouble() < 0.5 then
        if los.NextDouble() < pr then lewe
        else wybierzFragment pr lewe
      else
        if los.NextDouble() < pr then prawe
        else wybierzFragment pr prawe
  | _ -> wyr

let rec wstawFragment (pr : float) (cel : Wyrazenie<'a>) (zrodlo : Wyrazenie<'a>) =
  match cel with
  | Operator(f, lewe, prawe) ->
      if los.NextDouble() < 0.5 then
        if los.NextDouble() < pr then Operator(f, zrodlo, prawe)
        else Operator(f, wstawFragment pr lewe zrodlo, prawe)
      else
        if los.NextDouble() < pr then Operator(f, lewe, zrodlo)
        else Operator(f, lewe, wstawFragment pr prawe zrodlo)
  | _ -> zrodlo

let krzyzuj (w1 : Wyrazenie<'a>) (w2 : Wyrazenie<'a>) : Wyrazenie<'a> =
  let r = los.NextDouble()
  w1 
  |> wybierzFragment r 
  |> wstawFragment r w2

let ocenyComparer (oceny:float[]) x y =
    let roznica = oceny.[x] - oceny.[y]
    if roznica < 0.0
    then -1
    else 
        if roznica > 0.0
        then 1
        else 0

let algorytmEwolucyjny () =
  let Populacja = Array.init rozmiar_populacji (fun i -> generuj poczatkowa_wysokosc_drzew)
  let Oceny = Array.map ocena Populacja
  let Wylosowane : int [] = Array.zeroCreate rozmiar_turnieju
  for iteracja in 1 .. liczba_iteracji do
    for i in 0 .. rozmiar_turnieju - 1 do
      Wylosowane.[i] <- los.Next(rozmiar_populacji)
    Array.Sort(Wylosowane, ocenyComparer Oceny)
    let i1 = Wylosowane.[0]
    let i2 = Wylosowane.[1]
    let iost = Wylosowane.[rozmiar_turnieju - 1]
    let nowy = krzyzuj (Populacja.[i1]) (Populacja.[i2])
    Populacja.[iost] <- nowy
    Oceny.[iost] <- ocena nowy
  Array.Sort(Oceny, Populacja)
  Populacja

let znajdzOdpowiedniego (populacja:Wyrazenie<float>[]) =
    let nieZaDuzoOperatorow = fun osobnik -> ileOperatorow osobnik <= MAX_OPERATOROW
    let nieZaMaloPrecyzji = fun osobnik -> 
        try
        [| for i in 0 .. liczba_obiektow - 1 -> oblicz i osobnik |]
        |> Seq.zip wartosci
        |> Seq.map (fun (f, g) -> Math.Abs(f - g) < BLAD_MAX)
        |> Seq.forall (fun x -> x)
        with
        | :? OverflowException -> false
    Array.tryFind (fun osobnik -> nieZaDuzoOperatorow osobnik && nieZaMaloPrecyzji osobnik) populacja

let pokazZeDobrze rozwiazanie =
    printfn ""
    printfn "Szczegolowe wyniki:"
    for i in 0 .. liczba_obiektow - 1 do
        let cel = Dane.[i, 1]
        let otrzymano = oblicz i rozwiazanie
        let roznica = Math.Abs(cel - otrzymano)
        printfn "n=%f: oczekiwano %f, otrzymano %f (roznica %f)" Dane.[i, 0] cel otrzymano roznica

[<EntryPoint>]
let main (args : string []) =
  let mutable czasKonczyc = false
  let mutable najlepsiOsobnicy = algorytmEwolucyjny()
  let mutable licznikProb = 1
  while not czasKonczyc do
    match znajdzOdpowiedniego najlepsiOsobnicy with
    | Some osobnik -> 
      printfn "Ocena: %A, wyrazenie: %s" (ocena osobnik) (pisz osobnik)
      pokazZeDobrze osobnik
      czasKonczyc <- true
    | None ->
      najlepsiOsobnicy <- algorytmEwolucyjny ()
      printfn "%i. proba nieudana, ponawianie" licznikProb
      licznikProb <- licznikProb + 1
  System.Console.ReadLine() |> ignore
  0
