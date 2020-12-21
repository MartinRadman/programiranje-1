(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Namig: Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

let reverse list =
  let rec pomozna_obrni delni = function
    | [] -> delni
    | x :: xs -> pomozna_obrni (x :: delni) xs
  in
  pomozna_obrni [] list

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let rec repeat x n = if n < 1 then [] else x :: repeat x (n - 1)

(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

let range n =
  let rec range_aux list n = 
    if n < 0 then list else range_aux (n :: list) (n - 1)
  in
  range_aux [] n

(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f x0; f x1; f x2; ...].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+) 2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs 

(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x + 2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let map_tlrec f list =
  let rec map_tlrec_aux f acc = function
    | [] -> acc
    | x :: xs -> map_tlrec_aux f (acc @ [f x]) xs
  in
  map_tlrec_aux f [] list

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] je ekvivalentna python kodi:

  def mapi(f, list):
      mapi_list = []
      index = 0
      for x in list:
          mapi_list += [f(x, index)]
          index += 1
      return mapi_list

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

let  mapi f =
  let rec mapi_aux i acc = function
    | [] -> reverse acc
    | x :: xs -> mapi_aux (i + 1) (f x i :: acc) xs
  in
  mapi_aux 0 []

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

let  zip list1 list2 =
  let rec zip_aux list1 list2 acc =
    match (list1, list2) with
      | ([], []) -> reverse acc
      | (x1 :: xs1, x2 :: xs2) -> zip_aux xs1 xs2 ((x1, x2) :: acc)
      | _ -> failwith "Different lengths of input lists."
  in
  zip_aux list1 list2 []

(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip = function
  | [] -> [], []
  | (x, y) :: t -> let (list1, list2) = unzip t in x :: list1, y :: list2

(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let unzip_tlrec list = 
  let rec unzip_tlrec_aux acc1 acc2 = function
    | [] -> reverse acc1, reverse acc2
    | (x, y) :: tl -> unzip_tlrec_aux (x :: acc1) (y :: acc2) tl
  in
  unzip_tlrec_aux [] [] list

(*----------------------------------------------------------------------------*]
 Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
      while condition(x):
          x = f(x)
      return x

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # loop (fun x -> x < 10) ((+) 4) 4;;
 - : int = 12
[*----------------------------------------------------------------------------*)

let rec loop condition f x = if condition x then loop condition f (f x) else x 

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let fold_left_no_acc f list =
  let rec fold_left_no_acc_aux f = function
      | [x; y] -> f y x
      | x :: xs -> f (fold_left_no_acc_aux f xs) x
      | _ -> failwith "Seznam ima premalo elementov."
  in
  fold_left_no_acc_aux f (reverse list)

(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let apply_sequence f x n =
  let rec apply_sequence_aux f x n acc =
    if n < 1 then
      reverse acc
    else
      apply_sequence_aux f (f x) (n - 1) (f x :: acc)
  in
  apply_sequence_aux f x n [x]

(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let filter f list =
  let rec filter_aux f list acc =
    match list with
      | [] -> reverse acc
      | x :: xs -> if f x then filter_aux f xs (x :: acc) else filter_aux f xs acc
  in
  filter_aux f list []

(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec exists f = function
  | [] -> false
  | x :: xs -> if f x then true else exists f xs

    let izlusci_vrednost = function
    | Some vrednost -> vrednost
    | None -> 69

  let rec sestej_in_preveri_ali_je_splosno splosno x = function
    | [] -> None
    | a :: rep -> if x + a = (2020 - splosno) then Some (x * a) else sestej_in_preveri_ali_je_splosno splosno x rep


  let rec preveri_vse_vsote_splosno splosno = function
    | [] -> None
    | x :: xs -> let vrednost = sestej_in_preveri_ali_je_splosno splosno x xs
      in
      if vrednost <> None then
      vrednost
      else
      preveri_vse_vsote_splosno x xs

  let rec preveri_vse_trojne_vsote = function
    | [] -> None
    | x :: [] -> None
    | x1 :: xs -> let vrednost = preveri_vse_vsote_splosno x1 xs
    in
    if vrednost <> None then
    Some (x1 * izlusci_vrednost vrednost)
    else
    preveri_vse_trojne_vsote xs

  let preveri_dolzino d niz = if String.length niz >= d then true else false

  let explode input =
    let rec aux idx lst =
      if idx<0 then lst else aux (idx-1) (input.[idx] :: lst)
    in aux (String.length input - 1) []

    let rec se_da_sesteti2 x xs kandidat =
    match xs with
    | [] -> false
    | y :: ys -> if y + x = kandidat then true else se_da_sesteti2 x ys kandidat

  let rec se_da_sesteti1 opazovani kandidat =
    match opazovani with
    | [] -> false
    | x :: xs -> if se_da_sesteti2 x xs kandidat then true else se_da_sesteti1 xs kandidat

  let rec preveri_stevilke opazovani = function
    | [] -> failwith "Seznam nima ustreznega števila."
    | x :: xs -> if List.length opazovani <> 25 then
        preveri_stevilke (opazovani @ [x]) xs
      else
        if se_da_sesteti1 opazovani x then
          preveri_stevilke (List.tl opazovani @ [x]) xs
        else
          x



  let sum sez = List.fold_left (+) 0 sez

  let sestej_min_max sez =
    let min = List.fold_left (fun x y -> if x < y then x else y) 0 sez in
    let max = List.fold_left (fun x y -> if x > y then x else y) 0 sez in
    min + max

  let rec skrajsaj iskano = function
    | [] -> []
    | x :: xs -> if sum xs > iskano then skrajsaj iskano xs else xs

(* [6817951; 7713384; 11509827; 7749170; 7760870; 7796656; 8824488; 8860105; 8895891; 10281770; 12554407; 15536208; 9667607; 16645061; 13548127; 9923723; 13692660] *)

  let rec preveri_vsote_zaporednih opazovani iskano = function
    | [] -> failwith "Ne obstaja."
    | x :: xs -> if List.length opazovani < 2 then
        preveri_vsote_zaporednih (opazovani @ [x]) iskano xs
      else
        let vsota = sum opazovani in
        if vsota > iskano then 
          let nov_sez = skrajsaj iskano opazovani in
          if sum nov_sez = iskano then
            nov_sez
          else
            preveri_vsote_zaporednih (nov_sez @ [x]) iskano xs
        else
          if vsota = iskano then
            opazovani
          else
            preveri_vsote_zaporednih (opazovani @ [x]) iskano xs


  let explode input =
    let rec aux idx lst =
      if idx<0 then lst else aux (idx-1) (input.[idx] :: lst)
    in aux (String.length input - 1) []

  let pripravi_array sezsez =
    sezsez |> List.map Array.of_list |> Array.of_list

  let sosede_v_seznam array y x =
    match y, x with
    | 0, x ->
      (match x with
      | 0 -> [array.(1).(0); array.(0).(1); array.(1).(1)]
      | 9 -> [array.(0).(8); array.(1).(0); array.(1).(9)]
      | _ -> [array.(0).(x - 1); array.(0).(x + 1); array.(1).(x - 1); array.(1).(x); array.(1).(x + 1)])
    | y, 0 ->
      (match y with
      | 9 -> [array.(9).(1); array.(8).(0); array.(8).(1)]
      | _ -> [array.(y - 1).(0); array.(y - 1).(1); array.(y).(1); array.(y + 1).(0); array.(y + 1).(1)])
    | 9, x ->
      (match x with
      | 9 -> [array.(8).(9); array.(8).(8); array.(9).(9)]
      | _ -> [array.(9).(x - 1); array.(9).(x + 1); array.(8).(x - 1); array.(8).(x); array.(8).(x + 1)])
    | y, 9 -> [array.(y - 1).(9); array.(y + 1).(9); array.(y - 1).(8); array.(y).(8); array.(y + 1).(8)]
    | y, x -> [array.(y - 1).(x); array.(y + 1).(x); array.(y - 1).(x - 1); array.(y).(x - 1); array.(y + 1).(x - 1); array.(y - 1).(x + 1); array.(y).(x + 1); array.(y + 1).(x + 1)]

  let rec prestej_lojtre st = function
    | [] -> st
    | x :: xs -> if x = '#' then prestej_lojtre (st + 1) xs else prestej_lojtre st xs

  let index_naslednjega y x =
    if y <> 9 then 
      y + 1, x
    else
      if x <> 9 then
        0, x + 1
      else
        -1, -1

  let rec izvedi_korak stari_array array y x =
    if (y, x) = (-1, -1) then array else
    let st_lojter = (sosede_v_seznam stari_array y x) |> prestej_lojtre 0 in
    let yn, xn = index_naslednjega y x in
    if (array.(y).(x) = '#' && st_lojter >= 4) then
      (array.(y).(x) <- 'L';
      izvedi_korak stari_array array yn xn)
    else
      if array.(y).(x) = 'L' && st_lojter = 0 then
        (array.(y).(x) <- '#';
        izvedi_korak stari_array array yn xn)
      else
        izvedi_korak stari_array array yn xn

  let rec izvajaj_korake_dokler_so_spremembe array =
    let stari = array |> Array.copy |> Array.map Array.copy in
    let nov_array = izvedi_korak stari array 0 0 in
    if stari = nov_array then nov_array else izvajaj_korake_dokler_so_spremembe nov_array

  let rec prestej_zasedene y x st array =
    if (y, x) = (-1, -1) then st else
    let yn, xn = index_naslednjega y x in
    match array.(y).(x) with
    | '#' -> prestej_zasedene yn xn (st + 1) array
    | _ -> prestej_zasedene yn xn st array

  let rec preveri vr y x =
    if (y, x) <> (-1, -1) then
      let yn, xn = index_naslednjega y x in
      preveri ((yn, xn) :: vr) yn xn
    else
      vr
    

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let rec first f default = function
  | [] -> default
  | x :: xs -> if f x then x else first f default xs


let array = [|[|'#'; '.'; '#'; '#'; '.'; '#'; '#'; '.'; '#'; '#'|];
              [|'#'; '#'; '#'; '#'; '#'; '#'; '#'; '.'; '#'; '#'|];
              [|'#'; '.'; '#'; '.'; '#'; '.'; '.'; '#'; '.'; '.'|];
              [|'#'; '#'; '#'; '#'; '.'; '#'; '#'; '.'; '#'; '#'|];
              [|'#'; '.'; '#'; '#'; '.'; '#'; '#'; '.'; '#'; '#'|];
              [|'#'; '.'; '#'; '#'; '#'; '#'; '#'; '.'; '#'; '#'|];
              [|'.'; '.'; '#'; '.'; '#'; '.'; '.'; '.'; '.'; '.'|];
              [|'#'; '#'; '#'; '#'; '#'; '#'; '#'; '#'; '#'; '#'|];
              [|'#'; '.'; '#'; '#'; '#'; '#'; '#'; '#'; '.'; '#'|];
              [|'#'; '.'; '#'; '#'; '#'; '#'; '#'; '.'; '#'; '#'|]|]

let stari = array |> Array.copy |> Array.map Array.copy

let novi = izvedi_korak stari array 0 0
