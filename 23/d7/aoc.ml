open Base
open Stdio
open List
open Container.Continue_or_stop
open Fn

let mapping1 = function
  | '1' .. '9' as c -> Int.of_string (String.of_char c)
  | 'T' -> 10 
  | 'J' -> 11
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
     
let mapping2 = function
  | 'J' -> 0
  | '1' .. '9' as c -> Int.of_string (String.of_char c)
  | 'T' -> 10 
  | 'Q' -> 11
  | 'K' -> 12
  | 'A' -> 13

let check_tgts1 h tgts =
  let occurences_aux acc x =
    let update_occurence = function
      | None -> Some 1
      | Some v -> Some (v + 1) in
    Map.change acc x ~f:update_occurence in
  let check_tgt tgts x =
    match tgts with
    | [] -> Stop true
    | tgt :: tgts ->
       if x = tgt then
         Continue tgts
       else
         Stop false in
    fold h ~init:(Map.empty (module Char)) ~f:occurences_aux
    |> Map.to_alist
    |> map ~f:snd
    |> sort ~compare:(flip Int.compare)
    |> fold_until ~init:tgts ~f:check_tgt ~finish:is_empty
    
  
let check_tgts2 h tgts =
  let occurences_aux acc x =
    let update_occurence = function
      | None -> Some 1
      | Some v -> Some (v + 1) in
    Map.change acc x ~f:update_occurence in 
  let occurences_map = fold h ~init:(Map.empty (module Char)) ~f:occurences_aux in
  let js =
    match Map.find occurences_map 'J' with
    | None -> 0
    | Some v -> v in
  let occurences =
    Map.remove occurences_map 'J'
    |> Map.to_alist
    |> map ~f:snd
    |> sort ~compare:(flip Int.compare) in
  let check_tgt js occ tgt =
    if js >= tgt then
      Some (tgt - js, occ)
    else
      match occ with
      | [] -> None
      | x :: xs ->
         if x >= tgt then
           Some (js, xs)
         else if js >= tgt - x then
           Some (js - (tgt - x), xs)
         else
           None in
  let f (b, (js, occ)) tgt =
    match check_tgt js occ tgt with
    | None -> Stop false
    | Some (js', occ') ->
       Continue (b, (js', occ')) in
  fold_until tgts ~init:(true, (js, occurences)) ~f ~finish:fst

let kind check_tgts h =
  if check_tgts h [5] then
    7
  else if check_tgts h [4] then
    6
  else if check_tgts h [3;2] then
    5
  else if check_tgts h [3;1] then
    4
  else if check_tgts h [2;2] then
    3
  else if check_tgts h [2;1;1;1] then
    2
  else
    1

let compare kind mapping h1 h2 =
  let k1, k2 = kind h1, kind h2 in
  if k1 < k2 then
    -1
  else if k1 > k2 then
    1
  else
     compare Int.compare (map h1 ~f:mapping) (map h2 ~f:mapping)

let score l = 
  let f (i, total) (_, score) = (i + 1, i * score + total) in
  List.fold l ~init:(1, 0) ~f
  |> snd

let part1 input =
  sort input ~compare:(fun (c1, _) (c2, _) -> compare (kind check_tgts1) mapping1 c1 c2)
  |> score

let part2 input =
  sort input ~compare:(fun (c1, _) (c2, _) -> compare (kind check_tgts2) mapping2 c1 c2)
  |> score
 
let _ =
  let f s =
    let [s1; s2] = String.split_on_chars s ~on:[' '] in
    (String.to_list s1, Int.of_string s2) in 
  let input =
    In_channel.read_lines "input"
    |> List.map ~f  in
  begin
    printf "Part 1: %d\n" (part1 input);
    printf "Part 2: %d\n" (part2 input)
  end

