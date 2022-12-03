open Base
open Stdio
open Char
open List

let input = In_channel.read_lines "input"

let priority c =
  if (c >= 'A' && c <= 'Z') then
    Char.to_int c - 38
  else if (c >= 'a' && c <= 'z') then
    Char.to_int c - 96
  else
    failwith "Incorrect character"

let find_duplicate l1 l2 =
  let f acc x =
    if exists l1 ~f:(fun y -> x = y) then
      [x]
    else
      acc
  in fold l2 ~init:[] ~f:f 

let processed =
  let f s =
    let l = String.to_list s in
    let (l1, l2) = split_n l (length l / 2) in
    find_duplicate l1 l2
  in List.map (input >>= f) priority

let total : int = Option.value_exn (reduce processed ( + ))
