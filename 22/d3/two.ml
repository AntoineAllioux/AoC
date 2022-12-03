open Base
open Stdio
open Char
open List

let input = In_channel.read_lines "input"

let priority c =
  if (c >= 'A' && c <= 'Z') then
    to_int c - 38
  else if (c >= 'a' && c <= 'z') then
    to_int c - 96
  else
    failwith "Incorrect character"

let find_duplicate l1 l2 =
  let f acc x =
    if (not (exists acc (fun y -> x = y))
        && exists l1 (fun y -> x = y)) then
      x :: acc
    else
      acc
  in fold l2 ~init:[] ~f:f 

let processed =
  map input ~f:String.to_list
  |> chunks_of ~length:3
  |> map ~f:(fun [l1; l2; l3] -> find_duplicate (find_duplicate l1 l2) l3)
  |> join
  |> map ~f:priority

let total = Option.value_exn (reduce processed ( + ))
 
