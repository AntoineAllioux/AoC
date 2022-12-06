open Base
open Stdio
open List
open Continue_or_stop

let rec int_fold_until (n : int) ~(init : 'a) ~(f : 'a -> int -> ('a, 'b) Continue_or_stop.t)
          ~(finish : 'a -> 'b) =
  if n = 0 then
    finish init
  else
    match f init n with 
    | Stop x -> x
    | Continue x -> int_fold_until (n - 1) ~init:x ~f:f ~finish:finish

let find_marker input len pos =
  let aux m = 
    let pos = length input - m in
    let l = sub input ~pos:pos ~len:len in
    if Option.is_some (find_a_dup l ~compare:Char.compare) then
      Continue pos
    else
      Stop (pos + len)
  in int_fold_until (length input - pos) ~init:0 ~f:(fun _ -> aux) ~finish:(fun _ -> -1)

let input =
  In_channel.read_lines "input"
  |> hd_exn
  |> String.to_list
  
let one =
  find_marker input 4 0

let two =
  find_marker input 14 one

let () = printf "one: %d, two: %d" one two
