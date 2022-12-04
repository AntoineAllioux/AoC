open Base
open Stdio
open List
open Int

let input = In_channel.read_lines "input"

let parse s =
  let [a; b; c; d] = String.split_on_chars s ~on:['-'; ',']
  in ((of_string a, of_string b), (of_string c, of_string d))

let contains ((a, b), (c, d)) =
  (a >= c && b <= d) || (c >= a && d <= b)

let overlap ((a, b), (c, d)) =
  (a >= c && a <= d) || (b >= c && b <= d)
  || (c >= a && c <= b) || (d >= a && d <= b)
  
let one =
  map input ~f:parse
  |> count ~f:contains

let two =
  map input ~f:parse
  |> count ~f:overlap
 
let () = printf "one: %d, two: %d" one two
