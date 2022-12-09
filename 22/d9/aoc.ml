open Base
open Stdio

let rec int_fold (n : int) ~(init : 'a) ~(f : 'a -> int -> 'a) =
  if n <= 0 then
    init
  else
    int_fold (n - 1) ~init:(f init n) ~f

type move = U | D | R | L

 module Pair = struct
   type t = int * int [@@deriving compare, sexp_of]
 end

module Lexicographical_order = struct 
  include Pair
  include Base.Comparator.Make(Pair)
end

let parse = function
  | "U" -> U
  | "D" -> D
  | "R" -> R
  | "L" -> L

let move_head (x, y) move =
  match move with
  | U -> (x, y + 1)
  | D -> (x, y - 1)
  | R -> (x + 1, y)
  | L -> (x - 1, y)

let sign n = if n >= 0 then 1 else -1

let move_knot (hx, hy) (tx, ty) =
  let dx = hx - tx in
  let dy = hy - ty in
  let new_x =
    if abs dx > 1 || (abs dy > 1 && abs dx = 1)  then
      tx + sign dx 
    else tx in
  let new_y =
    if abs dy > 1 || (abs dx > 1 && abs dy = 1) then 
      ty + sign dy
    else
      ty
  in (new_x, new_y)

let init_knots n = int_fold n ~init:[] ~f:(fun l _ -> (0, 0) :: l)

let process_moves n moves =
  let process_move (knots, history) (move, n) =
    let update_knots (knots, history) =
      let updated_head = move_head (List.hd_exn knots) move in 
      let update_knot l knot = (move_knot (List.hd_exn l) knot) :: l in
      let updated_knots = List.fold (List.tl_exn knots) ~init:[updated_head] ~f:update_knot in
      let updated_history = Set.add history (List.hd_exn updated_knots) in
      (List.rev updated_knots , updated_history) in
    int_fold n ~init:(knots, history) ~f:(fun acc _ -> update_knots acc) in
  let history = Set.empty (module Lexicographical_order) in
  snd (List.fold moves ~init:(init_knots n, history) ~f:process_move)

let moves =
  let f s =
    let [move; n] = String.split s ~on:' ' in
    (parse move, Int.of_string n) in
  In_channel.read_lines "input"
  |> List.map ~f

let one = Set.length (process_moves 2 moves)

let two = Set.length (process_moves 10 moves)

let () = printf "one: %d, two: %d" one two

