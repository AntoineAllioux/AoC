open Base
open Stdio
open Array

let rec int_fold (n : int) ~(init : 'a) ~(f : 'a -> int -> 'a) =
  if n <= 0 then
    init
  else
    int_fold (n - 1) ~init:(f init n) ~f

let range_fold_aux (start : int) (stop : int) ~(init : 'a) ~(f : 'a -> int -> 'a) =
  let len = abs(stop - start) in
  let sign = if stop - start >= 0 then 1 else -1 in
  let f' acc x = f acc (stop - sign * (x - 1)) in
  int_fold (len + 1) ~init ~f:f'

let range_fold_incr (start : int) (stop : int) ~(init : 'a) ~(f : 'a -> int -> 'a) =
  if stop < start then
    init
  else
    range_fold_aux start stop ~init ~f

let range_fold_decr (start : int) (stop : int) ~(init : 'a) ~(f : 'a -> int -> 'a) =
  if stop > start then
    init
  else
    range_fold_aux start stop ~init ~f

type map = int Array.t Array.t

let get_height map x y = Array.get (Array.get map y) x

let map_height map = Array.length map

let map_width map = Array.length (Array.get map 0)

let is_visible map x y =
  let x_len = map_height map in
  let y_len = map_width map in
  let height = get_height map x y in
  let aux acc x y = acc && (get_height map x y < height) in
  range_fold_incr 0 (x - 1) ~init:true ~f:(fun acc n -> aux acc n y)
  || range_fold_incr (x + 1) (x_len - 1) ~init:true ~f:(fun acc n -> aux acc n y)
  || range_fold_incr 0 (y - 1) ~init:true ~f:(fun acc n -> aux acc x n)
  || range_fold_incr (y + 1) (y_len - 1) ~init:true ~f:(fun acc n -> aux acc x n)

let scenic_score map x y =
  let x_len = map_height map in
  let y_len = map_width map in
  let height = get_height map x y in
  let aux (flag, acc) x y =
    if flag then
      if get_height map x y >= height then
        (false, acc + 1)
      else
        (true, acc + 1)
    else
      (flag, acc) in 
  let (_ , a) = range_fold_decr (x - 1) 0 ~init:(true, 0) ~f:(fun acc n -> aux acc n y) in                      
  let (_ , b) = range_fold_incr (x + 1) (x_len - 1) ~init:(true, 0) ~f:(fun acc n -> aux acc n y) in
  let (_ , c) = range_fold_decr (y - 1) 0 ~init:(true, 0) ~f:(fun acc n -> aux acc x n) in                      
  let (_ , d) = range_fold_incr (y + 1) (y_len - 1) ~init:(true, 0) ~f:(fun acc n -> aux acc x n) in
  a * b * c * d

let input =
  In_channel.read_lines "input"

let map =
  let map = Array.create ~len:(List.length input) (Array.create ~len:0 0)  in
  let aux y s =
    let s_len = String.length s in
    let xs = Array.create ~len:s_len 0 in
    let () = String.iteri s ~f:(fun x h -> Array.set xs x (Char.to_int h - 48)) in
    Array.set map y xs in
  let () = List.iteri input ~f:aux in
  map

let one =
  range_fold_incr 0 (map_height map - 1) ~init:0 ~f:(fun acc i ->
      range_fold_incr 0 (map_width map - 1) ~init:acc ~f:(fun acc j ->
          acc + if is_visible map i j then 1 else 0))

let two =
  range_fold_incr 0 (map_height map - 1) ~init:0 ~f:(fun acc i ->
      range_fold_incr 0 (map_width map - 1) ~init:acc ~f:(fun acc j ->
          let score = scenic_score map i j in
          if score > acc then
            score
          else
            acc))
    
  
let () = printf "one: %d, two: %d" one two 
