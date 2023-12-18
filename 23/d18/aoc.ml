open Base
open Stdio
open List
open Fn
open Cartesian_product

type direction =
  | R of int
  | L of int
  | U of int
  | D of int

let process_directions data =
  
  let process_triplet =
    function
    | D _, (L x, U _) -> L (x + 1)  
    | U _, (R x, D _) -> R (x + 1)
    | L _, (U x, R _) -> U (x + 1)
    | R _, (D x, L _) -> D (x + 1)

    | D _, (R x, D _) -> R x  
    | U _, (L x, U _) -> L x
    | D _, (L x, D _) -> L x
    | U _, (R x, U _) -> R x
    | L _, (D x, L _) -> D x  
    | R _, (U x, R _) -> U x
    | R _, (D x, R _) -> D x
    | L _, (U x, L _) -> U x

    | D _, (R x, U _) -> R (x - 1)   
    | L _, (D x, R _) -> D (x - 1)
    | U _, (L x, D _) -> L (x - 1)
    | R _, (U x, L _) -> U (x - 1) in
  
  let l1 = last_exn data :: drop_last_exn data in
  let l2 = tl_exn data @ [hd_exn data] in
  map (zip_exn l1 (zip_exn data l2)) ~f:process_triplet

let lagoon_capacity directions =
  
  let trench =
    let compute_trench (((x, y) :: _) as dir) = function
      | U n -> (x, y - n) :: dir
      | D n -> (x, y + n) :: dir
      | R n -> (x + n, y) :: dir
      | L n -> (x - n, y) :: dir in
    fold directions ~init:([(0, 0)]) ~f:compute_trench in
  
  let vertical_intervals =
    zip_exn (drop_last_exn trench) (tl_exn trench)
    |> filter ~f:(fun ((x1, _), (x2, _)) -> x1 = x2) in
  
  let xs =
    map trench ~f:fst
    |> sort ~compare:Int.compare
    |> remove_consecutive_duplicates ~equal:Int.equal in
  
  let ys =
    map trench ~f:snd
    |> sort ~compare:Int.compare
    |> remove_consecutive_duplicates ~equal:Int.equal in

  let xs_intervals = zip_exn (drop_last_exn xs) (tl_exn xs) in
  let ys_intervals = zip_exn (drop_last_exn ys) (tl_exn ys) in
  
  let intersections_count (x, y) =
    
    let check_intersection ((x1, y1), (_, y2)) =
      Float.(>) x (Float.of_int x1)
      && Float.(<) (Float.of_int (min y1 y2)) y
      && Float.(<) y (Float.of_int (max y1 y2)) in
    
    filter vertical_intervals ~f:check_intersection
    |> length in
  
  let compute_area acc ((x1, x2), (y1, y2)) =
    let pt = (Float.of_int x1 +. 0.5, Float.of_int y1 +. 0.5) in
    if intersections_count pt % 2 = 1
    then acc + (x2 - x1) * (y2 - y1)
    else acc in
  
  both xs_intervals ys_intervals
  |> fold ~init:0 ~f:compute_area

let part1 input =
  
  let directions =
    map input ~f:(fun s ->
        let [dir; n; _; _; _] = String.split_on_chars s ~on:[' '; '('; ')'] in
        let n = Int.of_string n in
        match dir with
        | "R" -> R n
        | "L" -> L n
        | "U" -> U n
        | "D" -> D n)
    |> process_directions in
  
  lagoon_capacity directions

let part2 input =
  
  let directions =
    map input ~f:(fun s ->
        let [_; _; _; col; _] = String.split_on_chars s ~on:[' '; '('; ')'] in
        let n = Int.of_string (String.concat ["0x"; (String.drop_suffix (String.drop_prefix col 1) 1)]) in
        match String.drop_prefix col 6 with
        | "0" -> R n
        | "1" -> D n
        | "2" -> L n
        | "3" -> U n)
    |> process_directions in

  lagoon_capacity directions

let _ =
  
  let input =
    In_channel.read_lines "input" in
  
  printf "Part 1: %d\n" (part1 input);
  printf "Part 2: %d\n" (part2 input);
