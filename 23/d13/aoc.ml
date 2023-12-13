open Base
open Stdio
open List
open Fn
open Cartesian_product
open Container.Continue_or_stop

let sum l =
  match reduce l ~f:( + ) with
  | Some v -> v
  | None -> 0

let height pattern = Array.length pattern
let width pattern = Array.length pattern.(0)

let compute_reflexions pattern =
  let height = height pattern in
  let width = width pattern in
  
  let reflexions dim1 dim2 f =
    let check_axis x =
      let max_len = min x (dim1 - x) in
      zip_exn (range ~stride:(-1) (x - 1) (x - 1 - max_len)) (range x (x + max_len))
      |> for_all ~f:(fun (x1, x2) -> for_all (range 0 dim2) ~f:(fun y -> f y x1 x2)) in
    
    filter (range 1 dim1) ~f:check_axis in
  
  let refl_y =
    reflexions width height
      (fun y x1 x2 -> Char.equal pattern.(y).(x1) pattern.(y).(x2)) in
  let refl_x =
    reflexions height width
      (fun x y1 y2 -> Char.equal pattern.(y1).(x) pattern.(y2).(x)) in
  
  (refl_x, refl_y)

let repair_and_compute_reflexions pattern =
  let height = height pattern in
  let width = width pattern in
  let (orig_refl_x, orig_refl_y) = compute_reflexions pattern in

  let switch =
    function
    | '.' -> '#'
    | '#' -> '.' in
  
  let check_repair acc (x, y) =
    pattern.(y).(x) <- switch pattern.(y).(x);
    let (refl_x, refl_y) = compute_reflexions pattern in
    pattern.(y).(x) <- switch pattern.(y).(x);
    let refl_x =
      filter refl_x
        ~f:(compose not (mem orig_refl_x ~equal:Int.equal)) in
    let refl_y =
      filter refl_y
        ~f:(compose not (mem orig_refl_y ~equal:Int.equal)) in
    
    if not (is_empty refl_x) || not (is_empty refl_y)
    then Stop (refl_x, refl_y)
    else Continue acc in 

  both (range 0 width) (range 0 height)
  |> fold_until
       ~init:(orig_refl_x, orig_refl_y)
       ~f:check_repair
       ~finish:id
  
let _ =
  let input =
    In_channel.read_lines "input"
    |> map ~f:String.to_list
    |> group ~break:(const is_empty)
    |> map ~f:(filter ~f:(compose not is_empty)) in

  let valley = 
    map input ~f:(fun v -> Array.of_list (map v ~f:Array.of_list)) in
  
  let part1 =
    map valley ~f:compute_reflexions
    |> map ~f:(fun (refl_x, refl_y) -> sum refl_x * 100 + sum refl_y)
    |> sum in
  
  let part2 =
    map valley ~f:repair_and_compute_reflexions
    |> map ~f:(fun (refl_x, refl_y) -> sum refl_x * 100 + sum refl_y)
    |> sum in

  printf "Part 1: %d\n" part1;
  printf "Part 2: %d\n" part2
