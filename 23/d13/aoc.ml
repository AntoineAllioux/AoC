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

let height valley = Array.length valley
let width valley = Array.length valley.(0)

let reflexions_generic dim1 dim2 f =
  let check_axis x =
    let max_len = min x (dim1 - x) in
    zip_exn (range ~stride:(-1) (x - 1) (x - 1 - max_len)) (range x (x + max_len))
    |> for_all ~f:(fun (x1, x2) -> for_all (range 0 dim2) ~f:(fun y -> f y x1 x2)) in
  
  filter (range 1 dim1) ~f:check_axis

let compute_reflexions valley =
  let height = height valley in
  let width = width valley in
  let refl_y =
    reflexions_generic width height
      (fun y x1 x2 -> Char.equal valley.(y).(x1) valley.(y).(x2)) in
  let refl_x =
    reflexions_generic height width
      (fun x y1 y2 -> Char.equal valley.(y1).(x) valley.(y2).(x)) in

  (refl_x, refl_y)

let repair_and_compute_reflexions valley =
  let height = height valley in
  let width = width valley in
  let (orig_refl_x, orig_refl_y) = compute_reflexions valley in
  
  let switch =
    function
    | '.' -> '#'
    | '#' -> '.' in

  both (range 0 width) (range 0 height)
  |> fold_until
       ~init:(orig_refl_x, orig_refl_y)
       ~f:(fun acc (x, y) ->
         valley.(y).(x) <- switch valley.(y).(x);
         let (refl_x, refl_y) = compute_reflexions valley in
         valley.(y).(x) <- switch valley.(y).(x);
         let refl_x =
           filter refl_x
             ~f:(compose not (mem orig_refl_x ~equal:Int.equal)) in
         let refl_y =
           filter refl_y
             ~f:(compose not (mem orig_refl_y ~equal:Int.equal)) in
         if not (is_empty refl_x) || not (is_empty refl_y)
         then Stop (refl_x, refl_y)
         else Continue acc)
       ~finish:id
  
let _ =
  let input =
    In_channel.read_lines "input"
    |> map ~f:String.to_list
    |> group ~break:(fun _ -> is_empty)
    |> map ~f:(filter ~f:(compose not is_empty))
    |> filter ~f:(compose not is_empty) in

  let valleys = 
    map input ~f:(fun v -> Array.of_list (map v ~f:Array.of_list)) in
  
  let part1 =
    map valleys ~f:compute_reflexions
    |> map ~f:(fun (xs, ys) -> sum xs * 100 + sum ys)
    |> sum in
  
  let part2 =
    map valleys ~f:repair_and_compute_reflexions
    |> map ~f:(fun (xs, ys) -> sum xs * 100 + sum ys)
    |> sum in

  printf "Part 1: %d\n" part1;
  printf "Part 2: %d\n" part2
