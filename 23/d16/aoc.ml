open Base
open Stdio
open List

let height grid = Array.length grid
let width grid = Array.length grid.(0)

let check_boundaries grid (x, y) =
  x >= 0 && x < width grid && y >= 0 && y < height grid

let filter_valid grid pos =
  filter pos ~f:(fun ((x, y), _) -> check_boundaries grid (x, y))

type direction = Right | Left | Above | Below
[@@deriving sexp_of, compare, equal]

module Pos = struct
  module T = struct
    type t = int * int [@@deriving sexp_of, compare, equal]
  end

  include T
  include Comparator.Make (T)
end

module Mem = struct
  module T = struct
    type t = Pos.t * direction [@@deriving sexp_of, compare, equal]
  end

  include T
  include Comparator.Make (T)
end

let next_tiles grid (x, y) direction =
  let positions =
    match grid.(y).(x) with
    | '.' -> (
        match direction with
        | Right -> [ ((x - 1, y), Right) ]
        | Left -> [ ((x + 1, y), Left) ]
        | Above -> [ ((x, y + 1), Above) ]
        | Below -> [ ((x, y - 1), Below) ])
    | '/' -> (
        match direction with
        | Right -> [ ((x, y + 1), Above) ]
        | Left -> [ ((x, y - 1), Below) ]
        | Above -> [ ((x - 1, y), Right) ]
        | Below -> [ ((x + 1, y), Left) ])
    | '\\' -> (
        match direction with
        | Right -> [ ((x, y - 1), Below) ]
        | Left -> [ ((x, y + 1), Above) ]
        | Above -> [ ((x + 1, y), Left) ]
        | Below -> [ ((x - 1, y), Right) ])
    | '-' -> (
        match direction with
        | Right -> [ ((x - 1, y), Right) ]
        | Left -> [ ((x + 1, y), Left) ]
        | Above | Below -> [ ((x - 1, y), Right); ((x + 1, y), Left) ])
    | '|' -> (
        match direction with
        | Right | Left -> [ ((x, y - 1), Below); ((x, y + 1), Above) ]
        | Above -> [ ((x, y + 1), Above) ]
        | Below -> [ ((x, y - 1), Below) ])
  in
  filter_valid grid positions

let energized_tiles grid start direction =
  let rec loop mem stack acc =
    match stack with
    | [] -> acc
    | ((x, y), direction) :: stack -> (
        match Map.find mem ((x, y), direction) with
        | Some _ -> loop mem stack acc
        | None ->
            let tiles = next_tiles grid (x, y) direction in
            let acc = Set.add acc (x, y) in
            let mem' = Map.add_exn mem ~key:((x, y), direction) ~data:true in
            loop mem' (tiles @ stack) acc)
  in
  Set.length (loop (Map.empty (module Mem)) [ (start, direction) ] (Set.empty (module Pos)))

let part2 grid =
  let height = height grid in
  let width = width grid in
  let left =
    map (range 0 height) ~f:(fun y -> energized_tiles grid (0, y) Left)
    |> reduce_exn ~f:max
  in
  let right =
    map (range 0 height) ~f:(fun y -> energized_tiles grid (width - 1, y) Right)
    |> reduce_exn ~f:max
  in
  let top =
    map (range 0 width) ~f:(fun x -> energized_tiles grid (x, 0) Above)
    |> reduce_exn ~f:max
  in
  let bottom =
    map (range 0 width) ~f:(fun x -> energized_tiles grid (x, height - 1) Below)
    |> reduce_exn ~f:max
  in

  max left (max right (max top bottom))

let _ =
  let input = In_channel.read_lines "input" |> map ~f:String.to_list in

  let grid = Array.of_list (map input ~f:Array.of_list) in

  let part1 = energized_tiles grid (0, 0) Left in

  printf "Part 1: %d\n" part1;
  printf "Part 2: %d\n" (part2 grid)
