open Base
open Stdio
open List
open Continue_or_stop
open Cartesian_product

module Pos = struct
  module T = struct
    type t = int * int [@@deriving sexp_of, compare, equal, hash]
  end

  include T
  include Comparator.Make (T)
end

let height graph = Array.length graph
let width graph = Array.length graph.(0)

let check_boundaries graph (x, y) =
  x >= 0 && x < width graph && y >= 0 && y < height graph

let neighbours graph (x, y) =
  [ (x - 1, y); (x, y - 1); (x + 1, y); (x, y + 1) ]
  |> filter ~f:(check_boundaries graph)
  |> filter ~f:(fun (x, y) -> not (Char.equal graph.(y).(x) '#'))

let part1 graph src n =
  let positions = Set.singleton (module Pos) src in

  let rec loop n positions =
    if n = 0 then Set.length positions
    else
      let positions =
        Set.fold positions
          ~init:(Set.empty (module Pos))
          ~f:(fun acc p ->
            Set.union acc (Set.of_list (module Pos) (neighbours graph p)))
      in
      loop (n - 1) positions
  in
  loop n positions

let part2 graph src n =
  let n = (n - 65) / 131 in

  let diamond0 = part1 graph src 64 in
  let diamond1 = part1 graph src 65 in
  let corners0 = part1 graph src 130 - diamond0 in
  let corners1 = part1 graph src 131 - diamond1 in

  let rec loop p n acc =
    let diamond, corners0, corners1 =
      if p then (diamond1, corners1, corners0)
      else (diamond0, corners0, corners1)
    in
    if n = 0 then diamond + acc
    else
      let acc = (2 * n * ((2 * diamond) + corners0 + corners1)) + acc in
      loop (not p) (n - 1) acc
  in
  loop true n 0

let _ =
  let input = In_channel.read_lines "input" |> map ~f:String.to_list in

  let graph = Array.of_list (map input ~f:Array.of_list) in

  let height = height graph in
  let width = width graph in

  let src =
    both (range 0 width) (range 0 height)
    |> fold_until ~init:None
         ~f:(fun acc (x, y) ->
           if Char.equal graph.(y).(x) 'S' then Stop (x, y) else Continue acc)
         ~finish:Option.value_exn
  in

  printf "Part1: %d\n" (part1 graph src 64);
  printf "Part2: %d\n" (part2 graph src 26501365)
