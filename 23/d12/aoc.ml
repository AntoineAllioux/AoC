open Base
open Stdio
open List
open Fn

type spring =
  | Operational
  | Damaged
  | Unknown
[@@deriving sexp_of, compare, equal]

let sum = reduce_exn ~f:( + )

let curry f (x, y) = f x y

let arrangements springs damaged_springs =
  
  let module Mem = struct
      module T = struct
        type t = spring list * int list * bool
        [@@deriving sexp_of, compare, equal]
      end
      include T
      include Comparator.Make(T)
    end in
  
  let mem = ref (Map.empty (module Mem)) in
  
  let rec loop springs damaged_springs active =
    match Map.find (! mem) (springs, damaged_springs, active) with
    | Some v -> v
    | _ ->
       let res = 
         match (springs, damaged_springs) with
         | ([], []) -> 1
         | ([], 0 :: l)  -> loop [] l false
         | ([], _)  -> 0
         | (Operational :: s, []) -> loop s [] false
         | (Operational :: s, n :: ns) when n = 0 ->
            loop s ns false
         | (Operational :: s, n :: ns) when n > 0 ->
            if active
            then 0
            else loop s (n :: ns) false
         | (Damaged :: _, []) -> 0
         | (Damaged :: s, n :: ns) when n > 0 ->
            loop s (n - 1 :: ns) true
         | (Damaged :: s, n :: ns) when n = 0 ->
            if active
            then 0
            else loop (Damaged :: s) ns false
         | (Unknown :: s, []) -> loop s [] false
         | (Unknown :: s, n :: ns) when n > 0 ->
            if active
            then loop s (n - 1 :: ns) true
            else loop s (n - 1 :: ns) true 
                 + loop s (n :: ns) false
         | (Unknown :: s, n :: ns) when n = 0 ->
            if active
            then loop s ns false
            else loop (Unknown :: s) ns false in
       
       mem := Map.add_exn (! mem)
                ~key:(springs, damaged_springs, active)
                ~data:res;
       res

  in loop springs damaged_springs false

let _ =
  let parse_entry n s =
    let [s1; s2] = String.split s ~on:' ' in

    let parse_symbol c =
      match c with
      | '?'  -> Unknown
      | '#' -> Damaged
      | '.' -> Operational in

    let springs =
      map (String.to_list s1) ~f:parse_symbol in

    let unrolled_springs =
      init n ~f:(const springs)
      |> intersperse ~sep:[Unknown]
      |> concat in

    let damaged_springs =
      String.split s2 ~on:','
      |> map ~f:Int.of_string in

    let unrolled_damaged_springs =
      concat (init n ~f:(const damaged_springs)) in
    
    (unrolled_springs, unrolled_damaged_springs) in

  let input = In_channel.read_lines "input" in
  
  let part1 =
    input
    |> map ~f:(parse_entry 1)
    |> map ~f:(curry arrangements)
    |> sum in

  let part2 =
    input
    |> map ~f:(parse_entry 5)
    |> map ~f:(curry arrangements)
    |> sum in
  
  printf "Part 1: %d\n" part1;
  printf "Part 2: %d\n" part2
