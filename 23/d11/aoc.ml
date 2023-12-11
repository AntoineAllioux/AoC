open Base
open Stdio
open List
open Fn
open Cartesian_product

let sum = reduce_exn ~f:( + ) 

let galaxies universe =
  let height = Array.length universe in
  let width = Array.length universe.(0) in
  both (range 0 width) (range 0 height)
  |> filter ~f:(fun (x, y) -> Char.equal universe.(y).(x) '#')

let shortest_paths universe galaxies expansion_factor =  
  let expanded_rows y1 y2 =
    let y1, y2 = if y1 < y2 then y1, y2 else y2, y1 in
    map (range y1 (y2 + 1)) ~f:(fun y -> if Char.equal universe.(y).(0) '*' then 1 else 0)
    |> sum in
  
  let expanded_cols x1 x2 =
    let x1, x2 = if x1 < x2 then x1, x2 else x2, x1 in
    map (range x1 (x2 + 1)) ~f:(fun x -> if Char.equal universe.(0).(x) '*' then 1 else 0)
    |> sum in

  let distance (x1, y1) (x2, y2) =
    let expanded_cols = (expansion_factor - 1) * (expanded_cols x1 x2) in
    let expanded_rows = (expansion_factor - 1) * (expanded_rows y1 y2) in
    abs (x1 - x2) + abs (y1 - y2) + expanded_cols + expanded_rows in
  
  map (range 0 (length galaxies)) ~f:(fun i ->
      map (range (i + 1) (length galaxies)) ~f:(fun j ->
          let g1 = nth_exn galaxies i in
          let g2 = nth_exn galaxies j in
          distance g1 g2))
  |> concat
  |> sum

let _ =
  let input =
    In_channel.read_lines "input"
    |> map ~f:String.to_list in

  let tmp_width = length (hd_exn input) in
  
  let is_col_empty i =
    map input ~f:(fun row -> nth_exn row i)
    |> for_all ~f:(Char.equal '.') in
  
  let universe_tmp =
    let empty_cols = filter (range 0 tmp_width) ~f:is_col_empty in
    
    let f row =
      if for_all row ~f:(Char.equal '.')
      then init (tmp_width) ~f:(const '*')
      else 
        mapi row ~f:(fun i c ->
            if mem empty_cols i ~equal:Int.equal
            then '*'
            else c) in
    
    map input ~f in
  
  let universe = Array.of_list (map universe_tmp ~f:Array.of_list) in
  let galaxies = galaxies universe in
  
  begin
    printf "Part 1: %d\n" (shortest_paths universe galaxies 2);
    printf "Part 2: %d\n" (shortest_paths universe galaxies 1000000)
  end
