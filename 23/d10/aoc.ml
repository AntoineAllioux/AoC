open Base
open Stdio
open List
open Fn
open Cartesian_product

module Pos = struct
  module T = struct
    type t = int * int
    [@@deriving sexp_of, compare, equal]
  end
  include T
  include Comparator.Make(T)
end

let height maze = Array.length maze

let width maze = Array.length maze.(0)

let check_boundaries maze (x, y) =
  x >= 0 && x < width maze
  && y >= 0 && y < height maze

let neighbours maze (x, y) =
  let tile = maze.(y).(x) in
  let tile_neighbours =
    match tile with
    | '|' -> [(x, y - 1); (x, y + 1)]
    | '-' -> [(x - 1, y); (x + 1, y)]
    | 'L' -> [(x, y - 1); (x + 1, y)]
    | 'J' -> [(x - 1, y); (x, y - 1)]
    | '7' -> [(x - 1, y); (x, y + 1)]
    | 'F' -> [(x, y + 1); (x + 1, y)]
    | _ -> [] in
  filter tile_neighbours ~f:(check_boundaries maze) 

let all_neighbours maze (x, y) =
  [(x - 1, y); (x, y - 1); (x + 1, y); (x, y + 1)]
  |> filter ~f:(check_boundaries maze)

let determine_s maze =
  let s_x, s_y =
    Array.find_mapi_exn maze
      ~f:(fun y row ->
        Option.map
          (Array.findi row ~f:(fun _ -> Char.equal 'S'))
          ~f:(fun (x, _) -> (x, y))) in
  
  let s_neighbours =
    [ ((s_x - 1, s_y), `Left); ((s_x, s_y - 1), `Above);
      ((s_x + 1, s_y), `Right); ((s_x, s_y + 1), `Below) ]
    |> filter ~f:(fun ((x, y), _) ->
           x >= 0 && x < width maze && y >= 0 && y < height maze
           && mem (neighbours maze (x, y)) (s_x, s_y) ~equal:Pos.equal)
    |> map ~f:snd in
  
  let s =
    match s_neighbours with
    | [`Left; `Above] -> 'J'
    | [`Left; `Right] -> '-'
    | [`Left; `Below] -> '7'
    | [`Above; `Right] -> 'L'
    | [`Above; `Below] -> '|'
    | [`Right; `Below] -> 'F' in

  (s, (s_x, s_y))

let bfs maze neighbours src =
  let queue = Queue.create () in
  let _ = Queue.enqueue queue src in
  let rec loop visited =
    let _ =
      Queue.filter_inplace queue
        ~f:(compose not (Set.mem visited)) in
    if Queue.is_empty queue
    then visited
    else
      let (x, y) = Queue.dequeue_exn queue in
      let neighbours = neighbours maze (x, y) in
      let _ = Queue.enqueue_all queue neighbours in
      let visited = Set.add visited (x, y) in
      loop visited in
  loop (Set.empty (module Pos))
 
(* First version of Part 2: we cast rays from the left border to the tiles which are not part of the loop
 * and we count the parity of the number of crossings to determine whether they are inside the loop *)
let part2_raycasting maze loop =
  let is_inside (x, y) =
    let rec count_crossings = function
      | [] -> 0
      | 'L' :: 'J' :: tiles | 'F' :: '7' :: tiles -> count_crossings tiles
      | 'L' :: '7' :: tiles | 'F' :: 'J' :: tiles -> 1 + count_crossings tiles
      | _ :: tiles  -> 1 + count_crossings tiles in
    let crossings =
      take (Array.to_list maze.(y)) (x + 1)
      |> filteri ~f:(fun x _ -> Set.mem loop (x, y))
      |> filter ~f:(compose not (Char.equal '-'))
      |> count_crossings in
    crossings % 2 = 1 in
  
  both (range 0 (width maze)) (range 0 (height maze))
  |> filter ~f:(compose not (Set.mem loop))
  |> map ~f:is_inside
  |> filter ~f:(Bool.equal true)
  |> length

(* We define an in-place version of bfs for the second version of part 2
 * which is more expensive *)
let bfs_inplace maze neighbours src =
  let queue = Queue.create () in
  let _ = Queue.enqueue queue src in
  let rec loop () =
    let _ =
      Queue.filter_inplace queue
        ~f:(fun (x, y) ->
          not (Char.equal maze.(y).(x) 'X')) in
    if Queue.is_empty queue
    then ()
    else
      let (x, y) = Queue.dequeue_exn queue in
      let neighbours = neighbours maze (x, y) in
      let _ = Queue.enqueue_all queue neighbours in
      let _ = maze.(y).(x) <- 'X' in
      loop () in
  loop ()

(* Second version of Part 2: we fill the exterior and count the remaining tiles *)
let part2_filling input =
  let tmp = Array.of_list (map input ~f:Array.of_list) in
  let (s, (s_x, s_y)) = determine_s tmp in
  let f_h c =
    let c = if Char.equal c 'S' then s else c in
    if Char.equal c '-'
       || Char.equal c 'F'
       || Char.equal c 'L'
    then [c; '-']
    else [c; '#'] in
  
  let f_v c =
    if Char.equal c '|'
       || Char.equal c 'F'
       || Char.equal c '7'
    then '|'
    else '#' in
  
  let maze_interior =
    map input
      ~f:(fun row ->
        let new_row = drop_last_exn (concat (map row ~f:f_h)) in
        let filler = map new_row ~f:f_v in
        [('#' :: new_row @ ['#']) ; ('#' :: filler @ ['#'])])
    |> concat
    |> drop_last_exn in
  
  let maze_width = 2 * width tmp + 1 in
  let maze =
    Array.of_list
      (map
         (init maze_width ~f:(const '#') :: maze_interior @ [init maze_width ~f:(const '#')])
         ~f:Array.of_list) in
  
  let check_visited (x, y) =
    let (x, y) = 2 * x + 1, 2 * y + 1 in
    Char.equal maze.(y).(x) 'X' in
  
  let _ = bfs_inplace maze neighbours (s_x, s_y) in
  let _ = bfs_inplace maze all_neighbours (0, 0) in
  both (range 0 (width tmp)) (range 0 (height tmp))
  |> filter ~f:(compose not check_visited)
  |> length

let _ =
  let input =
    In_channel.read_lines "input"
    |> map ~f:String.to_list in
  
  let maze = Array.of_list (map input ~f:Array.of_list) in
  let (s, (s_x, s_y)) = determine_s maze in
  let _ = maze.(s_y).(s_x) <- s in
  let loop = bfs maze neighbours (s_x, s_y) in 
  let part1 = Set.length loop / 2 in

  begin
    printf "Part 1: %d\n" part1;
    printf "Part 2 (filling): %d\n" (part2_filling input);
    printf "Part 2 (raycasting): %d\n" (part2_raycasting maze loop);
  end
