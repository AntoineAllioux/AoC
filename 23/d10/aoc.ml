open Base
open Stdio
open List
open Fn
open Cartesian_product

module Pos = struct
  module T = struct
    type t = int * int
    [@@deriving compare, sexp_of, equal]
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

let prepare_maze input =
  let tmp = Array.of_list (map input ~f:Array.of_list) in
  
  let s_x, s_y =
    Array.find_mapi_exn tmp
      ~f:(fun y row ->
        Option.map
          (Array.findi row ~f:(fun _ -> Char.equal 'S'))
          ~f:(fun (x, _) -> (x, y))) in
  
  let s_neighbours =
    [ ((s_x - 1, s_y), `Left); ((s_x, s_y - 1), `Above);
      ((s_x + 1, s_y), `Right); ((s_x, s_y + 1), `Below) ]
    |> filter ~f:(fun ((x, y), _) ->
           x >= 0 && x < width tmp && y >= 0 && y < height tmp
           && mem (neighbours tmp (x, y)) (s_x, s_y) ~equal:Pos.equal)
    |> map ~f:snd in
  
  let s =
    match s_neighbours with
    | [`Left; `Above] -> 'J'
    | [`Left; `Right] -> '-'
    | [`Left; `Below] -> '7'
    | [`Above; `Right] -> 'L'
    | [`Above; `Below] -> '|'
    | [`Right; `Below] -> 'F' in
 
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
  
  let width = 2 * width tmp + 1 in

  let maze =
    Array.of_list
      (map
         (init width ~f:(const '#') :: maze_interior @ [init width ~f:(const '#')])
         ~f:Array.of_list) in
  
  (maze, (s_x, s_y))

let _ =
  let input =
    In_channel.read_lines "input"
    |> map ~f:String.to_list in
  
  let (maze, (s_x, s_y)) = prepare_maze input in

  let check_visited (x, y) =
    let (x, y) = 2 * x + 1, 2 * y + 1 in
    Char.equal maze.(y).(x) 'X' in
  
  let part1 =
    let _ = bfs_inplace maze neighbours (s_x, s_y) in
    let loop_length = 
      both (range 0 ((width maze - 1) / 2)) (range 0 ((height maze - 1) / 2))
      |> filter ~f:check_visited
      |> length in
    loop_length / 2 in
  
  let part2 =
    let _ = bfs_inplace maze all_neighbours (0, 0) in
    both (range 0 ((width maze - 1) / 2)) (range 0 ((height maze - 1) / 2))
    |> filter ~f:(compose not check_visited)
    |> length in
  
  begin
    printf "Part 1: %d\n" part1;
    printf "Part 2: %d\n" part2;
  end
