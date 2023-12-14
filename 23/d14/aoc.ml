open Base
open Stdio
open List
open Domainslib

let sum l =
  match reduce l ~f:( + ) with
  | Some v -> v
  | None -> 0

let height pattern = Array.length pattern
let width pattern = Array.length pattern.(0)

type direction =
  | North
  | West
  | South
  | East

let tilt pool platform direction =
  let height = height platform in
  let width = width platform in
  let new_platform =
    Array.map ~f:(Array.map ~f:(fun c -> if Char.equal c 'O' then '.' else c)) platform in

  let headroom (x, y) direction =
    match direction with
    | North ->
       let rec loop y =
         if y < 0 || Char.equal platform.(y).(x) '#'
         then 0
         else if Char.equal platform.(y).(x) '.'
         then 1 + loop (y - 1)
         else loop (y - 1)
       in loop y
    | West ->
       let rec loop x =
         if x < 0 || Char.equal platform.(y).(x) '#'
         then 0
         else if Char.equal platform.(y).(x) '.'
         then 1 + loop (x - 1)
         else loop (x - 1)
       in loop x
    | South ->
       let rec loop y =
         if y >= height || Char.equal platform.(y).(x) '#'
         then 0
         else if Char.equal platform.(y).(x) '.'
         then 1 + loop (y + 1)
         else loop (y + 1)
       in loop y
    | East ->
       let rec loop x =
         if x >= width || Char.equal platform.(y).(x) '#'
         then 0
         else if Char.equal platform.(y).(x) '.'
         then 1 + loop (x + 1)
         else loop (x + 1)
       in loop x in

  let _ =
    Task.parallel_for pool ~start:0 ~finish:(width*height - 1) ~body:(fun i ->     
        let x = i % width in
        let y = i / width in
        
        if Char.equal platform.(y).(x) 'O'
        then
          let headroom = headroom (x, y) direction in
          begin
            match direction with
            | North -> new_platform.(y - headroom).(x) <- 'O' 
            | West -> new_platform.(y).(x - headroom) <- 'O'
            | South -> new_platform.(y + headroom).(x) <- 'O'
            | East -> new_platform.(y).(x + headroom) <- 'O'             
          end)
  in new_platform

let array_to_list platform =
  Array.to_list (Array.map platform ~f:Array.to_list)

let total_load (platform: char list list) =
  let sum_aux i row =
    sum (map row ~f:(fun c -> if Char.equal c 'O' then 1 else 0))
    * (length platform - i)  in
    
  mapi platform ~f:sum_aux
  |> sum 
      
let part1 pool platform =
  tilt pool platform North
  |> array_to_list
  |> total_load 
    
let part2 pool platform n =

  let module Mem = struct
      module T = struct
        type t = char list list
        [@@deriving sexp_of, compare, hash]
      end
      include T
      include Comparator.Make(T)
    end in

  let mem = Hashtbl.create (module Mem) in

  let cycle platform =
    let platform = tilt pool platform North in
    let platform = tilt pool platform West in
    let platform = tilt pool platform South in
    let platform = tilt pool platform East in
    platform in

  let rec loop platform i =
    if i >= n
    then total_load (array_to_list platform)
    else
      let platform = cycle platform in
      let platform_list = array_to_list platform in
      match Hashtbl.find mem platform_list with
      | None ->
         Hashtbl.add_exn mem ~key:platform_list ~data:i;
         loop platform (i + 1)
      | Some j ->
         let f ~key:v ~data:k acc =
           if k = j + (n - j) % (i - j) - 1 then Some v else acc in
         Hashtbl.fold mem ~init:None ~f
         |> Option.value_exn
         |> total_load in
  
  loop platform 0

let _ =
  let input =
    In_channel.read_lines "input"
    |> map ~f:String.to_list in
  
  let platform = 
    Array.of_list (map input ~f:Array.of_list) in

  let pool = Task.setup_pool ~num_domains:11 () in
  let part1 = Task.run pool (fun () -> part1 pool platform) in
  let part2 = Task.run pool (fun () -> part2 pool platform 1000000000) in

  Task.teardown_pool pool;
  
  printf "Part 1: %d\n" part1;
  printf "Part 2: %d\n" part2

