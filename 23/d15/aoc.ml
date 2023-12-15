open Base
open Stdio
open List
open Fn
 
type instruction =
   | Remove of string
   | Add of string * int

type lens = Lens of string * int

let sum l =
  match reduce l ~f:( + ) with
  | Some v -> v
  | None -> 0

let hash s =
  String.to_list s
  |> fold ~init:0 ~f:(fun acc c ->
         ((Char.to_int c + acc) * 17) % 256)

let process_instructions ins =
  let process_instruction boxes =
    function
    | Remove lbl ->
       Map.change boxes (hash lbl) ~f:(
           function
           | None -> None
           | Some box ->
              let new_box =
                filter box ~f:(fun (Lens (lbl2, _)) ->
                    not (String.equal lbl lbl2))
              in Some new_box)
    | Add (lbl, focal) ->
       Map.change boxes (hash lbl) ~f:(
           function
           | None -> Some [Lens (lbl, focal)]
           | Some box ->
              let new_box = 
                if exists box ~f:(fun (Lens (lbl2, _)) -> String.equal lbl2 lbl)
                then map box ~f:(fun (Lens (lbl2, _) as lens) ->
                         if String.equal lbl2 lbl
                         then Lens (lbl, focal)
                         else lens)
                else box @ [Lens (lbl, focal)] in
              Some new_box) in
  fold ins ~init:(Map.empty (module Int)) ~f:process_instruction

let power boxes =
  let box_power hash box =
    foldi box
      ~init:0
      ~f:(fun i acc (Lens (_, focal)) ->
        (hash + 1) * (i + 1) * focal + acc) in 
  Map.fold boxes
    ~init:0
    ~f:(fun ~key:hash ~data:box acc ->
      acc + box_power hash box)

let _ =
  
  let input =
    In_channel.read_all "input"
    |> String.filter ~f:(compose not (Char.equal '\n'))
    |> String.split ~on:',' in

  let parsed_input =
    let process_entry s =
      match String.split s ~on:'=' with
      | [label; focal] -> Add (label, (Int.of_string focal))
      | _ -> Remove (hd_exn (String.split s ~on:'-')) in
    input
    |> map ~f:process_entry in

  let part1 =
    input
    |> map ~f:hash
    |> sum in

  let part2 =
    parsed_input
    |> process_instructions
    |> power in
  
  printf "Part 1: %d\n" part1;
  printf "Part 2: %d\n" part2
