open Angstrom
open Stdio
open Base
open Container.Continue_or_stop
open Fn

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

let alpha =
  take_while1 (function 'a' .. 'z' -> true | _ -> false)

let space = char ' '

let parse_header =
  let open Angstrom.Let_syntax in
  let* _ = string "seeds: " in
  let* seeds = sep_by1 space integer in
  return seeds

let parse_map_header =
  let open Angstrom.Let_syntax in
  let* src = alpha in
  let* _ = take 4 in
  let* dest = alpha in
  let* _ = take 5 in 
  return (src, dest)

let triple =
  let open Angstrom.Let_syntax in
  let* x = integer in
  let* _ = space in
  let* y = integer in
  let* _ = space in
  let* z = integer in
  return (x, y, z)

let parse_map =
  let open Angstrom.Let_syntax in
  let* (src, dest) = parse_map_header in
  let* _ = end_of_line in
  let* l = sep_by end_of_line triple in
  let* _ = end_of_line in
  return (src, dest, l)

let parse =
  let open Angstrom.Let_syntax in
  let* seeds = parse_header in
  let* _ = end_of_line in
  let* _ = end_of_line in
  let* maps = sep_by end_of_line parse_map in
  return (seeds, maps)

let part1 input =
  let Ok ((seeds, maps)) = parse_string ~consume:Consume.All parse input in
  let get_loc v (_, _, maps) =
    let forward v (dest, src, range) =
      if v >= src && v <= src + range then
        Stop (dest + (v - src))
      else
        Continue v in
    List.fold_until maps ~init:v ~f:forward ~finish:id in 
  let get_locs seed = List.fold maps ~init:seed ~f:get_loc in
  List.map seeds ~f:get_locs
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

(* Code for the part 2 *)

let parse_header2 =
  let open Angstrom.Let_syntax in
  let* _ = string "seeds: " in
  let* seeds = sep_by1 space (both integer (space *> integer)) in
  return seeds

let parse2 =
  let open Angstrom.Let_syntax in
  let* seeds = parse_header2 in
  let* _ = end_of_line in
  let* _ = end_of_line in
  let* maps = sep_by end_of_line parse_map in
  return (seeds, maps)

let part2 input =
  let Ok ((seeds, maps)) = parse_string ~consume:Consume.All parse2 input in
  let rev_map = List.rev maps in
  let check_loc loc =
    let get_seed v (_, _, maps) =
      let backward v (dest, src, range) =
        if v >= dest && v < dest + range then
          Stop (src + (v - dest))
        else
          Continue v in
      List.fold_until maps ~init:v ~f:backward ~finish:id in
    let potential_seed = List.fold rev_map ~init:loc ~f:get_seed in
    let check_seed _ (start, range) =
      if potential_seed >= start && potential_seed < start + range then
        Stop (Some loc)
      else
        Continue () in
    List.fold_until seeds ~init:() ~f:check_seed ~finish:(const None) in
  let rec loop i = 
    match check_loc i with
    | None -> loop (i + 1)
    | Some loc -> loc      
  in loop 0

let _ =
  let input = In_channel.read_all "input" in
  begin
    printf "Part 1: %d\n" (part1 input);
    printf "Part 2: %d\n" (part2 input)
  end
