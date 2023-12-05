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

(* Completes a map with the trivial mappings *)
let complete_map maps =
  let f (i, l) (src, tgt, len) =
    if src = i then
      (src + len, (src, tgt, len) :: l)
    else
      (src + len, (src, tgt, len) :: (i, i, src - i) :: l) in
  List.fold maps ~init:(0, []) ~f
  |> snd
  |> List.rev

let rec compose_range l (src, tgt, len) =
  let f ((src, tgt, len), l) (src2, tgt2, len2) =
    if len = 0 then
      Stop l
    else if tgt >= src2 + len2 then
      Continue ((src, tgt, len), l)
    else
      let new_len = min (tgt + len) (src2 + len2) - tgt in
      Continue (((src + new_len, tgt + new_len, len - new_len), (src, tgt2 + (tgt - src2), new_len) :: l)) in
  List.fold_until l ~init:((src, tgt, len), []) ~f ~finish:(fun ((src, tgt, len), l) -> (src, tgt, len) :: l)
  |> List.rev

let compose l1 l2 =
  List.map l1 ~f:(compose_range l2)
  |> List.concat

let eval map x =
  let f _ (src, tgt, len) =
    if x < src || x > src + len then
      Continue x
    else
      Stop (tgt + x - src) in
  List.fold_until map ~init:x ~f ~finish:id

let process_maps maps =
  List.map maps ~f:(fun (_, _, maps) -> maps)
  |> List.map ~f:(List.map ~f:(fun (tgt, src, len) -> (src, tgt, len)))
  |> List.map ~f:(List.sort ~compare:(fun (src1, _, _) (src2, _, _) -> Int.compare src1 src2))
  |> List.map ~f:complete_map
  |> List.reduce ~f:compose
  |> Option.value_exn

let part1 input =
  let Ok ((seeds, maps)) = parse_string ~consume:Consume.All parse input in
  let map = process_maps maps in
  List.map seeds ~f:(eval map)
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

(* Returns the part of a mapping relevant to a certain range of seeds *)
let intersection map (x, len) =
  let f ((x, len), l) (src, tgt, len2) =
    if len = 0 then
      Stop l
    else if x + len <= src || src + len2 < x then
      Continue ((x, len), l)
    else
      let new_len = min (x + len) (src + len2) - x in
      Continue (((x + new_len, len - new_len), (x, tgt + (x - src), new_len) :: l)) in
  List.fold_until map ~init:((x, len), []) ~f ~finish:snd
  |> List.rev

let part2 input =
  let Ok ((seeds, maps)) = parse_string ~consume:Consume.All parse2 input in
  let seeds = List.sort seeds ~compare:(fun (s1, _) (s2, _) -> Int.compare s1 s2) in
  let map = process_maps maps in
  List.map seeds ~f:(intersection map)
  |> List.concat
  |> List.map ~f:(fun (_, tgt, _) -> tgt)
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn
  
let _ =
  let input = In_channel.read_all "input" in
  begin
    printf "Part 1: %d\n" (part1 input);
    printf "Part 2: %d\n" (part2 input);
  end
