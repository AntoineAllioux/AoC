open Base
open Stdio
open List
open Fn
open Continue_or_stop
open Model
open Parser

let sum l = reduce_exn l ~f:( + )

let part1 workflows ratings =
  let is_accepted rating =
    let check_rating rating condition =
      fold rating ~init:false ~f:(fun acc (Rat (cat, x)) ->
          match condition with
          | Lt (cat2, x2) when compare_cat cat cat2 = 0 && x < x2 -> true
          | Gt (cat2, x2) when compare_cat cat cat2 = 0 && x > x2 -> true
          | _ -> acc)
    in

    let rec loop rating workflow_id =
      let (Workflow (conds, final)) = Map.find_exn workflows workflow_id in
      let decision =
        fold_until conds ~init:final
          ~f:(fun acc (condition, final) ->
            if check_rating rating condition then Stop final else Continue acc)
          ~finish:id
      in

      match decision with Goto w -> loop rating w | Acc -> true | Rej -> false
    in

    loop rating "in"
  in

  filter ratings ~f:is_accepted
  |> map ~f:(compose sum (map ~f:(fun (Rat (_, x)) -> x)))
  |> sum

let part2 workflows =
  let combinations ranges =
    Map.fold ranges ~init:1 ~f:(fun ~key:_ ~data:(min, max) acc ->
        (max - min + 1) * acc)
  in

  let ranges_init =
    Map.of_alist_exn
      (module Cat)
      [ (X, (1, 4000)); (M, (1, 4000)); (A, (1, 4000)); (S, (1, 4000)) ]
  in

  let update_ranges ranges cond neg =
    match cond with
    | Lt (cat, x) ->
        if neg then
          Map.update ranges cat ~f:(fun (Some (cat_min, cat_max)) ->
              (max cat_min x, cat_max))
        else
          Map.update ranges cat ~f:(fun (Some (cat_min, cat_max)) ->
              (cat_min, min (x - 1) cat_max))
    | Gt (cat, x) ->
        if neg then
          Map.update ranges cat ~f:(fun (Some (cat_min, cat_max)) ->
              (cat_min, min x cat_max))
        else
          Map.update ranges cat ~f:(fun (Some (cat_min, cat_max)) ->
              (max cat_min (x + 1), cat_max))
  in

  let rec loop workflow_id ranges =
    let (Workflow (conds, final_decision)) =
      Map.find_exn workflows workflow_id
    in
    let new_ranges, acc =
      fold conds ~init:(ranges, 0) ~f:(fun (ranges, acc) (cond, decision) ->
          match decision with
          | Acc ->
              (update_ranges ranges cond true,
               acc + combinations (update_ranges ranges cond false))
          | Rej -> (update_ranges ranges cond true, acc)
          | Goto w ->
              (update_ranges ranges cond true,
               loop w (update_ranges ranges cond false) + acc))
    in

    match final_decision with
    | Acc -> combinations new_ranges + acc
    | Rej -> acc
    | Goto w -> loop w new_ranges + acc
  in
  loop "in" ranges_init

let _ =
  let input = In_channel.read_all "input" in

  let (Ok (workflows_list, ratings)) =
    Angstrom.parse_string ~consume:Angstrom.Consume.All input_p input
  in

  let workflows =
    fold workflows_list
      ~init:(Map.empty (module String))
      ~f:(fun mem (id, rules) -> Map.add_exn mem ~key:id ~data:rules)
  in

  printf "Part 1: %d\n" (part1 workflows ratings);
  printf "Part 2: %d\n" (part2 workflows)
