open Base
open Stdio
open List
open Fn
open Continue_or_stop
open Model
open Parser

type cond_ext = Cond of cond | Neg of cond

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
  let combinations conds =
    let ranges_init =
      Map.of_alist_exn
        (module Cat)
        [ (X, (1, 4000)); (M, (1, 4000)); (A, (1, 4000)); (S, (1, 4000)) ]
    in

    let ranges =
      fold conds ~init:ranges_init ~f:(fun ranges -> function
        | Cond (Lt (cat, x)) ->
            Map.update ranges cat ~f:(fun (Some (cat_min, cat_max)) ->
                (cat_min, min (x - 1) cat_max))
        | Cond (Gt (cat, x)) ->
            Map.update ranges cat ~f:(fun (Some (cat_min, cat_max)) ->
                (max cat_min (x + 1), cat_max))
        | Neg (Lt (cat, x)) ->
            Map.update ranges cat ~f:(fun (Some (cat_min, cat_max)) ->
                (max cat_min x, cat_max))
        | Neg (Gt (cat, x)) ->
            Map.update ranges cat ~f:(fun (Some (cat_min, cat_max)) ->
                (cat_min, min x cat_max)))
    in

    Map.fold ranges ~init:1 ~f:(fun ~key:_ ~data:(min, max) acc ->
        (max - min + 1) * acc)
  in

  let solutions =
    let rec loop workflow_id ctx =
      let (Workflow (conds, final_decision)) =
        Map.find_exn workflows workflow_id
      in
      let new_conds, sols =
        fold conds ~init:(ctx, []) ~f:(fun (ctx, sols) (cond, decision) ->
            match decision with
            | Acc -> (Neg cond :: ctx, (Cond cond :: ctx) :: sols)
            | Rej -> (Neg cond :: ctx, sols)
            | Goto w -> (Neg cond :: ctx, loop w (Cond cond :: ctx) @ sols))
      in

      match final_decision with
      | Acc -> new_conds :: sols
      | Rej -> sols
      | Goto w -> loop w new_conds @ sols
    in
    loop "in" []
  in

  solutions |> map ~f:combinations |> sum

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
