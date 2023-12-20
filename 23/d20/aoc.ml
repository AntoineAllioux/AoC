open Stdio
open Base.List
open Base.Int
open Parser
open Model
open Graph_utils
module Map = Base.Map
module String = Base.String
module Queue = Base.Queue

module State = State.Make (struct
  type t = graph
end)

open Monad.Let_syntax (State)

let preds id graph =
  Map.fold graph ~init:[] ~f:(fun ~key:id2 ~data:(Nd (_, succ)) acc ->
      match find succ ~f:(fun (succ_id, _) -> String.equal id succ_id) with
      | None -> acc
      | Some (_, signal) -> (id2, signal) :: acc)

let update_src (Pulse (src, tgt, pulse_type)) =
  match src with
  | Some src ->
      State.modify (fun graph ->
          Map.update graph src ~f:(fun (Some (Nd (mod_type, succ))) ->
              let new_succ =
                map succ ~f:(fun (succ_id, sig2) ->
                    if String.equal succ_id tgt then (succ_id, pulse_type)
                    else (succ_id, sig2))
              in
              Nd (mod_type, new_succ)))
  | None -> State.pure ()

let generate_pulses (Pulse (_, tgt, pulse_type) as pulse) =
  let* _ = update_src pulse in
  let* graph = State.get in
  match Map.find graph tgt with
  | None -> State.pure []
  | Some (Nd (mod_type, succ)) -> (
      match mod_type with
      | Broad ->
          let pulses =
            map ~f:(fun (succ_id, _) -> Pulse (Some tgt, succ_id, Low)) succ
          in
          State.pure pulses
      | Flip b -> (
          match pulse_type with
          | Low ->
              let* _ =
                State.modify (fun graph ->
                    Map.update graph tgt ~f:(fun (Some (Nd (_, succ))) ->
                        Nd (Flip (not b), succ)))
              in
              let new_signal = if b then Low else High in
              let pulses =
                map
                  ~f:(fun (succ_id, _) -> Pulse (Some tgt, succ_id, new_signal))
                  succ
              in
              State.pure pulses
          | _ -> State.pure [])
      | Conj ->
          let preds = preds tgt graph in
          let signal =
            if
              for_all preds ~f:(fun (_, signal) -> equal_pulse_type signal High)
            then Low
            else High
          in
          let pulses =
            map ~f:(fun (succ_id, _) -> Pulse (Some tgt, succ_id, signal)) succ
          in
          State.pure pulses
      | Out -> State.pure [])

let fold_pulses f init =
  let pulses = Queue.create () in
  Queue.enqueue pulses (Pulse (None, "broadcaster", Low));
  let rec loop init =
    if Queue.is_empty pulses then State.pure init
    else
      let pulse = Queue.dequeue_exn pulses in
      let* new_pulses = generate_pulses pulse in
      Queue.enqueue_all pulses new_pulses;
      loop (f pulse init)
  in
  loop init

let part1 graph =
  let process_pulse (Pulse (_, _, pulse_type)) (low, high) =
    if equal_pulse_type Low pulse_type then (low + 1, high) else (low, high + 1)
  in

  let rec loop low high n =
    if n = 0 then State.pure (low * high)
    else
      let* new_low, new_high = fold_pulses process_pulse (low, high) in
      loop new_low new_high (n - 1)
  in

  State.eval_state (loop 0 0 1000) graph

let part2 graph =
  let process_pulse n (Pulse (src, _, pulse_type)) counts =
    let nodes = [ "jg"; "mr"; "rz"; "kv" ] in
    match src with
    | Some src
      when mem nodes src ~equal:String.equal && equal_pulse_type pulse_type High
      ->
        Map.change counts src ~f:(function None -> Some n | Some v -> Some v)
    | _ -> counts
  in

  let rec gcd n m =
    if n > m then gcd m n else if n = 0 then m else gcd (m % n) n
  in
  let lcm n m = n * m / gcd n m in

  let counts = Map.empty (module String) in

  let rec loop counts n =
    if Map.length counts = 4 then
      State.pure (reduce_exn (Map.data counts) ~f:lcm)
    else
      let* counts = fold_pulses (process_pulse n) counts in
      loop counts (n + 1)
  in

  State.eval_state (loop counts 1) graph

let _ =
  let input = In_channel.read_all "input" in

  let (Ok parsed_modules) =
    Angstrom.parse_string ~consume:Angstrom.Consume.All input_p input
  in

  let f graph (id, mod_type, succ) =
    Map.add_exn graph ~key:id
      ~data:(Nd (mod_type, map succ ~f:(fun x -> (x, Nothing))))
  in

  let graph = fold parsed_modules ~init:(Map.empty (module String)) ~f in

  (* Write a dot file of the graph *)
  print_graph graph;

  printf "Part 1: %d\n" (part1 graph);
  printf "Part 2: %d\n" (part2 graph)
