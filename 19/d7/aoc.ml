open Stdio
open In_channel
open Machine
open List
open Traversable
open Either
open Program
open Monad

module Array = Base.Array
module Int = Base.Int
module String = Base.String

(* An amplifier is the state of its machine along with the current program running on it *)
type amp = Amp of amp_state * unit program

open Let_syntax (State)
open State

let pp_state =
  let* {pos; tape = Tape(tape)} = get in
  pure (
    begin
      printf "pos:%d, tape:\n" pos;
      printf "[";
      Array.iteri tape ~f:(fun i code -> printf "%d -> %d; " i code);
      printf "]\n";
    end)

let init_tape opcodes = 
  let tape = Array.create ~len:(List.length opcodes) 0 in
  let f pos op = 
    begin
      tape.(pos) <- op;
      pos + 1
    end in
  let _ = List.fold opcodes ~init:0 ~f in
  Tape(tape)

let part1 debug opcodes = 
  let phases = permutations (range 0 5) in
  let phase_run acc phase_config = 
    let amp_run acc phase =  
      let thread =  
        let* Read(cont) = run debug (Program.pure ()) in
        let* Read(cont) = run debug (cont phase) in
        let* Write(x, _) = run debug (cont acc) in
        pure x in
      let tape = init_tape opcodes in
      let init_state = {pos = 0; tape} in
      eval_state thread init_state in
    let value = fold phase_config ~init:0 ~f:amp_run in
    max acc value in
  fold phases ~init:0 ~f:phase_run

let part2 debug opcodes = 
  let module Either = Right_focused (struct type t = int end) in
  let open Map_accum (Either) (List_traversable) (struct type t = int end) in

  let init_state _ = 
    let tape = init_tape opcodes in 
    {pos = 0; tape} in

  let phases = permutations (range 5 10) in

  (* Returns the maximum thrust found up to this particular phase configuration *)
  let phase_run acc (phase_config : int list) = 
    let init_amps = List.map phase_config ~f:(fun x -> Amp (init_state x, Program.pure ())) in

    (* Computes the thrust for this particular phase configuration *)
    let rec cycle amps power flag = 
      let amps_with_phases = List.zip_exn amps phase_config in
      let amp_run (acc : int) (Amp (state, cont), phase : amp * int): (amp * int) Either.t =  
        let thread =
          let* cont_val = run debug cont in
          match cont_val with
          | Read cont ->
              if flag then
                let* Read cont = run debug (cont phase) in
                run debug (cont acc)
              else 
                run debug (cont acc)
          | cont_val -> pure cont_val in
        let (halting_state, state') = run_state thread state in
        match halting_state with
        | Write (x, cont) -> Right (Amp (state', cont), x)
        | Halted -> Left acc in

      match map_accum amps_with_phases ~init:power ~f:amp_run with
      | Right (amps, power) -> cycle amps power false
      | Left x -> x in

    max acc (cycle init_amps 0 true) in
  fold phases ~init:0 ~f:phase_run

let _ =
  let debug = false in
  let input = read_all "input" in
  let opcodes = List.map ~f:Int.of_string (String.split input ~on:',') in 
  begin
    printf "Part1: %d\n" (part1 debug opcodes);
    printf "Part2: %d\n" (part2 debug opcodes);
  end