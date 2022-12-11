open Base
open Stdio
open Utils

type state = int * int

module State = StateMonad.Make(struct type t = state end)
module StateUtils = Monad.Utils(State)

let ( let* ) = State.bind

type instruction =
  | Add of int
  | Noop

(** Returns the number of cycles of an instruction as well as the value to use
    to update the registry *)
let interpret_ins ins =
  match ins with
  | Add x -> (2, x)
  | Noop -> (1, 0)

(** Run a function which can observe the succesive state updates  *)
let run_function (acc : 'a) (f : 'a -> state -> 'a) (ins : instruction list) : 'a  =
  let aux acc ins : 'a State.t =
    let* (cycle, reg) = State.get in
    let (ins_cycles, reg_update) = interpret_ins ins in
    let acc =
      range_fold_incr 0 (ins_cycles - 1)
        ~init:acc
        ~f:(fun acc i -> f acc (cycle + i, reg)) in
    let* () = State.put (cycle + ins_cycles, reg + reg_update) in
    State.return acc in
  State.eval (StateUtils.foldm acc aux ins) (1, 1)

(** Part 1 function *)
let f1 acc (cycle, reg) =
  let l = [20; 60; 100; 140; 180; 220] in
  match List.find l ~f:(fun n -> n = cycle) with
  | Some _ -> acc + (cycle * reg)
  | None -> acc

(** Part 2 types and function *)
type pixel = L | D
type screen = pixel list

let f2 (screen : screen) ((cycle, reg) : state) : screen =
  let pos = (cycle - 1) % 40 in
  if (reg > pos - 2) && (reg < pos + 2) then
    L :: screen
  else
    D :: screen

(** Pretty printing of the screen *)
let display screen =
  let f n str pixel =
    let pos = n % 40 in
    let str = if pos = 0 then String.concat [str ; "\n"] else str in
    match pixel with
    | L -> String.concat [str ; "#"]
    | D -> String.concat [str ; "."]

  in List.foldi screen  ~init:"" ~f

(** Parsing *)
let parse s = match String.split s ~on:' ' with
  | [_ ; x] -> Add (Int.of_string x)
  | [ _ ] -> Noop
  | _ -> failwith "Parsing failed"

let ins =
  In_channel.read_lines "input"
  |> List.map ~f:parse

let () =
  printf "\n****************************************\n\
          ** Part one\n\
          ****************************************\n\n\
          Signal strength: %d\n\
          \n\n\
          ****************************************\n\
          ** Part two\n\
          ****************************************\n\
          %s"
    (run_function 0 f1 ins)
    (display (List.rev (run_function [] f2 ins)))

