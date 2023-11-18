open Program
open Monad
open Stdio
open Function

type tape = Tape of int array

type amp_state = {
  tape: tape;
  pos: int;
}

module Normal = struct 
  type 'a t = 
    | Halted
    | Continue of 'a
    | Read of (int -> 'a program)
    | Write of int * 'a program

  let map (x: 'a t) ~(f: 'a -> 'b) =
    match x with
    | Halted -> Halted
    | Continue cont -> Continue (f cont)
    | Read cont -> Read (Program.map ~f $ cont)
    | Write (value, cont) -> Write (value, Program.map cont ~f)
end

type 'a normal = 'a Normal.t

module State = State.Make(struct type t = amp_state end)
open State
open Let_syntax(State)

type 'a state = 'a State.t

open Normal

let continue x = pure (Continue x)
let halt = pure Halted
let read_input cont = pure (Read cont)
let write_output value cont = pure (Write (value, cont))

let set_pos pos =
  modify (fun s -> {s with pos = pos})

let incr_pos n = 
  modify (fun ({pos; _} as s) -> {s with pos = pos + n})

let rec eval (debug : bool) (ins : 'a program) : 'a normal state =
  let print_debug (str : string) =
    if debug then
      printf "%s" str
    else 
      () in
  let* {tape = Tape(tape); pos} = get in
  match ins with
  | Pure x -> continue x
  | Free (Read (mode, pos, cont)) ->
    let value = match mode with
    | Position -> tape.(tape.(pos))
    | Immediate -> tape.(pos) in
    let _ = print_debug (Printf.sprintf "Reading value %d\n" value) in
    eval debug (cont value)
  | Free (Write (value, pos, cont)) ->
    let _ = print_debug (Printf.sprintf "Writing %d at pos %d\n" value pos) in
    let _ = tape.(pos) <- value in 
    eval debug cont
  | Free (GetPos cont) -> 
    let _ = print_debug (Printf.sprintf "Getting pos %d\n" pos) in
    eval debug (cont pos)
  | Free (SetPos (pos, cont)) ->
    let _ = print_debug (Printf.sprintf "Setting pos to %d\n" pos) in 
    let* _ = set_pos pos in
    eval debug cont 
  | Free (In cont) ->
    let _ = print_debug (Printf.sprintf "Waiting for input\n") in
    let cont' x = 
      begin
        print_debug (Printf.sprintf "Got input %d\n" x);
        cont x
      end in
    read_input cont'
  | Free (Out (value, cont)) ->
    let _ = print_debug (Printf.sprintf "Outputing %d\n" value) in
    write_output value cont
  | Free Halt -> 
    let _ = print_debug "Halting" in
    halt

let rec run debug cont = 
  let* value = eval debug cont in
  match value with
  | Continue () -> run debug step
  | _ -> pure value