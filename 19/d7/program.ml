open Base.Int
open Monad
open Function

type mode = Position | Immediate

type opcode =
  | Halt
  | Add of mode * mode
  | Mul of mode * mode
  | In
  | Out of mode
  | JumpIfTrue of mode * mode
  | JumpIfFalse of mode * mode
  | Lt of mode * mode
  | Eq of mode * mode

let digit number digits start = (number / pow 10 start) % (pow 10 digits) 

let parse_mode op param = 
  match digit op 1 (param + 2) with 
  | 0 -> Position
  | 1 -> Immediate

let parse_opcode op =
  match digit op 2 0 with
  | 1 -> Add (parse_mode op 0, parse_mode op 1)
  | 2 -> Mul (parse_mode op 0, parse_mode op 1)
  | 3 -> In
  | 4 -> Out (parse_mode op 0)
  | 5 -> JumpIfTrue (parse_mode op 0, parse_mode op 1)
  | 6 -> JumpIfFalse (parse_mode op 0, parse_mode op 1)
  | 7 -> Lt (parse_mode op 0, parse_mode op 1)
  | 8 -> Eq (parse_mode op 0, parse_mode op 1)
  | 99 -> Halt

module Instruction = struct 
  type 'a t = 
    | Read of mode * int * (int -> 'a)
    | Write of int * int * 'a
    | GetPos of (int -> 'a)
    | SetPos of int * 'a
    | In of (int -> 'a)
    | Out of int * 'a
    | Halt

  let map (x: 'a t) ~(f:'a -> 'b): 'b t = 
    match x with
    | Read (mode, pos, cont) -> Read (mode, pos, f $ cont) 
    | Write (pos, value, cont) -> Write (pos, value, f cont)
    | SetPos (pos, cont) -> SetPos (pos, f cont)
    | GetPos cont -> GetPos (f $ cont)
    | In cont -> In (f $ cont)
    | Out (value, cont) -> Out (value, f cont)
    | Halt -> Halt
end

module Program = Free.Make(Instruction)
open Program

type 'a program = 'a Program.t
type 'a instruction = 'a Instruction.t

open Let_syntax (Program)

let read md pos = lift (Read (md, pos, id))
let write value pos = lift (Write (value, pos, id))
let read_input = lift (In id)
let write_output x = lift (Out (x, ()))
let set_pos pos = lift (SetPos (pos, ()))
let get_pos = lift (GetPos id)
let halt = lift Halt
let incr_pos n =
  let* pos = get_pos in 
  set_pos (pos + n)

let step =
  let* pos = get_pos in
  let* opcode = read Immediate pos in
  match (parse_opcode opcode) with 
  | Halt -> halt
  | Add (md1, md2) ->
    let* param1 = read md1 (pos + 1) in
    let* param2 = read md2 (pos + 2) in
    let* param3 = read Immediate (pos + 3) in
    let* _ = write (param1 + param2) param3 in
    incr_pos 4
  | Mul (md1, md2) ->
    let* param1 = read md1 (pos + 1) in
    let* param2 = read md2 (pos + 2) in
    let* param3 = read Immediate (pos + 3) in 
    let* _ = write (param1 * param2) param3 in
    incr_pos 4
  | In -> 
    let* param = read Immediate (pos + 1) in
    let* value = read_input in
    let* _ = write value param in
    incr_pos 2
  | Out (md) -> 
    let* value = read md (pos + 1) in
    let* _ = write_output value in
    incr_pos 2
  | JumpIfTrue (md1, md2) ->
      let* param1 = read md1 (pos + 1) in
      let* param2 = read md2 (pos + 2) in
      if param1 = 0 then
        incr_pos 3
      else
        set_pos param2
  | JumpIfFalse (md1, md2) ->
    let* param1 = read md1 (pos + 1) in
    let* param2 = read md2 (pos + 2) in
    if param1 = 0 then
      set_pos param2
    else
      incr_pos 3
  | Lt (md1, md2) -> 
    let* param1 = read md1 (pos + 1) in
    let* param2 = read md2 (pos + 2) in
    let* param3 = read Immediate (pos + 3) in
    let* _ = 
      if param1 < param2 then
        write 1 param3
      else
        write 0 param3 in
    incr_pos 4
  | Eq (md1, md2) -> 
    let* param1 = read md1 (pos + 1) in
    let* param2 = read md2 (pos + 2) in
    let* param3 = read Immediate (pos + 3) in
    let* _ = 
      if param1 = param2 then
        write 1 param3
      else
        write 0 param3 in
    incr_pos 4