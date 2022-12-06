open Base
open Stdio
open List
open Angstrom
open Continue_or_stop

type stack = char Stack.t

type ship = (int, stack, Int.comparator_witness) Map.t

type move = int * int * int

let rec int_fold (n : int) ~(init : 'a) ~(f : 'a -> int -> 'a) =
  if n = 0 then
    init
  else
    int_fold (n - 1) ~init:(f init n) ~f

let int_iter (n : int) ~(f : int -> unit) =
  int_fold n ~init:() ~f:(fun _ -> f)

let get_stack ship x =
  match Map.find ship x with
  | Some stack -> (ship, stack)
  | None -> 
     let stack = Stack.create () in
     (Map.add_exn ship x stack, stack)
  
let add_crate ship x c = 
  let (ship, stack) = get_stack ship x in
  let () = Stack.push stack c in 
  ship

let parse_crate = 
  (char '[' *> any_char <* char ']' >>| fun x -> Some x)
  <|> (string "   " >>| fun _ -> None)

let parse_ship_line = sep_by (char ' ')  parse_crate

let reverse_stack stack =
  let new_stack = Stack.create () in
  let () = Stack.iter stack (Stack.push new_stack)
  in new_stack

let parse_ship l ship = 
  let aux ship s =
    match parse_string ~consume:All parse_ship_line s with
    | Ok l -> 
       let aux x ship crate = Option.fold crate ~init:ship ~f:(fun ship -> add_crate ship (x + 1))
       in Continue (List.foldi l ~init:ship ~f:aux)
    | Error _ -> Stop (Map.map ship reverse_stack)   
  in 
  List.fold_until l ~init:ship ~f:aux ~finish:(fun _ -> ship) 

let parse_move s =
  let moves = String.split_on_chars s ~on:[' '] in
  let n = Int.of_string (List.nth_exn moves 1) in
  let x = Int.of_string (List.nth_exn moves 3) in
  let y = Int.of_string (List.nth_exn moves 5) in
  (n, x, y)

let parse_moves = List.map ~f:parse_move

let move_crates ship (n, x, y) =
  let stack_x = Map.find_exn ship x in
  let temp_stack = Stack.create () in
  let () = int_iter n ~f:(fun _ -> (Stack.push temp_stack (Stack.pop_exn stack_x))) in
  let (ship, stack_y) = get_stack ship y in
  let () = Stack.iter temp_stack (Stack.push stack_y) in
  ship

let process_moves_one ship moves =
  let aux ship (n, x, y) = 
    int_fold n ~init:ship ~f:(fun ship _ -> move_crates ship (1, x, y))
  in List.fold moves
       ~init:ship
       ~f:aux

let process_moves_two ship moves =
  List.fold moves
    ~init:ship
    ~f:move_crates

let pp_ship (ship : ship) =
  let aux (n : int)  =
    let () = printf "\n%d: " n in
    match Map.find ship n with 
    | None -> ()
    | Some col -> Stack.iter col ~f:(printf "[ %c ] ") in
  let () = Map.iter_keys ship ~f:aux in
  printf "\n"

let input = In_channel.read_lines "input"

let one =
  let ship = parse_ship input (Map.empty (module Int)) in
  let moves = parse_moves (List.tl_exn (List.drop_while input ~f:(fun s -> String.length s <> 0))) in
  let ship = process_moves_one ship moves in
  pp_ship ship

let two =
  let ship = parse_ship input (Map.empty (module Int)) in
  let moves = parse_moves (List.tl_exn (List.drop_while input ~f:(fun s -> String.length s <> 0))) in
  let ship = process_moves_two ship moves in
  pp_ship ship
