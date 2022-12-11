open Base
open Stdio
open Angstrom
open Utils

type operation =
  | Add of int option
  | Mul of int option 

type monkey =
  {
    items: int list;
    op : operation;
    div : int ;
    choice : int * int;
    count : int
  }

type state = (int, monkey, Int.comparator_witness) Map.t
module State = StateMonad.Make(struct type t = state end)
module StateUtils = Monad.Utils(State)
open StateUtils

let ( let+ ) = State.bind

let get_monkey monkey_id =
  let+ state = State.get in
  State.return (Map.find_exn state monkey_id)

let replace_monkey monkey_id monkey =
  let f state =
    Map.add_exn (Map.remove state monkey_id) ~key:monkey_id ~data:monkey in
  State.modify f

(** Parsing *)
let parse_number =
  let* n = take_till (fun c -> Char.compare c '0' < 0 || Char.compare c '9' > 0)
  in return (Int.of_string n)

let parse_op =
  let* Some c = peek_char in
  if Char.compare c 'o' = 0 then
    string "old" *> return None
  else
    let* n = parse_number in
    return (Some n)    

let parse_monkey =
  let* _ = string "Monkey " *> any_uint8 <* string ":\n" in
  let* items = string "  Starting items: " *> (sep_by (string ", ") parse_number) <* char '\n' in
  let* op = string "  Operation: new = old " *> any_char in
  let* op_x = char ' ' *> parse_op <* char '\n' in
  let* div = string "  Test: divisible by " *> parse_number <* char '\n' in
  let* monkey_true = string "    If true: throw to monkey " *> parse_number <* char '\n' in
  let* monkey_false = string "    If false: throw to monkey " *> parse_number <* char '\n' in
  let parsed_op =
    match op with
    | '+' -> Add op_x  
    | '*' -> Mul op_x
    | _ -> failwith "Parsing failed" in 
  return
    { items = items;
      op = parsed_op;
      div = div;
      choice = (monkey_true, monkey_false);
      count = 0
    }

let parse_monkeys = sep_by (char '\n') parse_monkey

let pp_monkeys state =
  let () = printf "\n" in
  let f ~(key : int) ~(data : monkey) str =
    String.concat
      [str ; Printf.sprintf "Monkey %d inspected %d items.\n" key data.count] in
  let str = Map.fold state ~init:"" ~f:f in
  let business =
    Map.to_alist state
    |> List.map ~f:(fun (_, monkey) -> monkey.count)
    |> List.sort ~compare:(fun x y -> Int.compare y x)
    |> fun l -> (List.nth_exn l 0) * (List.nth_exn l 1)
  in String.concat
       [str ; Printf.sprintf "\nMonkey business: %d\n" business]

let monkeys =
  let res =
    In_channel.read_all "input"
    |> parse_string ~consume:Consume.All parse_monkeys in
  match res with
  | Ok l -> l
  | Error _ -> failwith "Could not parse monkeys"

let empty_state =
  let f (i : int) acc monkey =
    Map.add_exn acc ~key:i ~data:monkey
  in List.foldi monkeys ~init:(Map.empty (module Int)) ~f:f

let throw_item item monkey_id =
  let+ monkey = get_monkey monkey_id in
  let new_monkey = { monkey with items = monkey.items @ [item] } in
  replace_monkey monkey_id new_monkey

let compute op x =
  match op with
  | Add (None) -> x + x
  | Add (Some y) -> x + y
  | Mul (None) -> x * x
  | Mul (Some y) -> x * y 

let process_monkey calc_worry monkey_id =
  let+ monkey = get_monkey monkey_id in
  let items = monkey.items in
  let f _ item =
    let worry = calc_worry (compute monkey.op item) in
    let cond = worry % monkey.div in
    if cond = 0 then
      throw_item worry (fst monkey.choice)
    else
      throw_item worry (snd monkey.choice) in
  let+ () = foldm () f items in
  let new_monkey =
    {
      monkey with
      items = [] ;
      count = monkey.count + List.length items
    } in
  replace_monkey monkey_id new_monkey

let range x y =
  int_fold (y - x + 1) ~init:[] ~f:(fun l i -> (i + x - 1) :: l) 

let processed_monkeys calc_worry rounds =
  let n = List.length monkeys in
  let process_round _ _ =
    foldm () (fun _ -> process_monkey calc_worry) (range 0 (n - 1)) in
  let state = foldm () process_round (range 1 rounds) in
  State.eval_state state empty_state

let one =
  let calc_worry worry = worry / 3 in
  processed_monkeys calc_worry 20

let two =
  let factor = 
    List.map monkeys ~f:(fun monkey -> monkey.div)
    |> List.reduce_exn ~f:( * ) in
  let calc_worry worry = worry % factor in
  processed_monkeys calc_worry 10000
    
let () =
  printf "\n****************************************\n\
          ** Part one\n\
          ****************************************\n\n\
          %s"
    (pp_monkeys one)

let () = 
  printf "\n****************************************\n\
          ** Part two\n\
          ****************************************\n\n\
          %s"
    (pp_monkeys two)
