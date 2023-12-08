open Base
open Stdio
open List
open Container.Continue_or_stop

let rec pgcd n m =
  if n > m then
    pgcd m n
  else if n = 0 then
    m
  else
    pgcd (m % n) n

let ppcm n m =
  (n * m) / (pgcd n m)

let steps desert moves check start =
  let open Either in
  
  let apply_move (start, steps) move =
    let node =
      let (l, r) = Map.find_exn desert start in
      if Char.equal move 'L' then l else r in
    if check node then
      Stop (First (steps + 1))
    else
      Continue (node, steps + 1) in
  
  let rec loop start steps =
    let round =
      fold_until moves
        ~init:(start, steps)
        ~f:apply_move
        ~finish:(fun x -> Second x) in
    match round with
    | First steps -> steps
    | Second (node, steps) -> loop node steps in
  
  loop start 0

let part1 desert moves =
  let check node = String.equal node "ZZZ" in
  steps desert moves check "AAA" 

let part2 desert moves =
  let check c node = Char.equal (last_exn (String.to_list node)) c in
  Map.keys desert
  |> filter ~f:(check 'A')
  |> map ~f:(steps desert moves (check 'Z')) 
  |> reduce_exn ~f:ppcm

let _ =
  let moves_raw :: _ :: desert_raw = In_channel.read_lines "input" in
  let moves = String.to_list moves_raw in
  let desert =
    map desert_raw
      ~f:(fun s ->
        let [a; _; _; _; b;_; c;_] = String.split_on_chars s ~on:[' '; '='; '('; ')'; ','] in
        (a, (b, c)))
    |> Map.of_alist_exn (module String) in
  begin 
    printf "Part 1: %d\n" (part1 desert moves);
    printf "Part 2: %d\n" (part2 desert moves)
  end
