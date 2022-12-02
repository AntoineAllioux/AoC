open Base
open Stdio

let ln = In_channel.read_lines "input"

type move = Rock | Paper | Scissors

type outcome = Win | Draw | Defeat

let parse_move = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _   -> failwith "Unexpected character"

let parse_outcome = function
  | "X" -> Defeat
  | "Y" -> Draw
  | "Z" -> Win
  | _   -> failwith "Unexpected character"

let l =
  let f s =
    let (a, b) = String.lsplit2_exn s ~on:' '
    in (parse_move a, parse_outcome b) 
  in List.map ln f

let weight = function
  | Rock     -> 1
  | Paper    -> 2
  | Scissors -> 3

let score = function
  | Win    -> 6
  | Draw   -> 3
  | Defeat -> 0

let play = function
  | (Rock, Draw)   | (Paper, Defeat) | (Scissors, Win)    -> Rock
  | (Rock, Win)    | (Paper, Draw)   | (Scissors, Defeat) -> Paper
  | (Rock, Defeat) | (Paper, Win)    | (Scissors, Draw)   -> Scissors

let total =
  List.map l (fun (x, y) -> score y + weight (play (x, y)))
  |> List.reduce_exn ~f:( + )
