open Base
open Stdio

let ln = In_channel.read_lines "input"

type move = Rock | Paper | Scissors

type outcome = Win | Draw | Defeat

let parse = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _  -> failwith "Unexpected character"

let l =
  let f s =
    let (a, b) = String.lsplit2_exn s ~on:' '
    in (parse a, parse b) 
  in List.map ln f

let play = function
  | (Rock, Scissors) | (Scissors, Paper)    | (Paper, Rock)  -> Defeat
  | (Rock, Rock)     | (Scissors, Scissors) | (Paper, Paper) -> Draw
  | _ -> Win

let weight = function
  | Rock     -> 1
  | Paper    -> 2
  | Scissors -> 3

let score = function
  | Win    -> 6
  | Draw   -> 3
  | Defeat -> 0 

let total =
  List.map l (fun (x, y) -> score (play (x, y)) + weight y)
  |> List.reduce_exn ~f:( + )
