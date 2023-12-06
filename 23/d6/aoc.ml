open Base
open List
open Stdio

let times l = fold l ~init:1 ~f:( * )

let score (t, d) =
  let rec loop i acc =
    if i > t then
      acc
    else if (t - i) * i > d then
      loop (i + 1) (acc + 1)
    else
      loop (i + 1) acc
  in loop 1 0

let part1 =
  [(48, 261); (93, 1192); (84, 1019); (66, 1063)]
  |> map ~f:score
  |> times
  
let part2 = score (48938466, 261119210191063)

let _ =
  begin
    printf "Part 1: %d\n" part1;
    printf "Part 2: %d\n" part2
  end
