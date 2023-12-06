open Base
open List
open Stdio

let times l = fold l ~init:1 ~f:( * )

let score (t, d) =
  map (range 0 (t + 1)) ~f:(fun x -> (t - x) * x)
  |> filter ~f:(fun x -> x > d)
  |> length

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
