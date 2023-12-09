open Base
open Stdio
open List
open Fn

let sum = reduce_exn ~f:( + )

let forecast l =
  let rec round stop acc =
    function
    | d1 :: d2 :: ds ->
       round (stop && d2 - d1 = 0) (d2 - d1 :: acc) (d2 :: ds) 
    | _ -> (stop, rev acc) in
  let rec loop acc l =
    let (stop, diff) = round true [] l in
    if stop
    then map (diff :: acc) ~f:last_exn
    else loop (diff :: acc) diff in
  loop [] l @ [last_exn l]
  |> sum

let _ =
  let history =
    In_channel.read_lines "input"
    |> map ~f:(compose (map ~f:Int.of_string) (String.split ~on:' ')) in
  let part1 =
    map history ~f:forecast
    |> sum in
  let part2 =
    map history ~f:(compose forecast rev)
    |> sum in
  begin
    printf "Part 1: %d\n" part1;
    printf "Part 2: %d\n" part2
  end
