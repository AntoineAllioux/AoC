open Base
open Stdio

let ln : string list = In_channel.read_lines "input"

let (l, _) =
  List.fold ln ~init:([], 0) ~f:(fun (l, n) x ->
      if (String.compare x "" = 0)
      then (n :: l, 0)
      else (l, Int.of_string x + n))  

let sorted = List.sort l ~compare:(fun x y -> Int.compare y x)

let max = List.nth_exn sorted 0

let sum =
  let a = List.nth_exn sorted 0 
  and b = List.nth_exn sorted 1
  and c = List.nth_exn sorted 2
  in a + b + c

let () = Stdlib.Printf.printf "max: %d, sum: %d" max sum
