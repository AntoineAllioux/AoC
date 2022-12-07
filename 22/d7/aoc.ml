open Base
open Stdio
open List
open Continue_or_stop
open Angstrom

type filesystem =
  | Dir of string * filesystem list
  | File of string * int

type ctx =
  | Top
  | DirCtx of string * filesystem list * ctx

type loc = ctx * filesystem

let rec fs_fold fs ~(file_case : string -> int -> 'a) ~(dir_case : string -> 'a list -> 'a) : 'a =
  match fs with
  | Dir (name, files) -> dir_case name (List.map files ~f:(fs_fold ~file_case ~dir_case))
  | File (name, size) -> file_case name size

let up = function
  | (Top, _) -> failwith "Cannot move up"
  | (DirCtx (name, files, ctx), file) -> (ctx, Dir (name, file :: files))

let find_file (files : filesystem list) (name : string) : filesystem option =
  let aux = function
    | Dir (dir_name, _) -> String.compare name dir_name = 0
    | File (file_name, _) -> String.compare name file_name = 0 
  in List.find files aux

let remove_file files name =
  let aux = function
    | Dir (dir_name, _) -> String.compare name dir_name <> 0
    | File (file_name, _) -> String.compare name file_name <> 0
  in List.filter files aux

let down (ctx, file) name : loc =
  match file with
  | File _ -> failwith "Cannot move down from a file"
  | Dir (dir_name, files) ->
     match find_file files name with
     | Some file -> (DirCtx (dir_name, remove_file files name, ctx), file)
     | None -> failwith "Trying to move to a non-existent directory"

let create_file (ctx, file) name size =
  match file with
  | File _ -> failwith "You have to be in a directory to add a file"
  | Dir (dir_name, files) ->
     match find_file files name with
     | Some _ -> (ctx, file)
     | None -> (ctx, Dir (dir_name, File (name, size) :: files))

let create_dir (ctx, fs) name =
  match fs with
  | File _ -> failwith "You have to be in a directory to create a directory"
  | Dir (dir_name, files) ->
     match find_file files name with
     | Some _ -> (ctx, fs)
     | None -> (ctx, Dir (dir_name, Dir (name, []) :: files))

let rec root (ctx, fs) : loc =
  match ctx with
  | Top -> (ctx, fs)
  | _ -> root (up (ctx, fs))

type command =
  | Root
  | Up
  | Down of string
  | CreateDir of string
  | CreateFile of string * int

let interpret_command fs command =
  match command with
  | Root -> root fs
  | Up -> up fs
  | Down name -> down fs name
  | CreateDir name -> create_dir fs name 
  | CreateFile (name, size) -> create_file fs name size

let interpret_commands fs commands =
  List.fold commands ~init:fs ~f:interpret_command 

(** Parsing *)
let word = take_while (fun c -> Char.compare c '\n' <> 0)

let parse_cd =
  let f s =
    if String.compare s "/" = 0 then
      Root
    else if String.compare s ".." = 0 then
      Up
    else
      Down s
  in 
  (string "$ cd " *> word) >>| f

let parse_file =
  let f (size, name) = CreateFile (name, Int.of_string size) in 
  both (take_while1 (Char.is_digit) <* char ' ') word >>| f

let parse_dir =
  string "dir " *> word >>| (fun name -> CreateDir name)
  
let parse_command =
  parse_cd
  <|> parse_file
  <|> parse_dir

let input =
  let f x = match x with
    | Ok command -> [command]
    | Error _ -> [] in
  In_channel.read_lines "input"
  |> List.map ~f:(parse_string ~consume:All parse_command)
  |> (fun x ->  List.(>>=) x f)
  |> interpret_commands (Top, Dir ("/", []))

let file_size fs : int =
  let file_case _ size = size in
  let dir_case _ sizes = Option.fold (List.reduce sizes ~f:(+)) ~init:0 ~f:(fun _  x -> x) in
  fs_fold fs ~file_case ~dir_case

let list_min l =
  let aux acc x = match acc with
    | Some y -> if x < y then Some x else Some y
    | None -> Some x
  in List.fold l ~init:None ~f:aux

let sum_list l = Option.fold (List.reduce l ~f:(+)) ~init:0 ~f:(fun _ x -> x)

let one fs =
  let file_case _ size = (0, size) in
  let dir_case _ acc =
    let candidates = List.map acc ~f:fst in
    let candidates_sum = sum_list candidates in
    let sizes = List.map acc ~f:snd in
    let dir_size = sum_list sizes in
    if dir_size <= 100000 then
      (dir_size + candidates_sum, dir_size)
    else
      (candidates_sum, dir_size)
  in fst (fs_fold fs ~file_case ~dir_case)

let two fs size_limit =
  let file_case _ size = (None, size) in
  let dir_case _ acc =
    let candidates = List.map acc ~f:fst in
    let candidates_min = list_min (List.bind candidates ~f:(fun x -> Option.fold x ~init:[] ~f:(fun _ x -> [x]))) in
    let sizes = List.map acc ~f:snd in
    let dir_size = sum_list sizes in
    match candidates_min with
    | Some x ->
       if dir_size < x && dir_size >= size_limit then
         (Some dir_size, dir_size) 
       else
         (Some x, dir_size)
    | None ->
       if dir_size >= size_limit then
         (Some dir_size, dir_size) 
       else
         (None, dir_size)
  in fst (fs_fold fs ~file_case ~dir_case)

let fs = snd (root input)


let free_space = 70000000 - file_size fs
let size = 30000000 - free_space

let () = printf "one: %d, two: %d\n" (one fs) (Option.value_exn (two fs size))
