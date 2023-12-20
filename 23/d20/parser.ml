open Base
open Angstrom
open Let_syntax
open Model

let alpha = take_while1 (function 'a' .. 'z' -> true | _ -> false)

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

let module_type_p =
  let* sym = peek_char in
  match sym with
  | Some '&' ->
      let* _ = take 1 in
      return Conj
  | Some '%' ->
      let* _ = take 1 in
      return (Flip false)
  | _ -> return Out

let module_p =
  let* mod_type = module_type_p in
  let* id = alpha in
  let mod_type = if String.equal id "broadcaster" then Broad else mod_type in
  let* _ = string " -> " in
  let* succ = sep_by (string ", ") alpha in
  let* _ = end_of_line in
  return (id, mod_type, succ)

let input_p = many module_p
