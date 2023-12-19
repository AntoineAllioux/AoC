open Base
open Fn
open Model
open Angstrom

let alpha = take_while1 (function 'a' .. 'z' -> true | _ -> false)

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| Int.of_string

let cat_p =
  char 'x' >>| const X
  <|> (char 'm' >>| const M)
  <|> (char 'a' >>| const A)
  <|> (char 's' >>| const S)

let decision_p =
  char 'A' >>| const Acc
  <|> (char 'R' >>| const Rej)
  <|> (alpha >>| fun id -> Goto id)

let condition_p =
  let open Let_syntax in
  let* cat = cat_p in
  let* cond = char '<' <|> char '>' in
  let* value = integer in
  let* _ = char ':' in
  let* decision = decision_p in

  if Char.equal cond '<' then return (Lt (cat, value), decision)
  else return (Gt (cat, value), decision)

let rules_p =
  let open Let_syntax in
  let* conditions = sep_by (char ',') condition_p in
  let* _ = char ',' in
  let* decision = decision_p in
  return (Workflow (conditions, decision))

let workflow_p =
  let open Let_syntax in
  let* id = alpha in
  let* _ = char '{' in
  let* rules = rules_p in
  let* _ = char '}' in
  return (id, rules)

let rating_p =
  let open Let_syntax in
  let rating_p =
    let* cat = cat_p in
    let* _ = char '=' in
    let* value = integer in
    return (Rat (cat, value))
  in

  let* _ = char '{' in
  let* ratings = sep_by (char ',') rating_p in
  let* _ = char '}' in
  return ratings

let input_p =
  let open Let_syntax in
  let* workflows = sep_by end_of_line workflow_p in
  let* _ = end_of_line in
  let* _ = end_of_line in
  let* ratings = sep_by end_of_line rating_p in
  let* _ = end_of_line in
  return (workflows, ratings)
