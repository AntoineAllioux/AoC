open Base

type cat = X | M | A | S [@@deriving sexp_of, compare, equal]

module Cat = struct
  module T = struct
    type t = cat [@@deriving sexp_of, compare, equal]
  end

  include T
  include Comparator.Make (T)
end

type decision = Acc | Rej | Goto of string
type cond = Lt of cat * int | Gt of cat * int
type workflow = Workflow of (cond * decision) list * decision
type rating = Rat of cat * int
