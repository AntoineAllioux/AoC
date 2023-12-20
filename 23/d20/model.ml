open Base

type mod_type = Broad | Flip of bool | Conj | Out

type pulse_type = Nothing | Low | High
[@@deriving equal]

type pulse = Pulse of (string option * string * pulse_type)
type node = Nd of mod_type * (string * pulse_type) list
type graph = (string, node, String.comparator_witness) Map.t
