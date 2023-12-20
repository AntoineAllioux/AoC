open Base
open Stdio
open List
open Model

module Node = struct
  type t = string [@@deriving compare, hash, equal]
end

module Edge = struct
  type t = string [@@deriving compare, hash, equal]

  let default = ""
end

module G = Graph.Persistent.Digraph.ConcreteLabeled (Node) (Edge)

module Dot = Graph.Graphviz.Dot (struct
  include G

  let edge_attributes (_, e, _) = [ `Label e; `Color 4711 ]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = [ `Shape `Box ]
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let print_graph graph =
  let g =
    Map.fold graph ~init:G.empty ~f:(fun ~key ~data:(Nd (_, succ)) g ->
        fold succ ~init:g ~f:(fun g (succ_id, _) -> G.add_edge g key succ_id))
  in
  let file = Out_channel.create "graph.dot" in
  Dot.output_graph file g
