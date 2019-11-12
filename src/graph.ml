open Space

module I = Ext.Int

type node = position

type edge = node * node * float

type iedge = int * int * float

module EdgeSet =
  Set.Make (struct type t = iedge let compare = compare end)

module PMap =
  Map.Make (struct type t = position let compare = compare end)

type t = {
    pos_indices     : int PMap.t;
    indices_pos     : position I.Map.t;
    last_used_index : int;
    edges           : EdgeSet.t;
}

let empty = {
    pos_indices     = PMap.empty;
    indices_pos     = I.Map.empty;
    last_used_index = -1;
    edges           = EdgeSet.empty;
}

let add_node graph position =
  let last_used_index = graph.last_used_index + 1 in
  let pos_indices = PMap.add position last_used_index graph.pos_indices in
  let indices_pos = I.Map.add last_used_index position graph.indices_pos in
  { graph with pos_indices; indices_pos; last_used_index }

let add_edge graph (a, b, w) =
  let a = PMap.find a graph.pos_indices in
  let b = PMap.find b graph.pos_indices in
  let edges = EdgeSet.add (a, b, w) graph.edges in
  let edges = EdgeSet.add (b, a, w) edges in
  { graph with edges }

let make nodes0 edges0 =
  let graph = List.fold_left add_node empty nodes0 in
  let graph = List.fold_left add_edge graph edges0 in
  graph

let nodes graph =
  I.Map.bindings graph.indices_pos |> List.split |> snd

let node graph idx =
  I.Map.find idx graph.indices_pos

let edges graph =
  EdgeSet.elements graph.edges
  |> List.map (fun (n1, n2, w) -> (node graph n1, node graph n2, w))

let out graph p =
  let a = PMap.find p graph.pos_indices in
  let _, _, greater =
    EdgeSet.split (a, min_int, min_float) graph.edges
  in
  let exception Stop of (node * node * float) list in
  try
    EdgeSet.fold (fun (a', b, w) edges ->
        if a = a' then
          (p, node graph b, w) :: edges
        else
          raise (Stop edges)
    ) greater []
  with Stop edges -> edges
