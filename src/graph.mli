(**

   Ce module implémente une structure de graphe purement
   fonctionnelle et non dirigée.

*)

(** Les noeuds du graphe sont des positions. *)
type node = Space.position

(** Une arête est composée de deux extrémités et d'un poid positif. *)
type edge = node * node * float

(** Le type des graphes. *)
type t

(** Le graphe vide. *)
val empty : t

(** [add_node g n] est un graphe qui est égal à [g] étendu par [n]. *)
val add_node : t -> node -> t

(** [add_edge g e] est un graphe qui est égal à [g] étendu par [e]. *)
val add_edge : t -> edge -> t

(** [make nodes edges] est le graphe de noeuds [nodes] et d'arêtes [edges]. *)
val make : node list -> edge list -> t

(** [nodes g] renvoie les noeuds de [g]. *)
val nodes : t -> node list

(** [edges g] renvoie les arêtes de [g]. *)
val edges : t -> edge list

(** [out g n] renvoie les arêtes de [g] sortant de [n]. *)
val out : t -> node -> edge list
