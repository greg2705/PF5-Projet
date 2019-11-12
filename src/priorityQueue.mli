module type Priority = sig
  include Set.OrderedType
  val infinity : t
  val to_string : t -> string
end

module Make (Key : Set.OrderedType) (Priority : Priority) : sig

  (** Une file de priorité de clés [Key.t] et de priorité [Priority.t]. *)
  type t

  (** La file vide. *)
  val empty : t

  (** La longueur d'une file. *)
  val length : t -> int

  (** L'élément de priorité minimale. *)
  val get_min : t -> (Priority.t * Key.t) option

  (** La file sans son élément minimal. *)
  val remove_min : t -> t

  (** La file étendue avec une clé d'une certaine priorité. *)
  val insert : t -> Key.t -> Priority.t -> t

  (** La file où une clé a priorité plus petite. *)
  val decrease : t -> Key.t -> Priority.t -> t

  (** La priorité d'une clé dans une file. *)
  val priority : t -> Key.t -> Priority.t

end
