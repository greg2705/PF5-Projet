(**

   Ce module fournit les types et les opérations pour représenter et
   faire évoluer le monde. Les fonctions documentées *peuvent* vous
   servir. Celles qui ne le sont pas ne devraient pas vous servir.

*)

(** Dans monde, il y a ...*)
type kind =
  | Ground of float
  (** des espaces avec plus ou moins de frottements : [Ground f] implique
      que la vitesse est multipliée par [f] lorsque l'on passe par ici.
      [f] est compris entre 0.5 et 1.5 ... *)

  | Hell
  (** et aussi des endroits où le robot ne doit pas s'aventurer sans
      être détruit. *)
[@@deriving yojson]

(** Le monde est défini par:*)
type t = {
    space       : kind Space.t;        (** des polygones de type [kind], *)
    trees       : tree list;           (** des arbres de Böhms,          *)
    microcodes  : left_microcode list; (** des microcodes laissés là,    *)
    teams       : team list;           (** des équipes de robots,        *)
    visibility  : float;               (** une distance où on y voit,    *)
    epoch       : int;                 (** un âge,                       *)
    end_of_time : int;                 (** une durée de vie.             *)
}

(** Un arbre de Böhm a *)
and tree = {
    tree_position : Space.position;    (** une position,                  *)
    branches      : int;               (** et un nombre de branches >= 0. *)
}

(** Les robots peuvent laisser des microcodes sur le sol. Ils contiennent *)
and left_microcode = {
    microcode          : microcode;      (** un message,      *)
    microcode_position : Space.position; (** sa position,     *)
    duration           : Space.duration; (** sa durée de vie. *)
}

(** Un [microcode] est une donnée... *)
and microcode =
  | MicroAtom of int               (** dont les atomes sont des entiers, et *)
  | MicroList of microcode list    (** que l'on peut structurer à sa guise. *)

(** Une équipe...*)
and team = {
    team_identifier : team_identifier; (** a un identifiant unique.          *)
    robots          : robot list;      (** est formée d'une liste de robots. *)
    spaceship       : Space.position;  (** et possède un vaisseau spatial.   *)
}

(** Un λman possède : *)
and robot = {
    robot_identifier      : robot_identifier; (** un identifiant unique     *)
    robot_team_identifier : team_identifier;  (** dans son équipe,          *)
    robot_position        : Space.position;   (** une position,             *)
    robot_spaceship       : Space.position;   (** un vaisseau,              *)
    robot_angle           : Space.angle;      (** une direction,            *)
    robot_speed           : Space.speed;      (** une vitesse,              *)
    robot_energy          : int;              (** une énergie positive,     *)
    robot_score           : int;              (** un score,                 *)
    robot_bonus           : int;              (** un bonus potentiel,       *)
    robot_down            : bool;             (** mais peut être désactivé. *)
}

(** Les identifiants sont de simples entiers. *)
and robot_identifier = int and team_identifier = int
[@@deriving yojson]

(** [load filename] charge le fichier JSON [filename] qui représente un
    monde. *)
val load : string -> t

(** [save filename world] sauvegarde [world] dans le fichier JSON [filename]. *)
val save : string -> t -> unit

(** [tree_at world pos] renvoie un arbre proche de [pos], s'il y en a un. *)
val tree_at : tree list -> Space.position -> tree option

(** [tree_positions trees] renvoie toutes les positions d'une liste d'arbres. *)
val tree_positions : tree list -> Space.position list

(** [update_tree world tree tree'] renvoie [world] où [tree'] remplace
    [tree']. *)
val update_tree : t -> tree -> tree -> t

(** [number_of_branches world] compte le nombre de branches d'arbres de Böhm
    dans [world]. *)
val number_of_branches : t -> int

(** [size_of_microcode m] est la taille d'un message. Plus un message est
    gros et plus il coûte cher à produire en termes d'énergie de robot. *)
val size_of_microcode : microcode -> int

(** [put world m d p] laisse un message [m] à une position [p]. Il
    s'autodétruira dans [d] unités de temps, sauf si on place un autre
    message identique à cette même position. *)
val put : t -> microcode -> Space.duration -> Space.position -> t

type observation =
{
    trees      : tree list;           (** Les arbres de Böhm.                *)
    messages   : left_microcode list; (** Les microcodes à proximité.        *)
    around     : kind Space.t;        (** L'espace visible par le robot.     *)
    speed      : Space.speed;         (** La vitesse courante du robot.      *)
    max_speed  : Space.speed;         (** Vitesse maximale au point courant. *)
    angle      : Space.angle;         (** La direction courante du robot.    *)
    position   : Space.position;      (** La position courante du robot.     *)
    spaceship  : Space.position;      (** Position du vaisseau.              *)
    energy     : int;                 (** L'énergie courante du robot.       *)
    score      : int;                 (** Le score du robot.                 *)
    epoch      : int;
    visibility : float;
    game_over  : bool;
}
[@@deriving yojson]

(** Une observation fournit une vue partielle sur un monde. *)
val world_of_observation : observation -> t

(** Cette fonction produit un robot, qu'il reste à initialiser. *)
val make_robot :
  team_identifier ->
  team_identifier ->
  Space.position -> Space.angle -> robot

(** Cette fonction déplace le robot en respectant la vitesse maximale
   autorisée observée. *)
val move : observation -> robot -> Space.angle -> Space.speed -> robot

(** Désactive un robot. *)
val bury : robot -> robot

(** Teste si un robot est désactivé. *)
val robot_is_dead : robot -> bool

(** [update_team world team team'] produit [world] où [team'] remplace
    [team]. *)
val update_team : t -> team -> team -> t

(** [update_robot world robot robot'] produit [world] où [robot'] remplace
    [robot]. *)
val update_robot : t -> robot -> robot -> t

(** Teste si une position est dans une bouche de l'enfer. *)
val inside_hell : t -> Space.position -> bool

(** Renvoie tous les segments des bouches de l'enfer. *)
val hell_segments : t -> Space.segment list

(** Renvoie le niveau de souffrance à une certaine position. *)
val suffering : t -> Space.position -> float






(** Ces fonctions sont non documentées. Vous n'en avez pas besoin. *)

val forall_robot :
  t -> (t -> robot -> (robot * 'a list) Lwt.t) -> (t * 'a list) Lwt.t

val map_robots : (robot -> robot) -> t -> t

val map_microcodes : (left_microcode -> left_microcode) -> t -> t

val remove_dead_microcodes : t -> t

val space_around : t -> float * float -> kind Space.t

val extends_world : t -> t -> t

val robot_perspective : t -> bool -> robot -> observation
