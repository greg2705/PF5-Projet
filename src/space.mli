(**

   Ce module fournit des opérations pour travailler sur un plan
   composé de polygones.

*)

(** Nous travaillons dans le plan. *)
type position = float * float
[@@deriving yojson]

(** [string_of_position p] renvoie une représentation textuelle de [p]. *)
val string_of_position : position -> string

(** [x_ p] est l'abscisse de [p]. *)
val x_ : position -> float

(** [x_ p] est l'ordonnée de [p]. *)
val y_ : position -> float

(** On manipulera des distances explicitement... *)
type distance = Distance of float

(** en les calculant grâce à dist2... *)
val dist2 : position -> position -> distance

(** mais aussi implicitement pour déterminer si deux points sont proches. *)
val close : position -> position -> float -> bool


(** Les polygones ont un contenu de type ['a]. *)
type 'a polygon
[@@deriving yojson]

(** On définit un polygone par ses sommets et son contenu. *)
val make : position list -> 'a -> 'a polygon

(** [square p s] est un carré en [p] et de côté [s]. *)
val square : position -> float -> 'a -> 'a polygon

(** [rectangle p w h] est un rectangle en [p], de largeur [w] et de
    hauteur [h].  *)
val rectangle : position -> float -> float -> 'a -> 'a polygon

(** On peut toujours observer les sommets d'un polygone. *)
val vertices : 'a polygon -> position list

(** On peut aussi observer le contenu d'un polygone. *)
val content : 'a polygon -> 'a

(** Voici le type du plan contenant des polygones de contenu ['a]. *)
type 'a t
[@@deriving yojson]

(** Voici le plan sans polygone. *)
val empty : 'a t

(** Voici le plan avec un polygone. *)
val polygon : 'a polygon -> 'a t

(** On peut superposer deux plans. *)
val blend : 'a t -> 'a t -> 'a t

(** [polygons space p] renvoie tous les polygones de [space] dont le
    contenu vérifie [p]. *)
val polygons : 'a t -> ('a -> bool) -> 'a polygon list

(** [inside space p] renvoie le contenu d'un polygone de [space] qui
    contient [p]. *)
val inside : position -> ('a -> bool) -> 'a t -> 'a option

(** Un segment est défini par deux points. *)
type segment = position * position

(** On peut observer les segments d'un polygone. *)
val polygon_segments : 'a polygon -> segment list

(** [segment_intersects s1 s2] détermine si deux segments se croisent. *)
val segment_intersects : segment -> segment -> bool

(** Une boîte englobante est définie par son coin de coordonnées
    minimales et son coin de coordonnées maximales. *)
type bounding_box = position * position

(** L'ensemble des polygones de [s] est dans la boîte [bounding_box s]. *)
val bounding_box : 'a t -> bounding_box

(** On peut calculer la boîte englobante de toutes listes de positions.*)
val bounding_box_of_positions : position list -> bounding_box

(** On peut aussi faire l'union de deux boîtes. *)
val bounding_box_union : bounding_box -> bounding_box -> bounding_box

(** [polygon_overlaps poly space p] renvoie [true] si et seulement si
    [poly] chevauche un polygone de [space] de contenu vérifiant [p]. *)
val polygon_overlaps : 'a polygon -> 'a t -> ('a -> bool) -> bool

(** On peut extraire l'ensemble des polygones d'un plan qui sont inclus dans
    une boîte. *)
val space_inside : bounding_box -> 'a t -> 'a t

(** On peut étendre un ensemble de polygones par un autre ensemble de
    polygones. *)
val extends_space : 'a t -> 'a t -> 'a t

(** La notion de durée... *)
type duration
[@@deriving yojson]

(** est représentable par un entier (positif). *)
val duration_of_int : int -> duration
val int_of_duration : duration -> int

(** On peut effectuer un décompte... *)
val decr_duration : duration -> duration

(** ...et tester lorsqu'il atteint zéro. *)
val is_zero : duration -> bool

(** On peut étendre une durée d'une autre durée. *)
val extends_duration : duration -> duration -> duration

(** Un angle est un nombre flottant représentant un angle en radian. *)
type angle
[@@deriving yojson]

val angle_of_float : float -> angle
val float_of_angle : angle -> float

(** Une vitesse est un nombre flottant toujours positif qui correspond
    à la distance parcourue par unité de temps. *)
type speed
[@@deriving yojson]

val speed_of_float : float -> speed
val float_of_speed : speed -> float

(** Les vitesses peuvent être modifiées par multiplication par un scalaire. *)
val scale_speed : float -> speed -> speed

(** [move position a s] est la position d'un objet se déplaçant
    suivant un certain angle [a] et une certaine vitesse [s]. *)
val move : position -> angle -> speed -> position
