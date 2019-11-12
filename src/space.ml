(**

   Ce module fournit une implémentation naïve d'un plan composé de
   polygones.

*)
type position = float * float
[@@deriving yojson]

let string_of_position (x, y) = Printf.sprintf "(%f,%f)" x y

let sq x = x *. x

type distance = Distance of float

let dist2 (x, y) (x', y') = Distance (sqrt (sq (x' -. x) +. sq (y' -. y)))

let close p1 p2 m =
  let Distance d = dist2 p1 p2 in
  d < m

let maximal_size = 1000000.

let x_ = fst

let y_ = snd

let clip_position ((x0, y0), (x1, y1)) (x, y) =
  (max x0 (min x1 x), max y0 (min y1 y))

type 'a polygon = {
   vertices : position list;
   content  : 'a
}
[@@deriving yojson]

let make vertices content = { vertices; content }

let vertices p = p.vertices

let content p = p.content

let rectangle p s u =
  let d = s /. 2. and d' = u /. 2. in
  make [
      (x_ p -. d, y_ p -. d');
      (x_ p +. d, y_ p -. d');
      (x_ p +. d, y_ p +. d');
      (x_ p -. d, y_ p +. d');
    ]

let square p s = rectangle p s s

type segment = position * position

let polygon_segments poly =
  Ext.List.fold_between
    (fun s a b -> (a, b) :: s) []
    (poly.vertices @ [List.hd poly.vertices])

(** [inside_segment p q r] suppose que [p], [q] et [r] sont sur la
    même droite et renvoie [true] si [q] se situe entre [p] et [r]. *)
let inside_segment p q r =
  x_ q <= max (x_ p) (x_ r) &&
  x_ q >= min (x_ p) (x_ r) &&
  y_ q <= max (y_ p) (y_ r) &&
  y_ q >= min (y_ p) (y_ r)

let%test _ = inside_segment (-1.,  0.) (0., 0.) (1.,  0.)
let%test _ = inside_segment ( 0., -1.) (0., 0.) (0.,  1.)
let%test _ = inside_segment (-1., -1.) (0., 0.) (1.,  1.)
let%test _ = inside_segment (-1.,  1.) (0., 0.) (1., -1.)
let%test _ = not (inside_segment (-1.,  0.) (-2.,  0.) (1.,  0.))
let%test _ = not (inside_segment ( 0., -1.) ( 0.,  2.) (0.,  1.))
let%test _ = not (inside_segment (-1., -1.) ( 2.,  2.) (1.,  1.))
let%test _ = not (inside_segment (-1.,  1.) ( 2., -2.) (1., -1.))

type orientation = Colinear | Direct | Indirect

(** [orientation p q r] détermine si l'orientation des vecteurs
    PQ et QR. *)
let orientation p q r =
  let product =
    (y_ q -. y_ p) *. (x_ r -. x_ q) -. (x_ q -. x_ p) *. (y_ r -. y_ q)
  in
  match compare product 0. with
    |  0 -> Colinear
    | -1 -> Direct
    |  1 -> Indirect
    |  _ -> assert false (* by compare. *)

let%test _ = (orientation (1., 2.) (2., 2.) (3., 2.)   = Colinear)
let%test _ = (orientation (-1., -1.) (1., 1.) (3., 3.) = Colinear)
let%test _ = (orientation (1., 1.) (2., 2.) (1., 2.)   = Direct)
let%test _ = (orientation (0., 0.) (1., -1.) (2., 0.)  = Direct)
let%test _ = (orientation (0., 0.) (1.,  1.) (2., 0.)  = Indirect)
let%test _ = (orientation (1., 1.) (-2., 2.) (1., 2.)  = Indirect)

(** [segment_intersects (a, b) (c, d)] détermine si le segment ab et
   le segment cd se croisent. *)
let segment_intersects (p1, q1) (p2, q2) =
  match
    orientation p1 q1 p2,
    orientation p1 q1 q2,
    orientation p2 q2 p1,
    orientation p2 q2 q1
  with
  | o1, o2, o3, o4    when (o1 <> o2 && o3 <> o4)  -> true
  | Colinear, _, _, _ when inside_segment p1 p2 q1 -> true
  | _, Colinear, _, _ when inside_segment p1 q2 q1 -> true
  | _, _, Colinear, _ when inside_segment p2 p1 q2 -> true
  | _, _, _, Colinear when inside_segment p2 q1 q2 -> true
  | _, _, _, _ -> false


let s1 = ((-1., -1.), (1., 1.))   and s2 = ((1., -1.), (-1., 1.))
and s3 = ((-2., -2.), (-1., -1.)) and s4 = ((-2., -2.), (2., -2.))
and s5 = ((1., 1.), (-2., -1.))   and s6 = ((-5., 1.), (3., 3.))
let%test _ =     (segment_intersects s1 s2)
let%test _ =     (segment_intersects s1 s3)
let%test _ =     (segment_intersects s2 s1)
let%test _ =     (segment_intersects s3 s1)
let%test _ =     (segment_intersects s1 s5)
let%test _ = not (segment_intersects s1 s4)
let%test _ = not (segment_intersects s4 s1)
let%test _ = not (segment_intersects s4 s6)
let%test _ = not (segment_intersects s4 s5)

let inside_polygon p { vertices; _ } =
  let p_right = (maximal_size, y_ p) in
  let s = (p, p_right) in
  let exception InSegment in
  try
    Ext.List.fold_between (fun count a b ->
        if segment_intersects s (a, b) then (
          if inside_segment a p b then raise InSegment;
          count + 1
        ) else count) 0 (vertices @ [List.hd vertices])
    |> fun count -> count mod 2 = 1
  with InSegment ->
    false

let%test _ =      inside_polygon (0., 0.) (square (0., 0.) 1. ())
let%test _ =      inside_polygon (1., 0.) (square (0., 0.) 2. ())
let%test _ = not (inside_polygon (2., 0.) (square (0., 0.) 2. ()))

type 'a t =
  | Empty
  | Single of 'a polygon
  | Blend  of 'a t * 'a t
[@@deriving yojson]

let empty = Empty

let polygon g = Single g

let polygons p f =
  let rec aux accu = function
    | Single a -> a :: accu
    | Empty -> accu
    | Blend (p1, p2) -> aux (aux accu p1) p2
  in
  aux [] p |> List.filter (fun p -> f p.content)

let blend p1 p2 =
  match p1, p2 with
  | Empty, p | p, Empty -> p
  | _ -> Blend (p1, p2)

let blends ps =
  Ext.List.reduce blend Empty ps

let inside (type a) p pred space =
  let exception Found of a in
  let rec aux = function
    | Empty ->
       ()
    | Single poly ->
       if inside_polygon p poly && pred poly.content then
         raise (Found poly.content)
    | Blend (p1, p2) -> aux p1; aux p2
  in
  try
    aux space; None
  with Found x -> Some x

type bounding_box = position * position

let empty_bounding_box =
  ((Float.max_float, Float.max_float), (Float.min_float, Float.min_float))

let bounding_box_of_positions ps =
  List.fold_left (fun ((x0, y0), (x1, y1)) (x, y) ->
      ((min x0 x, min y0 y), (max x1 x, max y1 y)))
    empty_bounding_box
    ps

let bounding_box_of_polygon poly =
  bounding_box_of_positions poly.vertices

let bounding_box_union ((x0, y0), (x1, y1)) ((x0', y0'), (x1', y1')) =
  ((min x0 x0', min y0 y0'), (max x1 x1', max y1 y1'))

let bounding_box space =
  let rec traverse box = function
    | Empty -> box
    | Single poly -> bounding_box_union box (bounding_box_of_polygon poly)
    | Blend (p1, p2) -> traverse (traverse box p1) p2
  in
  traverse empty_bounding_box space

let bounding_box_width ((x0, _), (x1, _)) = x1 -. x0

let bounding_box_height ((_, y0), (_, y1)) = y1 -. y0

let bounding_box_intersect ((x0, y0), (x1, y1)) ((x0', y0'), (x1', y1')) =
  let is_on_the_left_side = x0 > x1' || x0' > x1 in
  let is_above_the_other = y0 > y1' || y0' > y1 in
  not (is_on_the_left_side || is_above_the_other)

let b1 = ((0., 0.), (1., 1.)) and b2 = ((-1., -1.), (0., 0.))
and b3 = ((-1., -1.), (-0.5, -0.5)) and b4 = ((-1., -1.), (1., 1.))
let%test _ = bounding_box_intersect b1 b2
let%test _ = bounding_box_intersect b1 b4
let%test _ = not (bounding_box_intersect b1 b3)

let polygon_intersect p1 p2 =
  bounding_box_intersect
    (bounding_box_of_polygon p1)
    (bounding_box_of_polygon p2)

let clip_polygon bb p =
  { p with vertices = List.map (clip_position bb) p.vertices }

let polygons_inside b space =
  let rec aux ps = function
    | Empty ->
       ps
    | Single p ->
       let bb = bounding_box_of_polygon p in
       if bounding_box_intersect bb b then clip_polygon bb p :: ps else ps
    | Blend (p1, p2) ->
       aux (aux ps p1) p2
  in
  aux [] space

let space_inside b space =
  polygons_inside b space |> List.map polygon |> blends

let included_bounding_box ((x0, y0), (x1, y1)) ((x0', y0'), (x1', y1')) =
  x0 >= x0' && x1 <= x1' && y0 >= y0' && y1 <= y1'

type covering = Included | Covered | Nothing

let cover p p' =
  let b = bounding_box_of_polygon p and b' = bounding_box_of_polygon p' in
  if included_bounding_box b b' then Included
  else if included_bounding_box b' b then Covered
  else Nothing

let overlap p space =
  let hidden = ref false in
  let rec aux = function
  | Empty -> Empty
  | Single p' ->
     begin match cover p p' with
     | Included -> hidden := true; Single p'
     | Covered  -> Single p'
     | Nothing  -> Single p'
     end
  | Blend (p1, p2) ->
     blend (aux p1) (aux p2)
  in
  let space' = aux space in
  blend (if !hidden then Empty else Single p) space'

let polygon_overlaps p space f =
  List.exists (fun p' -> polygon_intersect p p') (polygons space f)

let extends_space space1 space2 =
  let rec aux space = function
    | Empty -> space
    | Single p -> overlap p space
    | Blend (p1, p2) -> aux (aux space p1) p2
  in
  aux space1 space2

type duration = int
[@@deriving yojson]

let duration_of_int, int_of_duration = Ext.Iso.id

let decr_duration d = max 0 (d - 1)

let is_zero d = (d = 0)

let extends_duration d1 d2 = d1 + d2

type angle = float
[@@deriving yojson]

let angle_of_float, float_of_angle = Ext.Iso.id

type speed = float
[@@deriving yojson]

let speed_of_float, float_of_speed = Ext.Iso.id

let scale_speed k s = k *. s

let move p a s =
  (x_ p +. s *. cos a, y_ p +. s *. sin a)
