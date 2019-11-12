type kind =
| Ground of float
  (** Un espace avec plus ou moins de frottements : [Ground f] implique
      que la vitesse est multipliée par [f] lorsque l'on passe par ici.
      [f] est compris entre 0.2 et 5. *)

| Hell
  (** Un endroit où le robot ne doit pas s'aventurer sans être détruit. *)
[@@deriving yojson]

type t = {
    space       : kind Space.t;
    trees       : tree list;
    microcodes  : left_microcode list;
    teams       : team list;
    visibility  : float;
    epoch       : int;
    end_of_time : int;
}

and tree = {
    tree_position : Space.position;
    branches      : int;
}

and left_microcode = {
    microcode          : microcode;
    microcode_position : Space.position;
    duration           : Space.duration;
}

(** Un [microcode] est une donnée... *)
and microcode =
  | MicroAtom of int               (** dont les atomes sont des entiers, et *)
  | MicroList of microcode list    (** que l'on peut structurer à sa guise. *)

and team = {
    team_identifier : team_identifier;
    robots          : robot list;
    spaceship       : Space.position;
}

and robot = {
    robot_identifier      : robot_identifier;
    robot_team_identifier : team_identifier;
    robot_position        : Space.position;
    robot_spaceship       : Space.position;
    robot_angle           : Space.angle;
    robot_speed           : Space.speed;
    robot_energy          : int;
    robot_score           : int;
    robot_bonus           : int;
    robot_down            : bool;
}

and robot_identifier = int

and team_identifier = int
[@@deriving yojson]

let load filename =
  Yojson.Safe.from_file filename |> of_yojson |> function
  | Ok r -> r
  | Error e -> Printf.eprintf "When loading the world:\n%s\n" e; exit 1

let save filename w =
  Yojson.Safe.to_file filename (to_yojson w)

let tree_at trees pos =
  List.find_opt
    (fun tree -> Space.(close tree.tree_position pos 1.))
    trees

let tree_positions trees =
  List.map (fun tree -> tree.tree_position) trees

let update_tree world tree tree' =
  let eq_tree t1 t2 = t1.tree_position = t2.tree_position in
  { world with trees = Ext.List.update eq_tree world.trees tree tree' }

let number_of_branches world =
  List.fold_left (fun a t -> a + t.branches) 0 world.trees

let size_of_microcode m =
  let rec aux accu = function
    | MicroAtom _ -> 1 + accu
    | MicroList l -> aux' accu l
  and aux' accu = function
    | [] -> accu
    | m :: ms -> aux' (aux accu m) ms
  in
  aux 0 m

let put world microcode duration microcode_position =
  let left_microcode = { microcode_position; microcode; duration } in
  let rec aux = function
    | [] -> [left_microcode]
    | m :: ms ->
       if Space.close microcode_position m.microcode_position 1.
          && m.microcode = microcode
       then
         { m with duration = Space.extends_duration m.duration duration } :: ms
       else
         m :: aux ms
  in
  { world with microcodes = aux world.microcodes }

type observation =
{
    trees      : tree list;           (** Les arbres de Böhm.                *)
    messages   : left_microcode list; (** Les microcodes à proximité.        *)
    around     : kind Space.t;       (** L'espace visible par le robot.     *)
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

let world_of_observation { trees; around; epoch; visibility; _ } =
  {
    trees;
    space = around;
    teams = [];
    microcodes = [];
    epoch;
    end_of_time = max_int;
    visibility
  }

let make_robot
      robot_identifier robot_team_identifier robot_position
      robot_angle
=
{
  robot_identifier; robot_team_identifier; robot_position; robot_angle;
  robot_energy = 0; robot_score = 0; robot_bonus = 0;
  robot_speed = Space.speed_of_float 1.;
  robot_down = false; robot_spaceship = robot_position
}

let robot_position robot =
  robot.robot_position

let robot_identifier robot =
  robot.robot_identifier

let robot_team_identifier robot =
  robot.robot_team_identifier

let change_speed observation robot robot_speed =
  let robot_speed = min robot_speed observation.max_speed in
  { robot with robot_speed }

let move observation robot robot_angle robot_speed =
  { (change_speed observation robot robot_speed) with robot_angle }

let bury robot =
  { robot with robot_down = true }

let robot_is_dead robot =
  robot.robot_down

let update_team world team team' =
  let eq_team t1 t2 = t1.team_identifier = t2.team_identifier in
  { world with teams = Ext.List.update eq_team world.teams team team' }

let update_robot world robot robot' =
  let team =
    List.find
      (fun t -> t.team_identifier = robot.robot_team_identifier)
      world.teams
  in
  let eq_robot r1 r2 = r1.robot_identifier = r2.robot_identifier in
  update_team world team
    { team with robots = Ext.List.update eq_robot team.robots robot robot' }

let forall_robot world f =
  let%lwt (teams, interactions) =
    Ext.Lwt.List.foldmap (fun team interactions ->
        let%lwt (robots, interactions) =
          Ext.Lwt.List.foldmap (fun robot interactions ->
              let%lwt (robot, new_interactions) = f world robot in
              Lwt.return (robot, new_interactions @ interactions)
          ) interactions team.robots
        in
        Lwt.return ({ team with robots }, interactions)
      ) [] world.teams
  in
  Lwt.return ({ world with teams }, interactions)

let map_robots f world =
  let teams =
    world.teams
    |> List.map (fun team -> { team with robots = List.map f team.robots })
  in
  { world with teams }

let map_microcodes f world =
  { world with microcodes = List.map f world.microcodes }

let remove_dead_microcodes world =
  let microcodes =
    List.filter (fun m -> not (Space.is_zero m.duration)) world.microcodes
  in
  { world with microcodes }

let inside_hell world position =
  Space.inside position (( = ) Hell) world.space = Some Hell

let hell world =
  Space.polygons world.space (( = ) Hell)

let space_around (world : t) (x, y) =
  let s = world.visibility /. 2. in
  let b = ((x -. s, y -. s), (x +. s, y +. s)) in
  Space.space_inside b world.space

let extends_world w1 w2 =
  { w1 with space = Space.extends_space w1.space w2.space }

let robot_perspective (world : t) game_over robot =
  let pos = robot.robot_position in
  let messages =
    List.filter (fun p ->
      Space.close pos p.microcode_position world.visibility
    ) world.microcodes
  in
  {
    trees = world.trees;
    around = space_around world pos;
    messages;
    speed = robot.robot_speed;
    max_speed = Space.speed_of_float 2.0; (* FIXME *)
    angle = robot.robot_angle;
    position = robot.robot_position; (* FIXME *)
    spaceship = robot.robot_spaceship;
    energy = robot.robot_energy;
    score = robot.robot_score;
    epoch = world.epoch;
    visibility = world.visibility;
    game_over
}

let hell_segments world = List.(
  Space.polygons world.space (( = ) Hell)
  |> map Space.polygon_segments
  |> flatten
)

let suffering world position =
  let is_ground = function Ground _ -> true | _ -> false in
  match Space.inside position is_ground world.space with
  | Some (Ground f) -> f
  | _ -> 1.
