(**

   Ce module fournit un générateur aléatoire de mondes. Il est peu
   sophistiqué, c'est à vous de l'améliorer pour briller dans la tâche
   5 du projet.

*)

open World

let default_end_of_time =
  100000

let default_visibility =
  100.

let initial = {
    space       = Space.empty;
    trees       = [];
    teams       = [];
    epoch       = 0;
    microcodes  = [];
    end_of_time = default_end_of_time;
    visibility  = default_visibility;
}

let simple_world nb_players nb_robots_per_team max_hell max_ground max_tree =
  let width  = Ext.Float.random_in_range 500. 1000. in
  let height = Ext.Float.random_in_range 500. 1000. in
  let random_angle () = Space.angle_of_float (Random.float (2. *. Float.pi)) in
  let random_position space =
    let rec aux n =
      if n = 0 then failwith "Impossible to find a free random position.";
      let p = Ext.Float.(
        (random_in_range 0. width, random_in_range 0. height)
      ) in
      if Space.inside p (fun _ -> true) space <> None then aux (n - 1) else p
    in
    aux 10
  in
  let random_size () =
    Ext.Float.random_in_range 100. (width /. 10.)
  in
  let rec make_hell space =
    let s = Space.square (random_position space) (random_size ()) Hell in
    if Space.polygon_overlaps s space (( = ) Hell) then make_hell space else
    Space.(blend space (polygon s))
  in
  let make_ground space =
    let ratio = Ground (Ext.Float.random_in_range 0.5 1.5) in
    let s = Space.square (random_position space) (random_size ()) ratio in
    Space.(blend space (polygon s))
  in
  let make_tree space _ =
    let tree_position = random_position space in
    let branches = Ext.Int.random_in_range 2 20 in
    { tree_position; branches }
  in
  let make_team space team_identifier =
    let spaceship = random_position space in
    let make_robot id =
      make_robot id team_identifier spaceship (random_angle ())
    in
    { team_identifier; spaceship;
      robots = Ext.Fun.repeat nb_robots_per_team make_robot }
  in

  let nb_hell = Ext.Int.random_in_range 1 max_hell in
  let space = Ext.Fun.iter nb_hell make_hell Space.empty in
  let nb_grounds = Ext.Int.random_in_range 1 max_ground in
  let space = Ext.Fun.iter nb_grounds make_ground space in
  let nb_trees = Ext.Int.random_in_range 1 max_tree in
  let trees = Ext.Fun.repeat nb_trees (make_tree space) in
  let teams = Ext.Fun.repeat nb_players (make_team space) in
  { initial with space; trees; teams }

let output world =
  to_yojson world |> Yojson.Safe.pretty_to_string |> output_string stdout

let generate
      visualize nb_players nb_robots_per_teams
      max_hell max_ground max_tree =
  let world =
    simple_world nb_players nb_robots_per_teams max_hell max_ground max_tree
  in
  if visualize then (Visualizer.(show world; pause ()));
  output world

let visualization_flag = Cmdliner.(Arg.(
  value & flag & info ["v"]
  ~doc:"Visualize the generated world")
)

let nb_players = Cmdliner.(Arg.(
  value & opt int 1 & info ["p"]
  ~docv:"NBPLAYERS"
  ~doc:"Handle $(docv) players."
))

let nb_robots = Cmdliner.(Arg.(
  value & opt int 1 & info ["r"]
  ~docv:"NBROBOTS"
  ~doc:"Handle $(docv) robots per player."
))

let max_hell = Cmdliner.(Arg.(
  value & opt int 1 & info ["h"]
  ~docv:"MAXHELL"
  ~doc:"Use a maximum of $(docv) hell blocks."
))

let max_ground = Cmdliner.(Arg.(
  value & opt int 1 & info ["g"]
  ~docv:"MAXGROUND"
  ~doc:"Use a maximum of $(docv) ground blocks."
))

let max_tree = Cmdliner.(Arg.(
  value & opt int 1 & info ["t"]
  ~docv:"MAXTREE"
  ~doc:"Use a maximum of $(docv) trees."
))

let cmd = Cmdliner.(
  let doc   = "Generate a random world." in
  let exits = Term.default_exits in
  Term.(const generate $ visualization_flag
        $ nb_players $ nb_robots
        $ max_hell $ max_ground $ max_tree),
  Term.info "generate" ~doc ~exits
)
