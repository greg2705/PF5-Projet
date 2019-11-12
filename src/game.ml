(**

   Ce module implémente les règles du jeu sous la forme d'une boucle
   qui régit les interactions entre le monde et les joueurs.

*)

open Lwt
open Decision
open World
open Printf

(**

    La boucle de jeu utilise s'organise autour de la fonction
   [animate].

   À chaque étape de jeu, tant que la partie n'est pas terminée, on
   communique avec les clients pour obtenir leurs réactions aux
   nouvelles observations qu'on leur transmet. On interprète ensuite
   leurs réactions qui sont des interactions avec le monde. Puis, on
   déplace les robots. Enfin, on passe à l'étape suivante.

   Parce que les clients réagissent en parallèle, on utilise la
   bibliothèque Lwt qui permet de décrire des "promesses",
   c'est-à-dire des calculs qui s'exécutent de façon asynchrones.

*)
let loop visualize slowdown world clients =
  (**

      Associe une équipe à chaque client.

  *)
  let client_of_team =
    Ext.Int.check_equal (List.length clients) (List.length world.teams) @@
      eprintf "There are %d client(s) and %d expected team(s)\n";
    let clients = Array.of_list clients in
    fun team_identifier -> clients.(team_identifier)
  in

  (**

      La fonction animate est décrite plus haut.

  *)
  let rec animate world =
    let over = game_over world in
    if visualize then Visualizer.show world;
    let%lwt world', interactions = World.forall_robot world (act over) in
    let%lwt world' = interact interactions world' in
    let%lwt world' = move world' in
    Lwt_unix.sleep slowdown >>= fun () ->
    if over then Lwt.return world' else animate (next_epoch world')

  and next_epoch world =
    { (world : World.t) with epoch = world.epoch + 1 }
    |> map_microcodes less_time_for_microcode
    |> remove_dead_microcodes
    |> map_robots more_energy_for_robot

  and less_time_for_microcode m =
    { m with duration = Space.decr_duration m.duration }

  and more_energy_for_robot r =
    { r with robot_energy = r.robot_energy + 1 }

  and act over world robot =
    if robot.robot_down && not over then Lwt.return (robot, []) else
    let observation = World.robot_perspective world over robot in
    let team_id     = robot.robot_team_identifier in
    let robot_id    = robot.robot_identifier in
    client_of_team team_id robot_id observation >>= function
    | Move (a, v) ->
       let robot = World.move observation robot a v in
       Lwt.return (robot, [])
    | Wait ->
       Lwt.return (robot, [])
    | Die reason ->
       obituary robot reason;
       Lwt.return (World.bury robot, [])
    | (ChopTree | Put (_, _)) as interaction ->
       Lwt.return (robot, [(robot, interaction)])

  and move world =
    Lwt.return (World.map_robots move_robot world)

  and close_enough p1 p2 =
    Space.close p1 p2 1.

  and obituary robot reason =
    eprintf "robot %d/%d died by %s.\n%!"
      robot.robot_team_identifier robot.robot_identifier reason;

  and move_robot r =
    let speed =
      Space.scale_speed (suffering world r.robot_position) r.robot_speed
    in
    let pos = Space.move r.robot_position r.robot_angle speed in
    let r = if close_enough pos r.robot_spaceship then robot_bonus r else r in
    if inside_hell world pos && not (robot_is_dead r) then (
      obituary r "hell";
      World.bury r
    ) else { r with robot_position = pos }

  and robot_bonus r =
    { r with robot_score = r.robot_score + r.robot_bonus; robot_bonus = 0 }

  and interact interactions world =
    (** Pour des raisons d'équité entre les clients, on mélange les interactions
        avant de les interpréter. *)
    Ext.List.shuffle interactions
    |> List.fold_left (fun world (robot, i) ->
     match i with
     | ChopTree -> chop_tree world robot
     | Put (microcode, duration) -> put world microcode duration robot
     | _ -> assert false
     ) world
    |> Lwt.return

  and chop_tree world robot =
    match World.tree_at world.trees robot.robot_position with
    | None ->
       world
    | Some tree ->
       let tree' = { tree with branches = tree.branches - 1 } in
       let world = update_tree world tree tree' in
       let robot_score = robot.robot_score + 1 in
       let robot_bonus = robot.robot_bonus + 1 in
       let robot' = { robot with robot_score; robot_bonus } in
       update_robot world robot robot'

  and put world microcode duration robot =
    let cost = World.size_of_microcode microcode in
    if cost < robot.robot_energy then (
      let robot' = { robot with robot_energy = robot.robot_energy - cost } in
      let world = update_robot world robot robot' in
      World.put world microcode duration robot.robot_position
    ) else
      world

  and game_over world =
    limit_reached world
    || (no_more_tree world && one_team_in_spaceship world)
    || all_dead world

  and limit_reached world =
    world.epoch >= world.end_of_time

  and no_more_tree world =
    List.for_all (fun t -> t.branches = 0) world.trees

  and one_team_in_spaceship (world : World.t) =
    List.exists (fun (team : World.team) -> List.for_all (fun robot ->
       close_enough robot.robot_position team.spaceship
       || robot.robot_down)
       team.robots) world.teams

  and all_dead world =
    List.for_all (fun (team : World.team) ->
        List.for_all (fun robot -> robot.robot_down) team.robots) world.teams

  in
  let team_score (team : World.team) =
    List.fold_left ( + ) 0 (List.map (fun r -> r.robot_score) team.robots)
  in
  let show_score (world : World.t) = Lwt.return List.(
    printf "Epoch : %d\n" world.epoch;
    let scores = map (fun t -> (t.team_identifier, team_score t)) world.teams in
    iter (fun (id, s) -> printf "Team %d : %d\n" id s) scores;
    printf "Winner: %d\n" (
       Ext.List.best (fun (_, s) -> s) scores |> fun ((id, _), _) -> id
    ))
  in
  animate world >>= show_score
