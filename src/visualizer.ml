(**

   Ce module implémente une fenêtre de visualisation artisanale.

   On peut se déplacer à l'aide des touches t, g, h, et f.

   On peut zoomer à l'aide des touches r et e.

   On peut quitter à l'aide de la touche q.

*)

open Lwt
open World
open Space
open Graphics

let res_x = ref 1024

let res_y = ref 1024

let moved = ref false

let view_x = ref 0.

let view_y = ref 0.

let zoom = ref 1.

let state : World.t option ref = ref None

let clip d x = if x < 0 then 0 else if x > d then d else x

let clip_x = clip !res_x

let clip_y = clip !res_y

let t_x x = (int_of_float ((x -. !view_x) *. !zoom))

let t_y y = (int_of_float ((y -. !view_y) *. !zoom))

let t_ p = (t_x (x_ p), t_y (y_ p))

let scale size = max 1 (int_of_float (size *. !zoom))

let color_of_thing = function
  | World.Hell ->
     red
  | World.Ground f ->
     let c = int_of_float (float_of_int 255 *. f) in
     rgb c c c

let display_polygon p =
  set_color (color_of_thing (Space.content p));
  fill_poly (List.map t_ (Space.vertices p) |> Array.of_list)

let ground_first t1 t2 =
  match content t1, content t2 with
  | Hell, Hell | Ground _, Ground _ -> 0
  | Hell, _ -> 1
  | Ground _, _ -> -1

let display_space space =
     Space.polygons space (fun _ -> true)
  |> List.sort ground_first
  |> List.iter display_polygon

let display_tree { tree_position; branches } =
  set_color (rgb 200 200 (min 255 (128 + branches)));
  fill_circle (t_x (x_ tree_position)) (t_y (y_ tree_position)) (scale 5.)

let screen_bounding_box positions =
  List.fold_left (fun ((x0, y0), (x1, y1)) (x, y) ->
      ((min x x0, min y y0), (max x x1, max y y1))
    ) ((max_int, max_int), (min_int, min_int)) positions

let backgrounds = ref []
let save_background ((x0, y0), (x1, y1)) =
  let h = y1 - y0 + 1 and w = x1 - x0 + 1 in
  let i = get_image x0 y0 w h in
  backgrounds := (x0, y0, i) :: !backgrounds

let restore_background () =
  List.iter (fun (x, y, i) -> draw_image i x y) !backgrounds;
  backgrounds := []

let display_robot team_color robot =
  if not robot.robot_down then begin
    let pos = robot.robot_position and a = float_of_angle robot.robot_angle in
    let al = a +. Float.pi /. 2. and ar = a -. Float.pi /. 2. in
    let h = 10. and w = 3. in
    let head = (x_ pos +. h *. cos a, y_ pos +. h *. sin a) in
    let left = (x_ pos +. w *. cos al, y_ pos +. w *. sin al) in
    let right = (x_ pos +. w *. cos ar, y_ pos +. w *. sin ar) in
    set_color team_color;
    save_background (screen_bounding_box [ t_ head; t_ left; t_ right ]);
    fill_poly [| t_ head; t_ left; t_ right |]
  end

let display_microcode m =
  let pos = m.microcode_position in
  let l = Float.log (float_of_int (Space.int_of_duration m.duration)) in
  let d = max 1. (min 10. l) in
  let w = scale d in
  save_background (screen_bounding_box [
                       (t_x (x_ pos) - w, t_y (y_ pos) - w);
                       (t_x (x_ pos) + w, t_y (y_ pos) + w);
    ]);
  set_color black;
  fill_circle (t_x (x_ pos)) (t_y (y_ pos)) (scale d)

let display_spaceship team_color pos =
  let d = 5. and d' = 10. in
  set_color team_color;
  fill_circle (t_x (x_ pos)) (t_y (y_ pos)) (scale d);
  draw_rect (t_x (x_ pos -. d)) (t_y (y_ pos -. d)) (scale d') (scale d')

let random_color =
  let m = 100 in
  let c = Array.init m Random.(fun _ -> rgb (int 240) (int 240) (int 240)) in
  fun i -> c.(i mod m)

let display_team team =
  let team_color = random_color team.team_identifier in
  display_spaceship team_color team.spaceship;
  List.iter (display_robot team_color) team.robots

let display world =
  clear_graph ();
  display_space world.space;
  List.iter display_tree world.trees;
  List.iter display_team world.teams

let focus_on_box ((x0, y0), (x1, y1)) =
  let swidth = x1 -. x0 and sheight = y1 -. y0 in
  let r1 = (float_of_int !res_x) /. swidth
  and r2 = (float_of_int !res_y) /. sheight in
  view_x := x0;
  view_y := y0;
  zoom := min 3. (min r1 r2)

let control_loop () =
  Lwt_preemptive.detach (fun () ->
  let rec aux () =
  let status = wait_next_event [Key_pressed; Poll] in
  if not status.keypressed then Lwt_unix.sleep 0.01 >>= aux else (
    let status = wait_next_event [Key_pressed] in
      let d = 5. *. !zoom and dzoom = 0.05 in
      begin match status.key with
      | 'q' -> exit 1
      | 'e' -> zoom := min 10. (!zoom +. dzoom)
      | 'r' -> zoom := max 0.1 (!zoom -. dzoom)
      | 't' -> view_y := !view_y -. d
      | 'g' -> view_y := !view_y +. d
      | 'h' -> view_x := !view_x -. d
      | 'f' -> view_x := !view_x +. d
      | _ -> ()
      end;
      moved := true; Lwt_unix.sleep 0.01 >>= aux)
  in Lwt.async aux) ()

let bounding_box_world world =
  bounding_box_union
    (bounding_box world.space)
    (bounding_box_of_positions (
         List.map (fun t -> t.tree_position) world.trees
       @ List.map (fun (t : team) -> t.spaceship) world.teams))

let initialize new_world =
  let bbox = bounding_box_world new_world in
  open_graph (Printf.sprintf " %dx%d" !res_x !res_y);
  state := Some new_world;
  focus_on_box bbox;
  display new_world;
  Lwt.async control_loop

let pause () = ignore (wait_next_event [Key_pressed])

let show_info world =
  set_color white;
  fill_rect 0 0 100 50;
  moveto 10 10;
  set_color black;
  draw_string (Printf.sprintf "#trees : %d\n" (World.number_of_branches world))

let update force _old_world new_world =
  if not force then auto_synchronize false;
  if !moved || force then (
    display new_world;
    moved := false
  ) else (
    restore_background ();
    List.iter display_team new_world.teams;
    List.iter display_microcode new_world.microcodes;
    state := Some new_world
  );
  show_info new_world;
  if not force then synchronize ()

let show ?(force = false) new_world =
  match !state with
  | None -> initialize new_world
  | Some old_world -> update force old_world new_world

let show_node (x, y) =
  set_color blue;
  fill_circle (t_x x) (t_y y) 5

let show_edge (x0, y0) (x1, y1) =
  set_color blue;
  set_line_width 2;
  moveto (t_x x0) (t_y y0);
  lineto (t_x x1) (t_y y1)

let show_graph g =
  auto_synchronize false;
  let ns = Graph.nodes g in List.(
  iter show_node ns;
  iter (fun n -> iter (fun (_, n', _) -> show_edge n n') (Graph.out g n)) ns;
  synchronize ();
)

let show_milestone (x, y) =
  set_color yellow;
  fill_circle (t_x x) (t_y y) 5

let show_path path =
  auto_synchronize false;
  List.iter show_milestone path;
  synchronize ()
