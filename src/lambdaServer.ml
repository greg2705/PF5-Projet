(**

   Ce module implémente le serveur de jeu.

*)

let run world_filename client_commands visualize slowdown =
  let world = World.load world_filename in
  let clients = List.mapi Communication.server client_commands in
  Lwt_main.run (Game.loop visualize slowdown world clients)

open Cmdliner

let visualization_flag =
  Arg.(value & flag & info ["v"] ~doc:"Visualize the game graphically")

let clients =
  Arg.(value
       & (pos_all string) []
       & info [] ~docv:"CMD" ~doc:"The commands to execute clients.")

let world =
  Arg.(value
       & opt file "world.json"
       & info ["w"] ~docv:"WORLD" ~doc:"Load $(WORLD) description.")

let slowdown =
  Arg.(value & opt float 0. & info ["s"]
       ~docv:"DURATION"
       ~doc:"Wait for $(docv) seconds between each turn.")

let cmd =
  let doc = "Play a game with lambda-men." in
  let exits = Term.default_exits in
  Term.(const run $ world $ clients $ visualization_flag $ slowdown),
  Term.info "server" ~doc ~exits
