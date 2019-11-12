(**

    Ce module fournit le client qui interagit avec le serveur.

*)
(** Le client s'occupe de distribuer les messages aux robots. *)
let run visualize max_duration nb =

  (** Chaque robot a une mémoire. *)
  let robots = Array.init nb (fun _ -> Decision.initial_memory) in

  (** Chaque robot reçoit une observation et décide d'une action,
      en mettant éventuellement à jour sa mémoire. *)
  let robot i =
    Lwt_preemptive.detach (fun observation ->
        if observation.World.game_over then exit 0 else
        let action, memory = Decision.decide visualize observation robots.(i) in
        robots.(i) <- memory;
        action
    )
  in

  (** Le protocole de communication force le robot à répondre en moins
      de [max_duration] secondes. *)
  let communicate () =
    Communication.client @@ fun (i, o) ->
    Ext.Lwt.timeout max_duration Decision.Wait (robot i o)
  in

  (** Le client s'exécute tant que nécessaire. *)
  Lwt_main.run (Ext.Lwt.forever communicate ())

let visualize = Cmdliner.(Arg.(
  value & flag & info ["v"] ~doc:"Visualize the game graphically"
))

let count = Cmdliner.(Arg.(
  value & opt int 1 & info ["n"] ~docv:"COUNT" ~doc:"Handle $(docv) robots."
))

let max_duration = Cmdliner.(Arg.(
  value & opt float 0.1 & info ["t"]
  ~docv:"DURATION" ~doc:"Set client timeout to $(docv) seconds."
))

let cmd = Cmdliner.(
  let doc   = "Run the program as a client which controls the robots." in
  let exits = Term.default_exits in
  Term.(const run $ visualize $ max_duration $ count),
  Term.info "man" ~doc ~exits
)
