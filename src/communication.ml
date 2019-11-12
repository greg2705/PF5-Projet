(**

   Ce module implémente la couche de communication entre serveur
   et clients. On échange des informations "sérialisées" à travers
   un simple pipe UNIX.

*)

open Decision
open Lwt

type client = World.robot_identifier -> observation -> action Lwt.t

let server team_id client_cmd = Lwt_process.(Lwt_io.(
  let p = open_process ~stderr:`Keep (shell client_cmd) in
  let stdin = p#stdin and stdout = p#stdout in
  fun (id : World.robot_identifier) (observation : observation) ->
  write_value stdin (id, observation) >>= fun () ->
  if observation.World.game_over then Lwt.return Wait
  else try%lwt
     (read_value stdout : action Lwt.t)
  with End_of_file ->
    Lwt.return (Die "timeout")))

let client f = Lwt_io.(
  (read_value stdin : (World.robot_identifier * observation) Lwt.t)
  >>= fun (id, o) -> f (id, o)
  >>= (fun v -> Lwt_io.write_value stdout v >>= fun () -> Lwt_io.flush stdout)
)
