(**

   Ce module implémente les deux points d'entrée de l'exécutable :

   - "lambda man" implémente un robot. Dans ce cas, le programme se
     met en attente d'une interaction avec le serveur. Si on lui passe
     l'option "-n" suivie d'un entier N alors N robots seront pris en
     chargent par le client.

   - "lambda server" implémente le serveur de jeu.

      Il attend l'argument "-w" suivi d'un nom de fichier JSON
      décrivant le monde au format décrit dans le module World.

      Par défaut, il utilise la commande "lambda man" pour commander tous
      les robots de toutes les équipes.

      On peut aussi lui préciser une liste de commandes permettant de
      lancer des "lambda men". Dans ce cas,  il faut autant de commandes
      que d'équipes déclarées dans le fichier de description du monde.

*)

open LambdaLib
open Cmdliner

let default_cmd = LambdaMan.cmd

let cmds = [ LambdaMan.cmd; LambdaServer.cmd; WorldGenerator.cmd ]

let main = Term.(exit @@ eval_choice default_cmd cmds)
