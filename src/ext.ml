(**

   Ce module fournit quelques extensions des modules standards.

*)

module Lwt = struct

  open Lwt

  let timeout duration default p =
    try%lwt
      pick [Lwt_unix.timeout duration; p]
    with Lwt_unix.Timeout ->
      Lwt.return default

  let rec forever something () =
    something () >>= forever something

  module List = struct

    let foldmap f init l =
      let rec aux accu ys = function
        | [] ->
           Lwt.return (List.rev ys, accu)
        | x :: xs ->
           let%lwt (y, accu) = f x accu in
           aux accu (y :: ys) xs
      in
      aux init [] l

  end

end

module Iso = struct

  type ('a, 'b) iso = ('a -> 'b) * ('b -> 'a)

  let id = (Fun.id, Fun.id)

end

module Fun = struct

  let repeat n f =
    let rec aux k accu =
      if k = n then List.rev accu else aux (k + 1) (f k :: accu)
    in
    aux 0 []

  let rec iter n f init =
    if n = 0 then init else iter (n - 1) f (f init)

end

module List = struct

  let cons_if cond x xs =
    if cond then x :: xs else xs

  let update eq xs x x' =
    List.map (fun x0 -> if eq x0 x then x' else x0) xs

  let foldmap f init l =
    let rec aux accu ys = function
      | [] ->
         (List.rev ys, accu)
      | x :: xs ->
         let y, accu = f x accu in
         aux accu (y :: ys) xs
    in
    aux init [] l

  let fold_semi_product f init l =
    let rec aux accu = function
      | [] -> accu
      | x :: xs -> aux (List.fold_left (fun accu y -> f accu x y) accu xs) xs
    in
    aux init l

  let fold_between f init l =
   let rec aux accu = function
     | [] | [_] -> accu
     | x :: (y :: _ as xs) -> aux (f accu x y) xs
   in
   aux init l

   let shuffle l =
     let shuffle a =
       for i = 0 to Array.length a - 1 do
         let tmp = a.(i) and j = i + Random.int (Array.length a - i) in
         a.(i) <- a.(j);
         a.(j) <- tmp
       done;
       a
     in
     Array.of_list l |> shuffle |> Array.to_list;;

   let reduce add neutral ps =
     let rec aux v = function
       | [] -> v
       | x :: xs -> aux (add x v) xs
     in
     aux neutral ps

   let best evaluator xs =
     assert (xs <> []);
     let rec aux (best, v) = function
       | [] ->
          (best, v)
       | x :: xs ->
          let v' = evaluator x in
          aux (if v' > v then (x, v') else (best, v)) xs
     in
     List.(aux (hd xs, evaluator (hd xs)) (tl xs))

end

let _ = Random.self_init ()

module Int = struct

  let check_equal i1 i2 msg =
    if i1 <> i2 then (
      msg i1 i2;
      exit 1
    )

  let random_in_range start stop =
    if stop < start then
      0
    else
      Random.int (stop - start + 1) + start

  module Set = Set.Make (Stdlib.Int)

  module Map = Map.Make (Stdlib.Int)

end

module Float = struct

  let random_in_range start stop =
    Random.float (stop -. start +. 1.) +. start

end

module Unix = struct

  let bench what f =
    let start = Unix.gettimeofday () in
    let y = f () in
    let stop =  Unix.gettimeofday () in
    Printf.eprintf "[%s] %fs\n%!" what (stop -. start);
    y

end
