module type Priority = sig
  include Set.OrderedType
  val infinity : t
  val to_string : t -> string
end

module Make (Key : Set.OrderedType) (Priority : Priority) = struct

  module O =
    struct
      type t = Priority.t * Key.t
      let compare (p1, k1) (p2, k2) =
        match Priority.compare p1 p2 with
        | -1 -> -1
        | 1 -> 1
        | 0 -> Key.compare k1 k2
        | _ -> assert false
    end

  module S = Set.Make (O)

  module M = Map.Make (Key)

  type t = S.t * Priority.t M.t

  let empty = (S.empty, M.empty)

  let get_min (s, _) = S.min_elt_opt s

  let length (s, _) =
    S.cardinal s

  let insert (s, m) k p =
    (S.add (p, k) s, M.add k p m)

  let remove_min (s, m) =
    let (p, k) = S.min_elt s in
    (S.remove (p, k) s, M.remove k m)

  let remove_key (s, m) k =
    let p = M.find k m in
    (S.remove (p, k) s, M.remove k m)

  let priority (_, m) k =
    match M.find_opt k m with None -> Priority.infinity | Some p -> p

  let decrease (s, m) k p =
    let (s, m) = try remove_key (s, m) k with Not_found -> (s, m) in
    insert (s, m) k p

end
