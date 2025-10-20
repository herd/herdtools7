(** Utility functions for functions that are not available in 4.08. *)
let list_is_empty = function [] -> true | _ -> false

let list_is_equal eq l1 l2 =
  let rec aux l1 l2 =
    match (l1, l2) with
    | [], [] -> true
    | x1 :: t1, x2 :: t2 -> eq x1 x2 && aux t1 t2
    | _ -> false
  in
  aux l1 l2

let is_singleton_list list = 1 == List.length list
let list_tl_or_empty list = match list with [] -> [] | _ :: t -> t

(** [list_concat_map f l] gives the same result as List.concat (List.map f l).
*)
let list_concat_map f l = List.concat (List.map f l)

(** [string_exists p s] checks if at least one character of [s] satisfies the
    predicate [p]. *)
let string_exists p s =
  let len = String.length s in
  let rec check_from_index i =
    if i >= len then false
    else if p s.[i] then true
    else check_from_index (i + 1)
  in
  check_from_index 0
