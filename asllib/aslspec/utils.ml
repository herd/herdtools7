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

let is_singleton_list = function [ _ ] -> true | _ -> false
let list_tl_or_empty list = match list with [] -> [] | _ :: t -> t

(** [split_last lst] returns a pair consisting of the prefix up to the last and
    the last element, assuming the list is non-empty. *)
let split_last lst =
  match List.rev lst with [] -> assert false | x :: xs -> (List.rev xs, x)

(** [list_concat_map f l] gives the same result as List.concat (List.map f l).
*)
let list_concat_map f l = List.concat (List.map f l)

(** [list_tail list] returns the tail of [list], or raises an [Invalid_argument]
    exception if [list] is empty. *)
let list_tail = function
  | [] -> raise (Invalid_argument "list_tail: empty list")
  | _ :: t -> t

let list_match_two_elements lst =
  match lst with [ x1; x2 ] -> (x1, x2) | _ -> assert false

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

(** [list_starts_with eq ~prefix lst] checks if [prefix] is a prefix of [lst]
    according to the equality function [eq]. *)
let rec list_starts_with eq ~prefix lst =
  match (prefix, lst) with
  | [], _ -> true
  | _, [] -> false
  | p :: pt, l :: lt -> eq p l && list_starts_with eq ~prefix:pt lt

(** [string_starts_with ~prefix s] checks if string [s] starts with [prefix]. *)
let string_starts_with ~prefix s =
  let prefix_len = String.length prefix in
  let s_len = String.length s in
  if prefix_len > s_len then false
  else String.equal (String.sub s 0 prefix_len) prefix

(** [string_replace_all regexp f s] replaces all matches of [regexp] in [s] by
    the result of applying [f] to each matched substring. *)
let string_replace_all regexp f s =
  Str.full_split regexp s
  |> List.map (function
    | Str.Text txt -> txt
    | Str.Delim match_str -> f match_str)
  |> String.concat ""

(** [list_get_all_option l] returns [Some lst] if all elements of [l] are
    [Some x], where [lst] is the list of all such [x]. Otherwise, returns
    [None]. *)
let list_get_all_option l =
  let exception None_found in
  let option_get = function None -> raise None_found | Some x -> x in
  try List.map option_get l |> Option.some with None_found -> None
