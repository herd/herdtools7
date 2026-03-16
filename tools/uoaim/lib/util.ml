(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

exception Parse_error of { msg : string; pos : int }
exception Interpret_error of { msg : string; context : string }

(** Read all contents of input channel *)
let read_all (ch : in_channel) : string =
  let buffer = Buffer.create 512 in
  let rec loop () =
    match input_line ch with
    | line ->
        Buffer.add_string buffer line;
        Buffer.add_char buffer '\n';
        loop ()
    | exception End_of_file -> ()
  in
  loop ();
  Buffer.contents buffer

let pp_list_semicolon pp =
  Format.(pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp)

let distance : string -> string -> int =
  let minimum a b c = min a (min b c) in
  fun s t ->
    let m = String.length s in
    let n = String.length t in
    let d = Array.make_matrix (m + 1) (n + 1) 0 in

    for i = 0 to m do
      d.(i).(0) <- i
    done;
    for j = 0 to n do
      d.(0).(j) <- j
    done;
    for j = 1 to n do
      for i = 1 to m do
        if s.[i - 1] = t.[j - 1] then d.(i).(j) <- d.(i - 1).(j - 1)
        else
          d.(i).(j) <-
            minimum
              (d.(i - 1).(j) + 1)
              (d.(i).(j - 1) + 1)
              (d.(i - 1).(j - 1) + 1)
      done
    done;
    d.(m).(n)

let find_best ~(score : 'a -> 'b) ~(is_better : than:'b -> 'b -> bool)
    (l : 'a list) : 'a * 'b =
  match l with
  | [] -> raise (Invalid_argument "empty list")
  | x :: xs ->
      List.fold_left
        (fun ((_, best_score) as best) x ->
          let s = score x in
          if is_better ~than:best_score s then (x, s) else best)
        (x, score x)
        xs

(* Replace every match of the regular expression [re] left-to-right using [f],
   threading an accumulator through. *)
let replace_all (re : Re.re) (f : 'acc -> Re.Group.t -> string * 'acc)
    (acc : 'acc) (str : string) : string * 'acc =
  let buf = Buffer.create (String.length str) in
  let last_ix, acc =
    Re.all re str
    |> List.fold_left
         (fun (last_ix, acc) re_match ->
           let start_ix, end_ix = Re.Group.offset re_match 0 in
           let chunk_len = start_ix - last_ix in
           if chunk_len > 0 then Buffer.add_substring buf str last_ix chunk_len;
           let new_sub, new_acc = f acc re_match in
           Buffer.add_string buf new_sub;
           (end_ix, new_acc))
         (0, acc)
  in
  let tail_len = String.length str - last_ix in
  if tail_len > 0 then Buffer.add_substring buf str last_ix tail_len;
  (Buffer.contents buf, acc)

module Arg = struct
  let parse (opts : (string * string list * Arg.spec * string) list) =
    let opts =
      opts
      |> Misc.List.concat_map (fun (cmd, aliases, spec, desc) ->
          (cmd, spec, desc)
          :: List.map
               (fun a -> (a, spec, Format.sprintf "alias of %s" cmd))
               aliases)
    in
    Arg.parse opts
end

module List = struct
  (** Like [List.fold_left], but uses the input list's first element as the
      starting accumulator. Assumes the input list is non-empty.
      @raise Invalid_argument if the input list is empty. *)
  let fold_left_ne f = function
    | [] -> raise (Invalid_argument "empty list")
    | x :: xs -> List.fold_left f x xs
end

module String = struct
  let to_words s = String.split_on_char ' ' s
  let from_words ws = String.concat " " ws
  let remove_spaces s = String.(concat "" (to_words s))

  (* Copied from stdlib *)
  let ends_with ~suffix s =
    let len_s = String.length s and len_suf = String.length suffix in
    let diff = len_s - len_suf in
    let rec aux i =
      if i = len_suf then true
      else if String.unsafe_get s (diff + i) <> String.unsafe_get suffix i then
        false
      else aux (i + 1)
    in
    diff >= 0 && aux 0

  (** Return the line containing [pos] within [s] and its 0-based index.
      @raise Invalid_argument if [pos] is out of bounds. *)
  let line_of_pos s pos =
    let len = String.length s in
    if pos < 0 || pos > len then invalid_arg "Index out of bounds";
    let start =
      if pos = 0 then 0
      else
        match String.rindex_from s (pos - 1) '\n' with
        | i -> i + 1
        | exception Not_found -> 0
    in
    let stop =
      if pos = len then len
      else
        match String.index_from s pos '\n' with
        | i -> i
        | exception Not_found -> len
    in
    let line_index =
      let count = ref 0 in
      for i = 0 to start - 1 do
        if s.[i] = '\n' then incr count
      done;
      !count
    in
    (String.sub s start (stop - start), line_index)
end

module type Applicative = sig
  type 'a t

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end

(** Allows the traversal of a ['a t list] where [_ t] is a parameterized type
    with an applicative interface. *)
module ListTraversal (A : Applicative) : sig
  val traverse : ('a -> 'b A.t) -> 'a list -> 'b list A.t
  val sequence : 'a A.t list -> 'a list A.t
end = struct
  let rec traverse f = function
    | [] -> A.pure []
    | x :: xs ->
        let cons y ys = y :: ys in
        A.apply (A.apply (A.pure cons) (f x)) (traverse f xs)

  let sequence xs = traverse (fun x -> x) xs
end

module Either = struct
  type ('a, 'b) t = Left of 'a | Right of 'b

  let left x = Left x
  let right y = Right y
  let fold ~left ~right = function Left x -> left x | Right y -> right y
end
