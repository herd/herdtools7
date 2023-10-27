(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Extending built-in / base modules, either to port future features into
 *  earlier versions of OCaml, or to add extra functionality. *)

module Fun = struct
  exception Finally_raised of exn

  let negate f =
    fun a -> not (f a)

  let protect ~finally f =
    let finally' () =
      try finally ()
      with e -> raise (Finally_raised e)
    in
    let ret =
      try
        f ()
      with e -> begin
        finally' () ;
        raise e
      end
    in
    finally' () ;
    ret

  let open_out_protect f name =
    let out = open_out name in
    protect ~finally:(fun () -> close_out out) (fun () -> f out)

end

module List = struct
  include List

  let rec compare cf xs ys =
    match xs, ys with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
        match cf x y with
        | 0 -> compare cf xs ys
        | n -> n

  let to_ocaml_string f xs =
    Printf.sprintf "[%s]" (String.concat "; " (List.map f xs))
end

module Option = struct
  type 'a t = 'a option

  let get o =
    match o with
    | None -> invalid_arg "option is None"
    | Some v -> v

  let value o ~default =
    match o with
    | None -> default
    | Some v -> v

  let map f o =
    match o with
    | None -> None
    | Some v -> Some (f v)

  let is_none o =
    match o with
    | None -> true
    | Some _ -> false

  let compare cf a b =
    match a, b with
    | None, None -> 0
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some a, Some b -> cf a b

  let to_ocaml_string f o =
    match o with
    | None -> "None"
    | Some a -> Printf.sprintf "Some (%s)" (f a)
end

module String = struct
  include String

  let to_ocaml_string s = Printf.sprintf "%S" s
end

module Iter = struct
  type 'a t = unit -> 'a option

  let of_list xs =
    let r = ref xs in
    fun () ->
      match !r with
      | [] -> None
      | x::xs -> r := xs ; Some x
end
