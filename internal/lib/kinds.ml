(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Parse kinds.txt files. *)

module Option = Base.Option

exception ParseError of string

type kind = ConstrGen.kind
type t = (string * ConstrGen.kind) list

let check ~expected ~actual =
  let m =
    List.fold_left
      (fun m (ne,ke) -> StringMap.add ne ke m)
      StringMap.empty expected in
  List.fold_left
    (fun (ks,miss as b) (n,ka) ->
      try
        let ke = StringMap.find n m in
        if ConstrGen.compare_kind ka ke = 0 then b
        else begin
          (n,ke,ka)::ks,miss
        end
      with Not_found -> ks,n::miss)
    ([],[]) actual

let compare xs ys =
  let compare_pair (x_name, x_kind) (y_name, y_kind) =
    match String.compare x_name y_name with
    | 0 -> ConstrGen.compare_kind x_kind y_kind
    | n -> n
  in
  Base.List.compare compare_pair xs ys

let to_string ks =
  let max a b = if a > b then a else b in
  let rec max_len ks acc =
    match ks with
    | [] -> acc
    | (name, _) :: ks -> max_len ks (max (String.length name) acc)
  in
  let padding = max_len ks 0 in
  let buf = Buffer.create 16 in
  let append (name, kind) =
    Buffer.add_string buf name ;
    Buffer.add_string buf (String.make (1 + padding - (String.length name)) ' ') ;
    Buffer.add_string buf (ConstrGen.pp_kind kind) ;
    Buffer.add_char buf '\n'
  in
  List.iter append ks ;
  Buffer.contents buf


let all_spaces s =
  let len = String.length s in
  let rec all_rec j =
    if j >= len then true
    else
      match s.[j] with
      | ' '|'\t' -> all_rec (j+1)
      | _ -> false in
  all_rec 0

let is_comment l = String.length l > 0 && l.[0] = '#'

let from_channel ch =
  let string_pair_of_line l =
    let s = Stream.of_string l in
    let rec whitespace () =
      match Stream.peek s with
      | Some (' '|'\t') -> Stream.junk s ; whitespace ()
      | _ -> ()
    in
    let token () =
      let buf = Buffer.create 16 in
      let rec token' () =
        match Stream.peek s with
        | None | Some (' '|'\t'|'#') -> Buffer.contents buf
        | Some c -> Buffer.add_char buf c ; Stream.junk s ; token' ()
      in
      match token' () with
      | "" ->  raise (ParseError "empty string")
      | t -> t
    in
    whitespace () ;
    let first = token () in
    whitespace () ;
    let second = token () in
    whitespace () ;
    assert (Option.is_none (Stream.peek s)) ;
    first, second
  in
  let parse_line l =
    if all_spaces l || is_comment l then None
    else
      let name, kind = string_pair_of_line l in
      match ConstrGen.parse_kind kind with
      | None -> raise (ParseError "expected kind")
      | Some kind -> Some (name, kind)
  in
  Channel.map_opt_lines parse_line ch

let of_file path =
  Filesystem.read_file path from_channel
