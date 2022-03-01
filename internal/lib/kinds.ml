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

module LR = LexRename.Make(struct let verbose = 0 end)

let of_file path =
  let tbl = LR.read_from_file path ConstrGen.parse_kind in
  TblRename.fold
    (fun name (kind,_) k -> (name,kind)::k)
    tbl []
