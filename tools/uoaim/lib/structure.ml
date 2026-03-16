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

type 'c t = Constr of 'c | And of 'c t list | Or of 'c t list

let rec map_constr (f : 'a -> 'b t) : 'a t -> 'b t = function
  | Constr c -> f c
  | And l -> And (List.map (map_constr f) l)
  | Or l -> Or (List.map (map_constr f) l)

let constr c = Constr c

let rec constraints = function
  | Constr c -> [ c ]
  | And l -> Misc.List.concat_map constraints l
  | Or l -> Misc.List.concat_map constraints l

let of_conj_list : 'c list -> 'c t = function
  | [ c ] -> Constr c
  | cs -> And (List.map constr cs)

let rec fold_map f acc = function
  | Constr c ->
      let acc, c = f acc c in
      (acc, Constr c)
  | And l ->
      let acc, l = Misc.List.fold_left_map (fold_map f) acc l in
      (acc, And l)
  | Or l ->
      let acc, l = Misc.List.fold_left_map (fold_map f) acc l in
      (acc, Or l)

let rec pp pp_constr fmt =
  let open Format in
  function
  | Constr c -> fprintf fmt "- %a" pp_constr c
  | And items ->
      fprintf fmt "@[<v 2>And:@,%a@]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,") (pp pp_constr))
        items
  | Or items ->
      fprintf fmt "@[<v 2>Or:@,%a@]"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,") (pp pp_constr))
        items
