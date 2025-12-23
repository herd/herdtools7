(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Utilities for command-line options *)

open Printf

type 'a tfun = string -> ('a -> unit) -> string -> string * Arg.spec * string
type 'a tref = string -> 'a ref  -> string -> string * Arg.spec * string

let badarg opt arg ty =
  raise
    (Arg.Bad
       (sprintf "wrong argument '%s'; option '%s' expects a %s"
          opt arg ty))

(* Parsing booleans *)

let parse_bool opt v msg =
  opt,
  Arg.Bool (fun b -> v := b),
  sprintf "<bool> %s, default %b" msg !v

(* Parsing ints *)

let parse_int opt v msg =
  opt,
  Arg.Int (fun b -> v := b),
  sprintf "<int> %s, default %i" msg !v

let parse_int_opt opt v msg =
  opt,
  Arg.String
    (fun tag -> match tag with
    | "none" -> v := None
    | _ ->
        try v := Some (int_of_string tag)
        with _ -> badarg opt tag "integer"),
  sprintf "<int|none> %s" msg

(* Parsing floats *)

let parse_float opt v msg =
  opt,
  Arg.Float (fun b -> v := b),
  sprintf "<float> %s, default %.1f" msg !v

let parse_float_opt opt v msg =
  opt,
  Arg.String
    (fun tag -> match tag with
    | "none" -> v := None
    | _ ->
        try v := Some (float_of_string tag)
        with _ -> badarg tag opt "float"   ),
  sprintf "<float|none> %s" msg

(* Parsing positions *)

type pos = float * float

let parse_pos opt v msg =
  opt,
  Arg.String
    (fun tag -> match Misc.pos_of_string tag with
    | Some p -> v := p
    | None ->  badarg tag opt "float,float"),
  let x,y = !v in
  sprintf "<float,float> %s, default %.1f,%.1f" msg x y


let parse_pos_opt opt v msg =
  opt,
  Arg.String
    (fun tag -> match Misc.pos_of_string tag with
    | Some p -> v := Some p
    | None ->  badarg tag opt "float,float"),
  sprintf "<float,float> %s" msg

(* Parsing strings *)

let parse_string opt v msg =
  opt,
  Arg.String (fun s -> v := s),
  sprintf "<string> %s, default %s" msg !v

let parse_string_opt opt v msg =
  opt,
  Arg.String (fun s -> match s with "none" -> v := None | _ -> v := Some s),
  sprintf "<string|none> %s" msg

let parse_stringsetfun opt f msg =
  opt,
  Arg.String
    (fun tag ->
      let es = Misc.split_comma tag in
      f (StringSet.of_list es)),
  sprintf "<name,..,name> %s" msg

let parse_stringset opt v msg =
  parse_stringsetfun opt (fun s -> v := StringSet.union s !v) msg

(* Parsing tags *)

type ttag =
  string -> (string -> bool) -> string list
    -> string -> string * Arg.spec * string

let do_tag opt set tags tag =
  if not (set tag) then
    raise
      (Arg.Bad
         (sprintf "bad tag for %s, allowed tag are %s"
            opt (String.concat "," tags)))

let parse_tag opt set tags msg =
  opt,
  Arg.String (do_tag opt set tags),
  sprintf "<tags> where tags is <%s>. %s" (String.concat "|" tags) msg

let parse_tags opt set tags msg =
  let do_tag = do_tag opt set tags in
  opt,
  Arg.String
    (fun tags -> Misc.split_comma tags |>  List.iter do_tag),
  sprintf "<tags> where tags is <%s> (comma separated list). %s" (String.concat "|" tags) msg
