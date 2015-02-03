(*********************************************************************)
(*                          DIY/Litmus                               *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*     Luc Maranget, INRIA Paris-Rocquencourt, France.               *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


type code = string CAst.t

let dump_prog cfun =
  let f = function
    | CAst.Test { CAst.params; body; proc = i } ->
        let string_of_ty ty = CType.dump ty ^ "*" in
        let f {CAst.param_ty; param_name} =
          Printf.sprintf "%s %s" (string_of_ty param_ty) param_name
        in
        let params = String.concat ", " (List.map f params) in
        Printf.sprintf "static void P%i(%s) {%s}\n" i params body
    | CAst.Global x -> Printf.sprintf "{%s}\n\n" x
  in
  [f cfun]

let dump_prog_lines prog =
  let pp = List.map dump_prog prog in
  let pp = List.concat pp in
  List.map (Printf.sprintf "%s\n") pp

let print_prog chan prog =
  let pp = dump_prog_lines prog in
  List.iter (Printf.fprintf chan "%s") pp
