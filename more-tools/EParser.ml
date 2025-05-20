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

module Make (O:FindDef.Config) = struct

open Earley_core

let () = ignore (O.includes) ; ignore (O.libdir)


module FD = FindDef.Make(O)

let reduce_arg = PreCat.reduce FD.find
and reduce_def = PreCat.reduce FD.find_def

open PreCat

let parser define =
  | ws:words ":" args:args0 -> ( Def (get_tag ws,reduce_def ws,ws,args) )

and parser args0 =
  | xs:arg0+  -> ( xs )

and parser arg0 =
  | "o" ws:words "." -> ( Arg (reduce_arg ws,ws) )
  | "o" ws:words ":" xs:args1 -> (Connect (get_tag ws,ws,xs))

and parser args1 =
  | xs:arg1+ -> ( xs )

and parser arg1 =
  | dash ws:words "." -> ( Arg (reduce_arg ws,ws) )

and parser dash =
  | "-" | "--"

and parser words = ws:word+ -> ( ws )

and parser word =
  | w:RE("[-/a-zA-Z]*[a-zA-Z][-/a-zA-Z]*") -> ( w )
  | e:"E" n:RE("[1-9]") -> ( e ^ n )

and parser main = define+ EOF

let zyva _ chan = Earley.parse_channel main Blanks.default chan

end
