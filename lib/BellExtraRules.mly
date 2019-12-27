%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
%}

%%

scope_option:
| SCOPES COLON top_scope_tree {Some $3}
| {None}

levels_option:
| LEVELS COLON top_level_tree {Some $3}
| {None}

memory_map_option:
| REGIONS COLON top_memory_map {Some $3}
| {None}

%public scopes_and_memory_map:
 | scopes=scope_option levels=levels_option regions=memory_map_option
{ let open BellInfo in { scopes; regions; levels;}}

