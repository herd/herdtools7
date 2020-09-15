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
open BellInfo
let nones = { scopes=None; levels=None; regions=None; }
%}

%%

permut(S,L,R):
  scopes=S levels=L regions=R
| levels=L scopes=S regions=R
| regions=R levels=L scopes=S
| levels=L regions=R scopes=S
| regions=R scopes=S levels=L
| scopes=S regions=R levels=L
    { { levels=Some levels; scopes=Some scopes; regions=Some regions; } }
| scopes=S levels=L
| levels=L scopes=S
    { { nones with  levels=Some levels; scopes=Some scopes; } }
| scopes=S regions=R
| regions=R scopes=S
    { { nones with scopes=Some scopes; regions=Some regions; } }
| levels=L regions=R
| regions=R levels=L
    { { nones with levels=Some levels; regions=Some regions; } }
| scopes=S
    { { nones with scopes=Some scopes; } }
| levels=L
    { { nones with levels=Some levels; } }
| regions=R
    { { nones with regions=Some regions; } }
|
    {  nones }

scopes_key:
| SCOPES COLON top_scope_tree { $3 }

levels_key:
| LEVELS COLON top_level_tree { $3 }

memory_map_key:
| REGIONS COLON top_memory_map { $3 }



%public scopes_and_memory_map:
| x=permut(scopes_key, levels_key,  memory_map_key) { x }
