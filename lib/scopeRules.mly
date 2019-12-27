%{
(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
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

proc:
 | PROC { $1 }
 | NUM  { $1 }

proc_list_sc:
| proc proc_list_sc {$1::$2}
| {[]}

scope_tree_list:
| scope_tree {[$1]}
| scope_tree scope_tree_list {$1::$2}

scope_tree:
 | LPAR NAME scope_tree_list RPAR  
   {
   BellInfo.Tree($2,[],$3)
   }
 | LPAR NAME proc_list_sc RPAR 
   {
   BellInfo.Tree($2,$3,[])
   }

%public top_scope_tree:
 | scope_tree_list
    { let ts = $1 in
      match ts with
      | [t] -> t
      | _ -> BellInfo.Tree ("",[],ts) }

level_tree_list:
| PROC { [$1],[] }
| level_tree {let ps =  [] in ps,[$1]}
| PROC level_tree_list
    { let ps,ts = $2 in
      $1::ps,ts }
| level_tree level_tree_list
    {let ps,ts = $2 in
     ps,$1::ts }

level_tree:
 | LPAR NAME level_tree_list RPAR
  {
   let ps,ts = $3 in
   BellInfo.Tree ($2,ps,ts)
   }

%public top_level_tree:
 | level_tree_list
    { let ps,ts = $1 in
      match ps,ts with
      | [],[t] -> t
      | _ -> BellInfo.Tree ("",ps,ts) }

memory_map_atom:
 | NAME COLON NAME { ($1,$3) }

%public top_memory_map:
 | atoms = separated_list(COMMA,memory_map_atom) { atoms }
