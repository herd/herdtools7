(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2011-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Extract names from file list *)

module S =Splitter.Make(Splitter.Default)

let from_file name chan = 
  let splitted =  S.split name chan in
  splitted.Splitter.name.Name.name

let from_fname name = Misc.input_protect (from_file name) name

let from_fnames names =
  List.fold_right
    (fun name k ->
      try
        from_fname name::k
      with
      | Misc.Exit -> k
      | Misc.Fatal msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 name msg ;
          k
      | e ->
	  Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
	  raise e)
    names []
