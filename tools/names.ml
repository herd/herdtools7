(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

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
