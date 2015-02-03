(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Read renaming maps *)

exception Error

module type Config  = sig
  val verbose : int
end

module Make : functor(O:Config) ->
sig
  val read_from :
      int -> (* start value *)
        string -> in_channel ->
          'a TblRename.t -> (* old map *)
            (string -> 'a option) ->(* For parsing values *)
                int * 'a TblRename.t (* new map *)

  val read_from_file : 
      string -> (* One file name *)
        (string -> 'a option) ->(* For parsing values *)
            'a TblRename.t (* New map *)

  val read_from_files : 
      string list -> (* List of file names *)
        (string -> 'a option) ->(* For parsing values *)
            'a TblRename.t (* New map *)
            
  val read :
      string -> in_channel ->
        'a TblRename.t -> (* old map *)
          (string -> 'a option) ->(* For parsing values *)
              'a TblRename.t (* new map *)

end
