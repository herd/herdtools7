(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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
