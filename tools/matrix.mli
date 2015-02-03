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


(* Tool on matrices for compare. A code sharing attemp in 'mcompare' mess *)

(* cells are lists of their lines *)
type cell = string list
type column = cell array
type matrix = column list

module type Config = sig
  val mode : OutMode.t
  val chan : out_channel
  val verbose : int
  val show_empty_rows : bool
  val kinds : LogState.kind TblRename.t
  val conds : LogConstr.constr TblRename.t
  val orders : unit TblRename.t
end


(* Goes nowhere else *)
(*
val one_line : string -> cell
val one_lines : string list -> column
val one_liness : string list list -> matrix
*)

(*****************)
(* Simple Build  *)
(*****************)

module type I = sig
  type info 

(*
 Build information matrix from test result matrix.
 Arguments:
    + log  (ie column in log underlying matrix)
    + info (attached to row in log underlying matrix)
    + test result itself
*)

  val fmt_cell : LogState.t -> info -> LogState.test -> cell
(* Add function *)
  type v
  val add : v -> LogState.test -> v
end

module NoAdd : sig
  type v
  val add : v -> LogState.test -> v
end

module Build (I:I) : sig
  type key = I.info Key.t

(* Build a column *)
  val extract :
      LogState.t -> key array ->  LogState.test array -> column

(* Build a matrix *)
  val build : key array -> LogState.t list -> matrix

(* Sum columns *)
  val sum : key array -> I.v list -> LogState.t list -> I.v list
end

(*******************)
(* Final formating *)
(*******************)

module Dump(Opt:Config) :
  sig
    val dump :
        string (* legend *) ->
          bool (* horiz *) ->
            (int * string) list (* first row, 'header' *)->
              string list (* last row, 'sum' *) ->
              'a Key.t array -> (* First column *)
                ?col2:column -> (* Second column *)
                    matrix -> (* matrix proper *)
                      unit
  end
