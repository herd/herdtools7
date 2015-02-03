(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Event components *)
type loc = string
val ok : loc

type v = int
type proc = int

(* Direction of event *)
type dir = W | R

(* Edges compoments that do not depend on architecture *)

(* Change or proc accross edge *)
type ie = Int|Ext

(* Change of location across edge *)
type sd = Same|Diff

(* Direction of related events *)
type extr = Dir of dir | Irr 



(* Associated pretty print & generators *)
val pp_ie : ie -> string
val pp_dir : dir -> string
val pp_extr : extr -> string
val pp_sd : sd -> string

val fold_ie : (ie -> 'a -> 'a) -> 'a -> 'a
val fold_extr : (extr -> 'a -> 'a) -> 'a -> 'a
val fold_sd : (sd -> 'a -> 'a) -> 'a -> 'a
val fold_sd_extr : (sd -> extr -> 'a -> 'a) -> 'a -> 'a
val fold_sd_extr_extr : (sd -> extr -> extr -> 'a -> 'a) -> 'a -> 'a

type check =
  | Sc | Uni | Thin | Critical | Free | Ppo | Transitive | Total

(* Com *)
type com =  CRf | CFr | CWs

val pp_com : com -> string
val fold_com : (com -> 'a -> 'a) -> 'a -> 'a

(* Info in tests *)
type info = (string * string) list

(* Name of plain accesses *)
val plain : string
