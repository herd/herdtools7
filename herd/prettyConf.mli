(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Pretty printing configuration *)

(* What to show *)
type show =
  | ShowProp  (* According to prop *)
  | ShowCond  (* According to condition *)
  | ShowNone  (* Nothing *)
  | ShowAll   (* Everything *)
  | ShowNeg   (* prop negation *)
  | ShowWit   (* Be clever *)

val tags_show : string list
val pp_show : show -> string
val parse_show : string -> show option



(* Control over dot files (fig needs some adaptation) *)
type dotmode = Plain | Fig

val tags_dotmode : string list
val pp_dotmode : dotmode -> string
val parse_dotmode : string -> dotmode option

(* Control over dot command *)
type dotcom = DotCom | NeatoCom | CircoCom

val tags_dotcom : string list
val pp_dotcom : dotcom -> string
val parse_dotcom : string -> dotcom option

(* Events shown in figures *)
type showevents = AllEvents | MemEvents | NonRegEvents

val tags_showevents : string list
val pp_showevents : showevents -> string
val parse_showevents : string ->  showevents option

(* All options... *)
module type S = sig
  val verbose : int
  val dotcom : dotcom option
  val gv : bool
  val evince : bool
  val dotmode : dotmode
  val showevents : showevents
  val texmacros : bool
  val tikz : bool
  val hexa : bool
  val mono : bool
  val fontname : string option
  val fontsize : int option
  val edgedelta : int
  val penwidth : float option
  val arrowsize : float option
  val splines : Splines.t option
  val overlap : string option
  val sep : string option
  val pad : float option
  val margin : float option
  val scale : float
  val xscale : float
  val yscale : float
  val squished : bool
  val graph : Graph.t
  val showpo : bool
  val relabel : bool
  val withbox : bool
  val labelbox : bool
  val showthread : bool
  val showlegend : bool
  val showfinalrf : bool
  val showinitrf : bool
  val showpoloc : bool
  val showfr : bool
  val showinitwrites : bool
  val dotheader : string option
  val brackets : bool
  val showobserved : bool
  val movelabel : bool
  val fixedsize : bool
  val edgeattrs : DotEdgeAttr.t
  val doshow : StringSet.t
  val unshow : StringSet.t
  val symetric : StringSet.t
  val showraw : StringSet.t
  val extrachars : float
  val shift : float array
  val ptscale : float
  val edgemerge : bool
end
