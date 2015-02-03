(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Parsing of command line options and reading of configuration files *)

(* modify mutable variables defined here *)

(* Myself *)
val prog : string

(* Local options *)
val verbose : int ref
val debug : Debug.t ref
val names : string list ref
val rename : string option ref
val kinds : string list ref
val conds : string list ref
val model : Model.t option  ref
val unroll : int ref
val speedcheck : Speed.t ref
val optace : bool option ref
val initwrites : bool option ref
val badexecs : bool ref
val through : Model.through ref
val skipchecks : StringSet.t ref
val strictskip : bool ref
val show : PrettyConf.show ref
val nshow :int option ref
val auto : bool ref
val restrict : Restrict.t ref
val showkind : bool ref
val shortlegend : bool ref
val outcomereads : bool ref
val outputdir : string option ref
val suffix : string ref
val dumpes : bool ref
val dumplem : bool ref
val dumptex : bool ref

(* Pretty printing configuration, deserves its own module *)
module PP : sig
  open PrettyConf
  val dotmode : dotmode ref
  val dotcom : dotcom option ref
  val evince : bool ref
  val gv : bool ref
  val showevents : showevents ref
  val texmacros : bool ref
  val tikz : bool ref
  val hexa : bool ref
  val mono : bool ref
  val fontname : string option ref
  val fontsize : int option ref
  val edgedelta : int ref
  val penwidth : float option ref
  val arrowsize : float option ref
  val splines : Splines.t option ref
  val overlap : string option ref
  val sep : string option ref
  val pad : float option ref
  val margin : float option ref
  val scale : float ref
  val xscale : float ref
  val yscale : float ref
  val ptscale : float ref
  val squished : bool ref
  val graph : Graph.t ref
  val showpo : bool ref
  val relabel : bool ref
  val withbox : bool ref
  val labelbox : bool ref
  val showthread : bool ref
  val showlegend : bool ref
  val showfinalrf : bool ref
  val showinitrf : bool ref
  val showpoloc : bool ref
  val showfr : bool ref
  val showinitwrites : bool ref
  val dotheader : string option ref
  val brackets : bool ref
  val showobserved : bool ref
  val movelabel : bool ref
  val fixedsize : bool ref

  val add_edgeattr : string -> string -> string -> unit
  val get_edgeattrs : unit -> DotEdgeAttr.t
  val doshow : StringSet.t ref
  val unshow : StringSet.t ref
  val symetric : StringSet.t ref
  val showraw : StringSet.t ref
  val extrachars : float ref
  val shift : float array ref      
  val edgemerge : bool ref
end

