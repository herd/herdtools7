(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Parsing of command line options and reading of configuration files *)

(* modify mutable variables defined here *)

(* Myself *)
val prog : string

(* Local options *)
val verbose : int ref
val includes : string list ref
val exit_if_failed : bool ref
val debug : Debug_herd.t ref
val names : string list ref
val excl : string list ref
val rename : string list ref
val kinds : string list ref
val conds : string list ref
val model : Model.t option  ref
val bell : string option ref
val macros : string option ref
val unroll : int ref
val speedcheck : Speed.t ref
val optace : bool option ref
val variant : (Variant.t -> bool) ref
val precision : bool ref
module OptS : ParseTag.OptS with type t = Variant.t
val byte : MachSize.Tag.t ref
val endian : Endian.t option ref
val initwrites : bool option ref
val check_filter : bool ref
val badexecs : bool ref
val badflag : string option ref
val through : Model.through ref
val throughflag : string option ref
val skipchecks : StringSet.t ref
val strictskip : bool ref
val cycles : StringSet.t ref
val show : PrettyConf.show ref
val nshow :int option ref
val auto : bool ref
val candidates : bool ref
val restrict : Restrict.t ref
val showkind : bool ref
val shortlegend : bool ref
val outcomereads : bool ref
val outputdir : PrettyConf.outputdir_mode ref
val suffix : string ref
val dumpes : bool ref
val dumplem : bool ref
val dumptex : bool ref
val moreedges : bool ref

val statelessrc11 : bool ref

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
  val dsiy : float ref
  val siwidth : float ref
  val boxscale : float ref
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
  val finaldotpos : (float * float) ref
  val initdotpos : (float * float) ref
  val oneinit : bool ref
  val initpos : (float * float) option ref
  val showinitwrites : bool ref
  val threadposy : float ref
  val dotheader : string option ref
  val brackets : bool ref
  val showobserved : bool ref
  val movelabel : bool ref
  val fixedsize : bool ref

  val add_edgeattr : string -> string -> string -> unit
  val get_edgeattrs : unit -> DotEdgeAttr.t
  val doshow : StringSet.t ref
  val unshow : StringSet.t ref
  val add_doshow : StringSet.t -> unit
  val add_unshow : StringSet.t -> unit
  val symetric : StringSet.t ref
  val classes : string option ref
  val showraw : StringSet.t ref
  val extrachars : float ref
  val shift : float array ref      
  val edgemerge : bool ref
  val labelinit : bool ref
end

val libfind : string list -> string -> string
