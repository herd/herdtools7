(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* Pretty printing configuration *)

type outputdir_mode =
  | NoOutputdir
  | Outputdir of string
  | StdoutOutput

(* What to show *)
type show =
  | ShowProp  (* According to prop *)
  | ShowCond  (* According to condition *)
  | ShowNone  (* Nothing *)
  | ShowAll   (* Everything *)
  | ShowNeg   (* prop negation *)
  | ShowWit   (* Be clever *)
  | ShowFlag of string (* Show execution flagged with string *)

let tags_show =["prop";"all";"neg";"cond";"none";"wit";]

let pp_show = function
  | ShowProp -> "prop"
  | ShowCond -> "cond"
  | ShowNone -> "none"
  | ShowAll -> "all"
  | ShowNeg -> "neg"
  | ShowWit -> "wit"
  | ShowFlag f -> Printf.sprintf "flag<%s>" f

let parse_show tag = match Misc.lowercase tag with
| "prop" -> Some ShowProp
| "cond" -> Some ShowCond
| "none" -> Some ShowNone
| "all" -> Some ShowAll
| "neg" -> Some ShowNeg
| "wit" -> Some ShowWit
| _ -> None



type dotmode = Plain | Fig

let tags_dotmode = ["plain";"fig";]

let pp_dotmode = function
  | Plain -> "plain"
  | Fig -> "fig"

let parse_dotmode s = match s with
| "plain" -> Some Plain
| "fig" -> Some Fig
| _ -> None
(* Control over dot command *)
type dotcom = DotCom | NeatoCom | CircoCom

let tags_dotcom = ["dot";"neato";"circo";]
let pp_dotcom = function
  | DotCom -> "dot"
  | NeatoCom -> "neato"
  | CircoCom -> "circo"

let parse_dotcom = function
  | "dot" ->  Some DotCom
  | "neato" ->  Some NeatoCom
  | "circo" ->  Some CircoCom
  | _ -> None

(* Events shown in figures *)
type showevents = AllEvents | MemEvents | NonRegEvents | MemFenceEvents

let tags_showevents = ["all"; "mem"; "noregs";"memf";]

let pp_showevents = function
  | AllEvents -> "all"
  | MemEvents -> "mem"
  | NonRegEvents -> "noregs"
  | MemFenceEvents -> "memfence"

let parse_showevents = function
  | "all"  -> Some AllEvents
  | "mem"|"memory" -> Some MemEvents
  | "noregs" -> Some NonRegEvents
  | "memf"|"memfence" -> Some MemFenceEvents
  | _ -> None


module type S = sig
  val debug : bool
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
  val dsiy : float
  val siwidth : float
  val boxscale : float
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
  val finaldotpos : float * float
  val initdotpos : float * float
  val oneinit : bool
  val initpos : (float * float) option
  val threadposy : float
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
  val classes : string option
  val showraw : StringSet.t
  val extrachars : float
  val shift : float array
  val ptscale : float
  val edgemerge : bool
  val labelinit : bool
  val variant : Variant.t -> bool
end
