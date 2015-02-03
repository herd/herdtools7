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

(* Parsing of command line options and reading of configuration files
   modify mutable variables defined here *)

(* Myself *)
let prog = 
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "herd"

(* Local options *)
let verbose = ref 0
let debug = ref Debug.none
let names = ref []
let rename = ref None
let kinds = ref []
let conds = ref []
let model = ref None
let unroll = ref 2
let speedcheck = ref Speed.False
let optace = ref None
let initwrites = ref None
let badexecs = ref true
let through = ref Model.ThroughNone
let skipchecks = ref StringSet.empty
let strictskip = ref false
let show = ref PrettyConf.ShowNone
let nshow = ref None
let auto = ref false
let restrict = ref Restrict.No
let showkind = ref false
let shortlegend = ref false
let outcomereads = ref false
let suffix = ref ""
let dumpes = ref false
let outputdir = ref None
let dumplem = ref false
let dumptex = ref false

(* Pretty printing configuration, deserves its own module *)
module PP = struct
  open PrettyConf
  let dotmode =  ref Plain
  let dotcom = ref None
  let gv = ref false
  let evince = ref false
  let showevents = ref NonRegEvents
  let texmacros = ref false
  let tikz = ref false
  let hexa = ref false
  let mono = ref false
  let fontname = ref None
  let fontsize = ref None
  let edgedelta = ref 0
  let penwidth = ref None
  let arrowsize = ref None
  let splines = ref None
  let overlap = ref None
  let sep = ref None
  let pad = ref None
  let margin = ref None
  let scale = ref 1.0
  let xscale = ref 1.0
  let yscale = ref 1.0
  let ptscale = ref 3.0
  let squished = ref false
  let graph = ref Graph.Cluster
  let showpo = ref true
  let relabel = ref false
  let withbox = ref false
  let labelbox = ref false
  let showthread = ref true
  let showlegend = ref true
  let showfinalrf = ref false
  let showinitrf = ref false
  let showpoloc = ref false
  let showfr = ref true
  let showinitwrites = ref true
  let dotheader = ref None
  let brackets = ref false
  let showobserved = ref false
  let movelabel = ref false
  let fixedsize = ref false

  let edgeattrs = ref DotEdgeAttr.empty
  let add_edgeattr lbl a v = edgeattrs := DotEdgeAttr.add lbl a v !edgeattrs
  let get_edgeattrs () = !edgeattrs

  let doshow = ref StringSet.empty
  let unshow = ref StringSet.empty
  let symetric = ref StringSet.empty
  let showraw = ref StringSet.empty
  let extrachars = ref 0.0
  let shift = ref [| |]
  let edgemerge = ref false
end

