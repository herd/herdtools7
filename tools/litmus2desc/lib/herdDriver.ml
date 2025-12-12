(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Herd_core

module DefaultConfig = struct
  include GenParser.DefaultConfig
  module Timer = Timer.No

  let collect_graph_data = true
  let model = None
  let archcheck = false
  let through = Model.ThroughInvalid
  let strictskip = false
  let cycles = StringSet.empty
  let bell_model_info = None
  let debuglexer = false
  let check_kind = fun _ -> None
  let check_cond = fun _ -> None
  let restrict = Restrict.No
  let outcomereads = false
  let show = PrettyConf.ShowProp
  let nshow = None
  let badflag = None
  let badexecs = true
  let throughflag = None
  let verbose = 0
  let optace = OptAce.Iico
  let unroll = None
  let speedcheck = Speed.False
  let debug = Debug_herd.none
  let observed_finals_only = not ModelOption.(default.co)
  let initwrites = ModelOption.default.init
  let check_filter = true
  let maxphantom = None
  let variant = fun _ -> false
  let fault_handling = Fault.Handling.default
  let mte_precision = Precision.default
  let mte_store_only = false
  let skipchecks = StringSet.empty
  let statelessrc11 = false
  let dumpallfaults = false
  let endian = None
  let byte = MachSize.Tag.Auto
  let sve_vector_length = 128
  let sme_vector_length = 128
  let macros = None
  let check_name = fun _ -> true
  let check_rename = fun s -> Some s
  let hexa = false

  module PC = struct
    module PP = Opts.PP

    let debug = debug.Debug_herd.pretty
    let verbose = verbose
    let dotmode = !PP.dotmode
    let dotcom = !PP.dotcom
    let view = !PP.view
    let showevents = PrettyConf.AllEvents
    let texmacros = !PP.texmacros
    let tikz = !PP.tikz
    let hexa = !PP.hexa
    let mono = !PP.mono
    let fontname = !PP.fontname
    let fontsize = !PP.fontsize
    let edgedelta = !PP.edgedelta
    let penwidth = !PP.penwidth
    let arrowsize = !PP.arrowsize
    let splines = !PP.splines
    let overlap = !PP.overlap
    let sep = !PP.sep
    let margin = !PP.margin
    let pad = !PP.pad
    let scale = !PP.scale
    let xscale = !PP.xscale
    let yscale = !PP.yscale
    let dsiy = !PP.dsiy
    let siwidth = !PP.siwidth
    let boxscale = !PP.boxscale
    let ptscale = !PP.ptscale
    let squished = !PP.squished
    let graph = !PP.graph
    let showpo = !PP.showpo
    let relabel = !PP.relabel
    let withbox = !PP.withbox
    let labelbox = !PP.labelbox
    let showthread = !PP.showthread
    let showlegend = !PP.showlegend
    let showkind = !PP.showkind
    let shortlegend = !PP.shortlegend
    let showfinalrf = !PP.showfinalrf
    let showinitrf = !PP.showinitrf
    let finaldotpos = !PP.finaldotpos
    let initdotpos = !PP.initdotpos
    let oneinit = !PP.oneinit
    let initpos = !PP.initpos
    let threadposy = !PP.threadposy
    let showinitwrites = !PP.showinitwrites
    let brackets = !PP.brackets
    let showobserved = !PP.showobserved
    let movelabel = !PP.movelabel
    let fixedsize = !PP.fixedsize
    let extrachars = !PP.extrachars
    let edgeattrs = PP.get_edgeattrs ()
    let doshow = StringSet.of_list [ "dob"; "dtrm"; "co" ]
    let unshow = !PP.unshow
    let noid = !PP.noid
    let symetric = !PP.symetric
    let classes = !PP.classes
    let showraw = StringSet.of_list [ "dob"; "dtrm" ]
    let dotheader = None
    let shift = !PP.shift
    let edgemerge = !PP.edgemerge
    let labelinit = !PP.labelinit
    let variant = variant
  end
end

let top ~libdir str =
  let libfind =
    let module ML = MyLib.Make (struct
      let includes = []
      let env = Some "HERDLIB"

      let libdir =
        match libdir with
        | Some libdir -> libdir
        | None -> Filename.concat Version.libdir "herd"

      let debug = false
    end) in
    ML.find
  in
  let module C = struct
    include DefaultConfig

    let libfind = libfind
  end in
  let module T = ParseTest.Top (C) in
  let _, results = T.from_string ~filename:None ~contents:str StringMap.empty in
  match results with None -> assert false | Some results -> results

let fold_execs :
    ('exec -> 'a -> 'a) ->
    'a ->
    ('es, 'exec, 'stats) Top_herd.TestResult.t ->
    'a * 'stats =
 fun f acc t ->
  let acc_ref = ref acc in
  let stats =
    t.exec_iter (fun exec ->
        let new_acc = f exec !acc_ref in
        acc_ref := new_acc)
  in
  (!acc_ref, stats)
