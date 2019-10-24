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

(** Entry point to Herd  *)
open Js_of_ocaml
open Printf
open Opts

let dbg = false

(* Add webpath in front of include list, all pseudo-files will be there *)

let _ = includes := WebInput.webpath :: !includes

let load_config s =
  let module ML =
    MyLib.Make
      (struct
        let includes = !includes
        let env = None
        let libdir = Version.libdir
      end) in
  let found = ML.find s in
  LexConf_herd.lex found


(* Configure parser/models/etc. *)
let run_herd bell cat litmus cfg =

  let bell = Js.to_string bell
  and cat = Js.to_string cat
  and litmus = Js.to_string litmus
  and cfg = Js.to_string cfg in

  if dbg then begin
    eprintf "** bell **\n%s" bell ;
    eprintf "** cat  **\n%s" cat ;
    eprintf "** cfg  **\n%s" cfg ;
    eprintf "** lit  **\n%s" litmus ;
    ()
  end ;

  let insert_in_web_output s =
    Js.Unsafe.fun_call (Js.Unsafe.variable "herd_output") [|Js.Unsafe.inject (Js.string s)|] in
  Sys_js.set_channel_flusher stdout insert_in_web_output ;

  if not dbg then begin
    let insert_in_web_stderr s =
      Js.Unsafe.fun_call (Js.Unsafe.variable "herd_stderr") [|Js.Unsafe.inject (Js.string s)|] in
    Sys_js.set_channel_flusher stderr insert_in_web_stderr
  end ;
  let bell_fname = WebInput.set_bell_str bell
  and cat_fname  = WebInput.set_cat_str cat
  and cfg_fname = WebInput.set_cfg_str cfg
  and litmus_fname = WebInput.set_litmus_str litmus in
  WebInput.register_autoloader ();
  (* web options *)
  outputdir := PrettyConf.StdoutOutput;
  dumpes := false;
  load_config cfg_fname;
  show := PrettyConf.ShowAll;
  (* Do not overide default or config settings by default arguments *)
  begin match cat with
  | "" -> ()
  | _ ->  model := Some (Model.File (cat_fname));
  end ;
  begin match bell with
  | "" -> () (* Default bell will not overwrite config setting *)
  | _ ->  Opts.bell := Some bell_fname
  end ;
  let args = ref [Filename.concat WebInput.webpath litmus_fname] in

  (* Read generic model, if any *)
 let module ML =
    MyLib.Make
      (struct
        let includes = !includes
        let env = None
        let libdir = Version.libdir
      end) in

 let libfind = ML.find in

 let module ParserConfig = struct
   let debug = !debug.Debug_herd.lexer
   let libfind =  libfind
  end in

  let model,model_opts = match !model with
    | Some (Model.File fname) ->
       let module P = ParseModel.Make(ParserConfig) in
       begin try
           let (b,_,_) as r = P.parse fname in
           Some (Model.Generic r),b
         with
         | Misc.Fatal msg -> eprintf "%s: %s\n" prog msg ; exit 2
         | Misc.Exit ->
            eprintf "Failure of generic model parsing\n" ;
            exit 2 end
    | Some _ as m -> m,ModelOption.compat
    | None -> None,ModelOption.default in

  (* Check names, NB no select argument! *)
  let module Verbose =
  struct let verbose = if !debug.Debug_herd.lexer  then !verbose else 0 end in

  let module Check =
    CheckName.Make
      (struct
        include Verbose
        let rename = !rename
        let select = []
        let names = !names
        let excl = !excl
      end) in

  (* Read kinds/conds files *)
  let module LR = LexRename.Make(Verbose) in

  let kinds = LR.read_from_files !kinds ConstrGen.parse_kind in

  let conds = LR.read_from_files !conds (fun s -> Some s) in

  let module Config = struct
    let auto = !auto
    let candidates = !candidates
    let nshow = !nshow
    let restrict = !restrict
    let showkind = !showkind
    let shortlegend = !shortlegend
    let model = model
    let through = !through
    let skipchecks = !skipchecks
    let strictskip = !strictskip
    let cycles = !cycles
    let outcomereads = !outcomereads
    let show = !show
    let badexecs = !badexecs
    let badflag = !badflag
    let throughflag = !throughflag

    let statelessrc11 = !statelessrc11

    let check_name = Check.ok
    let check_rename = Check.rename_opt
    let check_kind = TblRename.find_value_opt kinds
    let check_cond =  TblRename.find_value_opt conds
    let libfind = libfind
    let macros = !macros

    let model_enumco = model_opts.ModelOption.co
    let observed_finals_only = not model_enumco
    let initwrites = match !initwrites with
    | None -> model_opts.ModelOption.init
    | Some b -> b
    let check_filter = !check_filter
    let debug = !debug
    let debuglexer = debug.Debug_herd.lexer
    let verbose = !verbose
    let unroll = !unroll
    let speedcheck = !speedcheck
    let optace = match !optace with
    | Some b -> b
    | None -> match model with
      | Some (Model.Minimal b) -> b
      | Some (Model.Generic _|Model.File _) -> false
      | _ -> false
     let variant = !variant
    let byte = !byte
    let endian = !endian
    let outputdir = !outputdir
    let suffix = !suffix
    let dumpes = !dumpes
    let moreedges = !moreedges

    module PC = struct
      let debug = debug.Debug_herd.pretty
      let verbose = verbose
      let dotmode = !PP.dotmode
      let dotcom = !PP.dotcom
      let gv = !PP.gv
      let evince = !PP.evince
      let showevents = !PP.showevents
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
      let doshow = !PP.doshow
      let unshow = !PP.unshow
      let symetric = !PP.symetric
      let classes = !PP.classes
      let showraw = !PP.showraw

      let dotheader = match !PP.dotheader with
      | None -> None
      | Some f ->
          let fname = libfind f in
          try
            Misc.input_protect
              (fun chan ->
                let xs =
                  MySys.read_by_line chan (fun x xs -> x::xs) [] in
                Some (String.concat "\n" (List.rev xs)))
              fname
          with Sys_error msg ->
            eprintf "Cannot read %s: %s\n" f msg ;
            exit 2
      let shift = !PP.shift
      let edgemerge = !PP.edgemerge
      let labelinit = !PP.labelinit
      let variant = variant
    end

  end in

  let bi = match !Opts.bell with
  | None -> None
  | Some fname ->
      let module R =
        ReadBell.Make
          (struct
            let debug_lexer = Config.debug.Debug_herd.lexer
            let debug_model = Config.debug.Debug_herd.barrier
            let verbose = Config.verbose
            let libfind = libfind
            let compat = Config.variant Variant.BackCompat
            let prog = prog
          end) in
      let bi = R.read fname in
      Some (fname,bi) in

  let from_file =
    let module T =
      ParseTest.Top (struct let bell_model_info = bi include Config end) in
    T.from_file in


(* Just go *)

  let tests = !args in
  let check_exit =
    let b = !Opts.exit_if_failed in
    fun seen -> if b then exit 1 else seen in
  let _seen =
    Misc.fold_argv
      (fun name seen ->
        try from_file name seen
        with
        | Misc.Exit -> check_exit seen
        | Misc.Fatal msg ->
            Warn.warn_always "%a: %s" Pos.pp_pos0 name msg ;
             check_exit seen
        | Misc.UserError msg ->
            Warn.warn_always "%a: %s (User error)" Pos.pp_pos0 name msg ;
             check_exit seen
        | e ->
            Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
            raise e)
      tests StringMap.empty in
  ()

let () = Js.Unsafe.global##.runHerd := Js.wrap_callback run_herd;
