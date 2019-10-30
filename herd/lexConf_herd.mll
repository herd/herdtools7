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

(* Read configuration files *)

{
open Printf
open Opts
open LexMisc

exception LocError of string

let error msg = raise (LocError msg)

let lex_some ty lex arg = match arg with
| "none" -> None
| _ ->
    try Some (lex arg)
    with _ -> error (sprintf "%s parameter expected" ty)

let lex_tag_fun key parse tags set tag = match parse tag with
| Some x -> set x
| None ->
    error
      (sprintf "bad tags for %s, allowed tag are %s"
         key (String.concat "," tags))

let lex_tag key parse tags v tag =
  lex_tag_fun key parse tags (fun x -> v := x) tag

let lex_bool_fun set arg =
  let x =
    try bool_of_string arg
    with _ ->  error "bool parameter expected" in
  set x

let lex_bool v arg = lex_bool_fun (fun b -> v := b) arg

let lex_int_fun set arg =
  let x =
    try int_of_string arg
    with _ ->  error "integer parameter expected" in
  set x

let lex_int r arg = lex_int_fun (fun x -> r := x) arg
let lex_int_opt r arg =
  r := lex_some "integer"  int_of_string arg

let lex_float_fun set arg =
  let x =
    try float_of_string arg
    with _ ->  error "float parameter expected" in
  set x

let lex_float r arg = lex_float_fun (fun x -> r := x) arg
let lex_float_opt r arg =
  r :=  lex_some "float"  float_of_string arg

let lex_pos_fun set arg = match Misc.pos_of_string arg with
| Some p -> set p
| None -> error "pair of float parameter expected"

let lex_pos r arg = lex_pos_fun (fun p -> r := p) arg
let lex_pos_opt r arg = lex_pos_fun (fun p -> r := Some p) arg

let lex_string_opt v arg =
  v := lex_some "string" (fun s -> s) arg

let lex_stringsetfun f arg =
  let es =  Misc.split_comma arg in
  f (StringSet.of_list es)

let lex_stringset v arg =
  lex_stringsetfun (fun s -> v := StringSet.union s !v) arg

open Lexing

let dolex main fname =
  let dolex chan =
    let lexbuf = Lexing.from_channel chan in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname=fname;};
    try main lexbuf
    with LocError msg -> LexMisc.error msg lexbuf in
  try Misc.input_protect dolex fname
  with Error (msg,pos) ->
    eprintf "%a: %s\n" Pos.pp_pos pos msg ;
    exit 2

let handle_key main key arg = match key with
| "conf" ->
     let module ML =
      MyLib.Make
        (struct
          let includes = !includes
          let env = Some "HERDLIB"
          let libdir = Version_herd.libdir
        end) in
      dolex main (ML.find arg)
| "verbose" ->  lex_int verbose arg
| "suffix" ->  suffix := arg
| "include" ->
   includes := !includes @ [arg]
(* Change input *)
| "names" ->
    names := !names @ [arg]
| "excl" ->
    excl := !excl @ [arg]
| "rename" ->
    rename := !rename @ [arg]
| "kinds" ->
    kinds := !kinds @ [arg]
| "conds" ->
    conds := !conds @ [arg]
(* Behaviour control *)
| "model"|"cat"  ->
    lex_tag_fun
       key Model.parse Model.tags
       (fun x -> model := Some x) arg
| "bell" ->
    bell := Some arg
| "macros" ->
    macros := Some arg
| "variant" ->
    let module PV = ParseTag.MakeS(Opts.OptS) in
    PV.parse_tag_set "variant" variant arg
| "machsize" ->
     lex_tag "machsize" MachSize.Tag.parse MachSize.Tag.tags byte arg
| "endian" ->
     lex_tag_fun "endian" Endian.parse Endian.tags
        (fun t -> endian := Some t) arg
| "through" ->
    lex_tag
       "through" Model.parse_through Model.tags_through
       through arg
| "skipchecks" ->
    lex_stringset skipchecks arg
| "strictskip" ->
    lex_bool strictskip arg
| "unroll" ->
    lex_int unroll arg
| "optace" ->
    lex_bool_fun (fun b ->  optace := Some b) arg
| "initwrites" ->
    lex_bool_fun (fun b ->  initwrites := Some b) arg
| "speedcheck" ->
     lex_tag "speedcheck" Speed.parse Speed.tags speedcheck arg
| "badexecs" ->
     lex_bool badexecs arg
| "badflag" ->
     lex_string_opt badflag arg
(* Control output *)
| "auto" ->
    lex_bool auto arg
| "show" ->
    lex_tag "show"
       PrettyConf.parse_show PrettyConf.tags_show show arg
| "showflag" ->
      show := PrettyConf.ShowFlag arg
| "nshow" ->
    lex_int_fun (fun x -> nshow := Some x) arg
| "restrict" ->
    lex_tag
       "restrict" Restrict.parse Restrict.tags restrict
       arg
| "showkind" ->  lex_bool showkind arg
| "shortlegend" ->
     lex_bool shortlegend arg
| "texmacros" ->
     lex_bool PP.texmacros arg
| "hexa" ->
     lex_bool PP.hexa arg
| "outcomereads" ->
     lex_bool outcomereads arg
| "dotmode" ->
     lex_tag "dotmode" PrettyConf.parse_dotmode PrettyConf.tags_dotmode
        PP.dotmode arg
| "dotcom" ->
     lex_tag_fun
        "dotcom" PrettyConf.parse_dotcom
        PrettyConf.tags_dotmode
        (fun x -> PP.dotcom := Some x)
        arg
| "gv" ->
     lex_bool PP.gv arg
| "evince" ->
     lex_bool PP.evince arg
| "showevents" ->
     lex_tag "showevents"
        PrettyConf.parse_showevents PrettyConf.tags_showevents
        PP.showevents arg
| "graph" ->
     lex_tag "graph" Graph.parse Graph.tags PP.graph arg
| "mono" ->   lex_bool PP.mono arg
| "fontname" ->   lex_string_opt PP.fontname arg
| "fontsize" ->   lex_int_opt PP.fontsize arg
| "edgefontsizedelta" ->   lex_int PP.edgedelta arg
| "penwidth" ->  lex_float_opt PP.penwidth arg
| "arrowsize" ->  lex_float_opt PP.arrowsize arg
| "splines" ->
    lex_tag_fun
       "splines" Splines.parse Splines.tags
       (fun x -> PP.splines := Some x) arg
| "overlap" ->  lex_string_opt PP.overlap arg
| "sep" ->  lex_string_opt PP.sep arg
| "margin" -> lex_float_opt PP.margin arg
| "pad" ->  lex_float_opt PP.pad arg
| "scale" ->  lex_float PP.scale arg
| "xscale" ->  lex_float PP.xscale arg
| "yscale" ->  lex_float PP.yscale arg
| "dsiy" -> lex_float PP.dsiy arg
| "siwidth" -> lex_float PP.siwidth arg
| "boxscale" ->  lex_float PP.boxscale arg
| "ptscale" ->  lex_float PP.ptscale arg
| "squished" ->  lex_bool PP.squished arg
| "showpo" ->  lex_bool PP.showpo arg
| "relabel" ->  lex_bool PP.relabel arg
| "withbox" ->  lex_bool PP.withbox arg
| "labelbox" ->  lex_bool PP.labelbox arg
| "showfinalrf" ->  lex_bool PP.showfinalrf arg
| "showinitrf" ->  lex_bool PP.showinitrf arg
| "finalrfpos" ->  lex_pos PP.finaldotpos arg
| "initrfpos" ->  lex_pos PP.initdotpos arg
| "oneinit" ->  lex_bool PP.oneinit arg
| "labelinit" ->  lex_bool PP.labelinit arg
| "initpos" ->  lex_pos_opt PP.initpos arg
| "threadposy" ->  lex_float PP.threadposy arg
| "showinitwrites" ->  lex_bool PP.showinitwrites arg
| "showthread" ->  lex_bool PP.showthread arg
| "showlegend" ->  lex_bool PP.showlegend arg
| "brackets" ->  lex_bool PP.brackets arg
| "showobserved" ->  lex_bool PP.showobserved arg
| "movelabel" ->  lex_bool PP.movelabel arg
| "fixedsize" ->  lex_bool PP.fixedsize arg
| "extrachars" ->  lex_float PP.extrachars arg
| "dotheader" ->  PP.dotheader := Some arg
| "doshow" ->
     lex_stringsetfun PP.add_doshow arg
| "unshow" ->
     lex_stringsetfun PP.add_unshow arg
| "symetric" ->
     lex_stringset PP.symetric arg
| "classes" ->
     lex_string_opt PP.classes arg
| "showraw" ->
     lex_stringset PP.showraw arg
| "edgeattr" ->
    begin match Misc.split_comma arg with
    | [lbl;a;v;] -> PP.add_edgeattr lbl a v
    | _ ->
        error (sprintf "bad ->ument for key edgeattr: '%s'" arg)
    end
| "shift" ->
    let fs = Misc.split_comma arg in
    let fs =
      List.map
        (fun f ->
          try float_of_string f
          with _ -> error "bad argument for keyt shift: '%s' arg")
        fs in
    PP.shift := Array.of_list fs

| "edgemerge" ->
    lex_bool PP.edgemerge arg
| _ ->
    error (sprintf "Unkown key '%s' in configuration file" key)
}

let blank = [' ''\t''\r']
let not_blank = [^' ''\t''\n''\r']
let arg = ((blank* '=' blank*|blank+) (not_blank [^'\n']* as arg) blank* ('\n'|eof))

rule main = parse
| eof
    { () }
| '#' [^'\n']* '\n'
| blank* '\n'
    {  incr_lineno lexbuf; main lexbuf }
| ""
    { opt lexbuf ;  incr_lineno lexbuf; main lexbuf }

and opt = parse
| (['a'-'z''A'-'Z']+ as key) arg
   { handle_key main key arg }
| ""
   { error "Unkown key in configuration file" }
{

let lex fname = dolex main fname
}
