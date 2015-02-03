(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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

let lex_string_opt v arg =
  v := lex_some "string" (fun s -> s) arg

let lex_stringset v arg =
  let es =  Misc.split_comma arg in
  v := StringSet.union (StringSet.of_list es) !v

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
(* Recurse... *)
| "conf" arg { dolex main (MyLib.find arg) }
| "verbose" arg { lex_int verbose arg }
| "suffix" arg { suffix := arg }
| "dumpes" arg { lex_bool dumpes arg }
(* Change input *)
| "names" arg
   { names := !names @ [arg] }
| "rename" arg
   { rename := Some arg }
| "kinds" arg
   { kinds := !kinds @ [arg] }
| "conds" arg
   { conds := !conds @ [arg] }
(* Behaviour control *)
| "model" arg
   { lex_tag_fun
       "model" Model.parse Model.tags
       (fun x -> model := Some x) arg }
| "through" arg
   { lex_tag
       "through" Model.parse_through Model.tags_through
       through arg }
| "skipchecks" arg
   { lex_stringset skipchecks arg }
| "strictskip" arg
   { lex_bool strictskip arg }
| "unroll" arg
   { lex_int unroll arg }
| "optace" arg
   { lex_bool_fun (fun b ->  optace := Some b) arg }
| "initwrites" arg
   { lex_bool_fun (fun b ->  initwrites := Some b) arg }
| "speedcheck" arg
    { lex_tag "speedcheck" Speed.parse Speed.tags speedcheck arg }
| "badexecs" arg
    { lex_bool badexecs arg }
(* Control output *)
| "auto" arg
   { lex_bool auto arg }
| "show" arg
   { lex_tag "show"
       PrettyConf.parse_show PrettyConf.tags_show show arg }
| "nshow" arg
   { lex_int_fun (fun x -> nshow := Some x) arg }
| "restrict" arg
   { lex_tag
       "restrict" Restrict.parse Restrict.tags restrict
       arg }
| "showkind" arg { lex_bool showkind arg }
| "shortlegend" arg
    { lex_bool shortlegend arg }
| "texmacros" arg
    { lex_bool PP.texmacros arg }
| "hexa" arg
    { lex_bool PP.hexa arg }
| "outcomereads" arg
    { lex_bool outcomereads arg }
| "dotmode" arg
    { lex_tag "dotmode" PrettyConf.parse_dotmode PrettyConf.tags_dotmode
        PP.dotmode arg }
| "dotcom" arg
    { lex_tag_fun
        "dotcom" PrettyConf.parse_dotcom
        PrettyConf.tags_dotmode
        (fun x -> PP.dotcom := Some x)
        arg }
| "gv" arg
    { lex_bool PP.gv arg }
| "evince" arg
    { lex_bool PP.evince arg }
| "showevents" arg
    { lex_tag "showevents"
        PrettyConf.parse_showevents PrettyConf.tags_showevents
        PP.showevents arg }
| "graph" arg
    { lex_tag "graph" Graph.parse Graph.tags PP.graph arg }
| "mono" arg  { lex_bool PP.mono arg }
| "fontname" arg  { lex_string_opt PP.fontname arg }
| "fontsize" arg  { lex_int_opt PP.fontsize arg }
| "edgefontsizedelta" arg  { lex_int PP.edgedelta arg }
| "penwidth" arg { lex_float_opt PP.penwidth arg }
| "arrowsize" arg { lex_float_opt PP.arrowsize arg }
| "splines" arg
   { lex_tag_fun
       "splines" Splines.parse Splines.tags
       (fun x -> PP.splines := Some x) arg }
| "overlap" arg { lex_string_opt PP.overlap arg }
| "sep" arg { lex_string_opt PP.sep arg }
| "margin" arg { lex_float_opt PP.margin arg }
| "pad" arg { lex_float_opt PP.pad arg }
| "scale" arg { lex_float PP.scale arg }
| "xscale" arg { lex_float PP.xscale arg }
| "yscale" arg { lex_float PP.yscale arg }
| "ptscale" arg { lex_float PP.ptscale arg }
| "squished" arg { lex_bool PP.squished arg }
| "showpo" arg { lex_bool PP.showpo arg }
| "relabel" arg { lex_bool PP.relabel arg }
| "withbox" arg { lex_bool PP.withbox arg }
| "labelbox" arg { lex_bool PP.labelbox arg }
| "showfinalrf" arg { lex_bool PP.showfinalrf arg }
| "showinitrf" arg { lex_bool PP.showinitrf arg }
| "showpoloc" arg { lex_bool PP.showpoloc arg }
| "showfr" arg { lex_bool PP.showfr arg }
| "showinitwrites" arg { lex_bool PP.showinitwrites arg }
| "showthread" arg { lex_bool PP.showthread arg }
| "showlegend" arg { lex_bool PP.showlegend arg }
| "brackets" arg { lex_bool PP.brackets arg }
| "showobserved" arg { lex_bool PP.showobserved arg }
| "movelabel" arg { lex_bool PP.movelabel arg }
| "fixedsize" arg { lex_bool PP.fixedsize arg }
| "extrachars" arg { lex_float PP.extrachars arg }
| "dotheader" arg { PP.dotheader := Some arg }
| "doshow" arg
    { lex_stringset PP.doshow arg }
| "unshow" arg
    { lex_stringset PP.unshow arg }
| "symetric" arg
    { lex_stringset PP.symetric arg }
| "showraw" arg
    { lex_stringset PP.showraw arg }
| "edgeattr" arg
  {
    match Misc.split_comma arg with
    | [lbl;a;v;] -> PP.add_edgeattr lbl a v
    | _ ->
        error (sprintf "bad argument for key edgeattr: '%s'" arg)
   }
| "shift" arg
   {
    let fs = Misc.split_comma arg in
    let fs =
      List.map
        (fun f ->
          try float_of_string f
          with _ -> error "bad argument for keyt shift: '%s' arg")
        fs in
    PP.shift := Array.of_list fs
   }
| "edgemerge" arg
   { lex_bool PP.edgemerge arg }
(* Errors *)
| ['a'-'z''A'-'Z']+ as key
   { error (sprintf "Unkown key '%s' in configuration file" key) }
| ""
   { error "Unkown key in configuration file" }
{

let lex fname = dolex main fname

}

