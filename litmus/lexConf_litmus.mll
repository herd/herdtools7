(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
open Option
open Printf

exception Error of string

let set_intkm r arg =
  match Misc.string_of_intkm arg with
  | Some  x -> r := x
  | None ->
      raise (Error "int[kM] parameter expected")

let set_intkm_withfun set arg =
  match Misc.string_of_intkm arg with
  | Some  x -> set x
  | None ->
      raise (Error "int[kM] parameter expected")

let set_string r arg = r := arg
let set_stringo r arg = r := Some arg

let set_int_withfun set arg =
  let x =
    try int_of_string arg
    with _ ->  raise (Error "integer parameter expected") in
  set x

let set_int r arg = set_int_withfun (fun x -> r := x) arg

let set_bool_withfun set arg =
  let x =
    try bool_of_string arg
    with _ ->  raise (Error "bool parameter expected") in
  set x

let set_bool r arg = set_bool_withfun (fun x -> r := x) arg

let set_word tag = match Word.parse tag with
| Some w -> Option.set_word w
| None ->
    raise
      (Error
         (sprintf "%s is a bad word parameter (allowed: %s)"
         tag (String.concat "," Word.tags)))

module LexTag(O:ParseTag.Opt) = struct
  let lexfun_with_fun opt f =
    (fun tag -> match O.parse tag with
    | Some v -> f v
    | None ->
        raise
          (Arg.Bad
             (sprintf "%s is a bad tag for %s, allowed tag are %s"
                tag opt (String.concat "," O.tags))))

  let lexfun opt r = lexfun_with_fun opt ((:=) r)
end

}

let blank = [' ''\t''\r']
let not_blank = [^' ''\t''\n''\r']
let arg = (blank* '='? blank* (not_blank [^'\n']* as arg) blank* '\n')

rule main = parse
| eof
    { () }
| '#' [^'\n']* '\n'
| blank* '\n'
    { main lexbuf }
| "makevar" arg
    { makevar := !makevar @ [arg] ; main lexbuf }
| ("gcc_opts"|"ccopts") arg
    { set_gccopts arg ; main lexbuf }
| "gcc" arg
    { set_string gcc arg ; main lexbuf }
| "ascall" arg
    { set_bool ascall arg ; main lexbuf }
| "linkopt" arg
    { set_string linkopt arg ; main lexbuf }
| "gas" arg
    { set_bool_withfun set_gas arg ; main lexbuf }
| "asmcommentaslabel" arg
    { set_bool asmcommentaslabel arg ; main lexbuf }
| "asmcomment" arg
    { set_stringo asmcomment arg ; main lexbuf }
| "barrier" arg
    { let module P = LexTag(Barrier) in
    P.lexfun "barrier" barrier arg ; main lexbuf }
| "targetos" arg
    { let module P = LexTag(TargetOS) in
    P.lexfun "targetos" targetos arg ; main lexbuf }
| "delay" arg
    { set_int_withfun set_delay arg ; main lexbuf }
| "thread" arg
    { let module P = LexTag(ThreadStyle) in
    P.lexfun "thread" threadstyle arg ; main lexbuf }
| "detached" arg
    { set_bool_withfun
        (fun b -> threadstyle
            := (if b then ThreadStyle.Detached else ThreadStyle.Std)) arg ;
      main lexbuf }
| "launch" arg
    { let module P = LexTag(Launch) in
    P.lexfun "launch" launch arg ; main lexbuf }
| "size_of_test" arg
    { set_intkm size arg ; main lexbuf }
| "loop" arg
    { set_intkm_withfun set_timeloop arg ; main lexbuf }
| "limit" arg
    { set_bool limit arg ; main lexbuf }
| "no" arg
    { set_stringo no arg ; main lexbuf }
| "hint" arg
    { set_stringo hint arg ; main lexbuf }
| "affinity" arg
    { let module P = LexTag(Affinity) in
    P.lexfun "affinity" affinity arg ; main lexbuf }
| "procs" arg
    { set_logicalprocs arg ; main lexbuf }
| "force_affinity" arg
    { set_bool force_affinity arg ; main lexbuf }
| "number_of_run" arg
    { set_intkm Option.runs arg ; main lexbuf }
| ("memory"|"mem") arg
   { let module P = LexTag(Memory) in
   P.lexfun "memory" memory arg ; main lexbuf }
| "contiguous" arg
    { set_bool contiguous arg ; main lexbuf ; }
| "safer" arg
   { let module P = LexTag(Safer) in
   P.lexfun "safer" safer arg ; main lexbuf }
| "mode" arg
   { let module P = LexTag(Mode) in
   P.lexfun "mode" mode arg ; main lexbuf }
| "cautious" arg
   { set_bool cautious arg ; main lexbuf }
| "preload" arg
   {let module P = LexTag(Preload) in
    P.lexfun "preload" preload arg ; main lexbuf }
| "collect" arg
   {let module P = LexTag(Collect) in
    P.lexfun "collect" collect arg ; main lexbuf }
| "alloc" arg
   { let module P = LexTag(Alloc) in
   P.lexfun "alloc" alloc arg  ; main lexbuf }
| "doublealloc" arg
   { set_bool doublealloc arg ; main lexbuf }
| "carch" arg
   { let module P = LexTag(Archs.System) in
     P.lexfun_with_fun "carch" set_carch arg ; main lexbuf }
| "os" arg
   { let module P = LexTag(TargetOS) in
   P.lexfun "os" targetos arg ; main lexbuf }
| ("word"|"ws") arg
  { set_word arg ; main lexbuf }
| "avail" arg
  { set_int_withfun (fun i ->  avail := Some i) arg ; main lexbuf }
| ("st"|"stride") arg
    { let module P = LexTag(Stride) in
      P.lexfun "stride" stride arg ; main lexbuf }
| ("vb"|"verbose_barrier") arg
   { set_bool verbose_barrier arg ; main lexbuf }
| ("vp"|"verbose_prelude") arg
   { set_bool_withfun (fun b -> verbose_prelude := Some b) arg ; main lexbuf }
| "isync" arg
   { set_bool isync arg ; main lexbuf }
| ("smtmode"|"smt_mode") arg
   { let module P = LexTag(Smt) in
   P.lexfun "smtmode" smtmode arg ; main lexbuf }
| "smt" arg
   { set_int smt arg ; main lexbuf }
| "nsockets" arg
   { set_int nsockets arg ; main lexbuf }
| "speedcheck" arg
   { let module P = LexTag(Speedcheck) in
   P.lexfun "speedcheck" speedcheck arg ; main lexbuf }
| "driver" arg
   { let module P = LexTag(Driver) in
   P.lexfun "driver" driver arg ; main lexbuf }
| "morearch" arg
   { let module P = LexTag(MoreArch) in
   P.lexfun "morearch" morearch arg ; main lexbuf }
| "crossrun" arg
   { let module P = LexTag(Crossrun) in
   P.lexfun "crossrun" crossrun arg ; main lexbuf }
| "adbdir" arg
   { set_string adbdir arg ; main lexbuf }
| "pldw" arg
   { set_bool pldw arg ; main lexbuf }
| "cacheflush" arg
   { set_bool cacheflush arg ; main lexbuf }
(* Change input *)
| "names" arg
   { names := !names @ [arg] ; main lexbuf }
| "excl" arg
   { excl := !excl @ [arg] ; main lexbuf }
| "rename" arg
   { rename := !rename @ [arg] ; main lexbuf }
| "kinds" arg
   { set_kinds arg ; main lexbuf }
| "conds" arg
   { set_conds arg ; main lexbuf }
| "sleep" arg
   { set_int sleep arg ; main lexbuf }
| "exit" arg
   { set_bool exit_cond arg ; main lexbuf }
| [^'\n']* as lxm '\n'?
  { raise (Error (sprintf "format: %s" lxm)) }
{

let lex chan = main (Lexing.from_channel chan)

}
