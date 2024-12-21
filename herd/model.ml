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

open Printf

(* Model indentifiers *)
type cav12_opt =  { cord : bool ; strongst : bool ; }

type jade_opt = { jstrongst : bool;}

type t =
  | File of string (* To convey model filename *)
  | CAV12 of cav12_opt
  | Generic of string * AST.t (* filename X ast *)

let tags =
  [
   "cav12";
   "<filename>.cat"
  ]

let parse tag =
  if Filename.check_suffix tag ".mdl" || Filename.check_suffix tag ".cat" then
    Some (File tag)
  else
    match Misc.lowercase tag with
    | "cav12" -> Some (CAV12 {cord=true; strongst=true;})
    | "cav12_nocord" -> Some (CAV12 {cord=false; strongst=true;})
    | "cav12_lightst" -> Some (CAV12 {cord=true; strongst=false;})
    | "cav12_nocord_lightst" -> Some (CAV12 {cord=false; strongst=false;})
    | _ -> None

let pp = function
  | CAV12 {cord=true; strongst=true;} -> "cav12"
  | CAV12 {cord=false; strongst=true;} -> "cav12_nocord"
  | CAV12 {cord=true; strongst=false;} ->"cav12_lightst"
  | CAV12 {cord=false; strongst=false;} ->"cav12_nocord_lightst"
  | File fname -> fname
  | Generic (_,(opts,name,_)) ->
      sprintf "Generic%s(%s)"
        (ModelOption.pp opts) name


(* What to let through *)

type through =
  | ThroughAll       (* Do not retain anything *)
  | ThroughInvalid   (* Let invalid go through (ie retain uniproc violations) *)
  | ThroughNone      (* Standard behaviour *)

let tags_through = ["all";"invalid";"none";]

let parse_through tag = match Misc.lowercase tag with
| "all" -> Some ThroughAll
| "invalid" -> Some ThroughInvalid
| "none" -> Some ThroughNone
| _ -> None

let pp_through = function
  | ThroughAll -> "all"
  | ThroughInvalid -> "invalid"
  | ThroughNone -> "none"


(* Common configuration *)
module type Config = sig
  val showsome : bool
  val through : through
  val debug : bool
  val debug_files : bool
  val profile: bool
  val verbose : int
  val skipchecks : StringSet.t
  val strictskip : bool
  val cycles : StringSet.t
  val optace : OptAce.t
  val libfind : string -> string
  val variant : Variant.t -> bool
  val dirty : DirtyBit.t option
end

let get_default_model variant a =
match a with
| `X86 -> File "x86tso.cat"
| `MIPS -> File "mips.cat"
| `PPC ->  File "ppc.cat"
| `ARM -> File "arm.cat"
| `BPF -> File "bpf.cat"
| `AArch64 ->
    File
      (if variant Variant.Deps then "aarch64deps.cat"
      else "aarch64.cat")
| `C -> File "c11_partialSC.cat"
| `RISCV -> File "riscv.cat"
| `X86_64 -> File "x86tso-mixed.cat"
| `ASL -> File "asl.cat"
| _ ->
    Warn.user_error
    "There is no default model for architecture %s.\nSpecify a model explicitly using the -model flag." (Archs.pp a)
