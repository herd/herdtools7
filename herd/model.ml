(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

(* Model indentifiers *)
type cav12_opt =  { cord : bool ; strongst : bool ; }

type jade_opt = { jstrongst : bool;}

type t =
  | File of string (* To convey model filename *)
  | Minimal of bool
  | CAV12 of cav12_opt
  | Generic of AST.t

let tags =
  [
   "cav12";
   "minimal";
   "uniproc";
   "<filename>.cat";
  ]

let parse tag =
  if Filename.check_suffix tag ".mdl" || Filename.check_suffix tag ".cat" then
    Some (File tag)
  else
    match String.lowercase tag with
    | "minimal" -> Some (Minimal false)
    | "uniproc" -> Some (Minimal true)
    | "cav12" -> Some (CAV12 {cord=true; strongst=true;})
    | "cav12_nocord" -> Some (CAV12 {cord=false; strongst=true;})
    | "cav12_lightst" -> Some (CAV12 {cord=true; strongst=false;})
    | "cav12_nocord_lightst" -> Some (CAV12 {cord=false; strongst=false;})
    | _ -> None

let pp = function
  | Minimal false -> "minimal"
  | Minimal true -> "uniproc"
  | CAV12 {cord=true; strongst=true;} -> "cav12"
  | CAV12 {cord=false; strongst=true;} -> "cav12_nocord"
  | CAV12 {cord=true; strongst=false;} ->"cav12_lightst"
  | CAV12 {cord=false; strongst=false;} ->"cav12_nocord_lightst"
  | File fname -> fname
  | Generic (opts,name,_) ->
      sprintf "Generic%s(%s)"
        (ModelOption.pp opts) name


(* What to let through *)

type through =
  | ThroughAll       (* Do not retain anything *)
  | ThroughInvalid   (* Let invalid go through (ie retain uniproc violations) *)
  | ThroughNone      (* Standard behaviour *)

let tags_through = ["all";"invalid";"none";]

let parse_through tag = match String.lowercase tag with
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
  val verbose : int
  val skipchecks : StringSet.t
  val strictskip : bool
  val optace : bool
  val libfind : string -> string
end

let get_default_model a = 
match a with
| `X86 -> File "x86tso.cat"
| `MIPS -> File "mips.cat"
| `PPC ->  File "ppc.cat"
| `ARM -> File "arm.cat"
| `AArch64 -> File "doc64.cat"
| `C -> File "simple-c11.cat"
| _ ->
    Warn.user_error 
    "There is no default model for architecture %s.\nSpecify a model explicitly using the -model flag." (Archs.pp a)
