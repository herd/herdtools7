(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Emit C printf  (or expand to emit functions) *)

module type Config = sig
  val emitprintf : bool
  val ctr : Fmt.int_ty
  val no_file : bool
end
module Make(Cfg:Config)(O:Indent.S) : EmitPrintf.S = struct

  type sfmt = string

  open Printf
  open Fmt


  let pp_pad = function
    | No_padding -> ""
    | Some_padding (p,Zeros) -> sprintf "0%i" p
    | Some_padding (p,Left) -> sprintf "-%i" p
    | Some_padding (p,Right) -> sprintf "%i" p


  let pp_ic = function
    | Int_i -> 'i'
    | Int_x -> 'x'
    | Int_u -> 'u'

  let pp_inttype sz ic = sprintf "PRI%c%s" (pp_ic ic) sz
  let pp_int8 = pp_inttype "8"
  let pp_int16 = pp_inttype "16"
  let pp_int32 = pp_inttype "32"
  let pp_int64 = pp_inttype "64"


  let pp_conv = function
    | Char -> "%c"
    | String -> "%s"
    | Float -> "%.2f"
    | Int (pad,ic,I) -> sprintf "%%%s%c" (pp_pad pad) (pp_ic ic)
    | Int (pad,c,I8) ->
        "%" ^ pp_pad pad ^ "\" " ^ pp_int8 c ^ " \""
    | Int (pad,c,I16) ->
        "%" ^ pp_pad pad ^ "\" " ^ pp_int16 c ^ " \""
    | Int (pad,c,I32) ->
        "%" ^ pp_pad pad ^ "\" " ^ pp_int32 c ^ " \""
    | Int (pad,c,I64) ->
        "%" ^ pp_pad pad ^ "\" " ^ pp_int64 c ^ " \""
    | Int (pad,_,CTR) ->
        "%" ^ pp_pad pad ^ "\" PCTR \""

  let pp_fmt fmt =
    let xs =
      List.map
        (function 
          | Conv c -> pp_conv c
          | Lit s -> String.escaped s
          | Percent -> "%%")
        fmt in
    sprintf "\"%s\"" (String.concat "" xs)

              
  let emit_printf out i fmt args =
    begin
      if Cfg.no_file then
        O.fx i "printf(%s%s);" (pp_fmt (LexFmt.lex fmt))
      else
        O.fx i "fprintf(%s,%s%s);" out (pp_fmt (LexFmt.lex fmt))
    end
      (String.concat ""
         (List.fold_right
            (fun a k -> ","::a::k)
            args []))

  let tr_int_conv = function
    | Int_i  -> ""
    | Int_x -> "_hex"
    | Int_u -> "_uns"

  let tr_int_ty = function
    | I -> "_int"
    | I8|I16|I32 -> "_int32"
    | I64 -> "_int64"
    | CTR -> assert false

  let rec tr_one out f a = match f with
  | Char ->  sprintf "emit_char(%s,%s);" out a
  | String -> sprintf "emit_string(%s,%s);" out a
  | Float -> sprintf "emit_double(%s,%s);" out a
  | Int (p,c,CTR) -> tr_one out (Int (p,c,Cfg.ctr)) a
  | Int (No_padding,c,i) ->
      sprintf "emit%s%s(%s,%s);" (tr_int_ty i) (tr_int_conv c) out a
  | Int (Some_padding(p,Left),c,i) ->
      sprintf "emit_pad%s%s(%s,' ',%i,%s);"
         (tr_int_ty i) (tr_int_conv c) out (-p) a
  | Int (Some_padding(p,Right),c,i) ->
      sprintf "emit_pad%s%s(%s,' ',%i,%s);"
         (tr_int_ty i) (tr_int_conv c) out p a
  | Int (Some_padding(p,Zeros),c,i) ->
      sprintf "emit_pad%s%s(%s,'0',%i,%s);"
         (tr_int_ty i) (tr_int_conv c) out p a
(*  | _ -> Warn.fatal "Missing conversion" *)

  let tr_fmt out =
    let rec tr_fmt fmt args = match fmt with
  | [] -> assert (args = []) ; []
  | f::fmt ->
      let ins,args = match f,args with
      | Percent,_ -> sprintf "emit_char(%s,'%%');" out ,args
      | Lit s,_ -> sprintf "emit_string(%s,%S);" out s,args
      | Conv c,a::args -> tr_one out c a,args
      | _,[] -> assert false in
      ins::tr_fmt fmt args in
    tr_fmt

  let emit_put out i fmt args =
    let fmt = LexFmt.lex fmt in
    let ins = tr_fmt out fmt args in
    List.iter (O.ox i) ins

  let mk_f =
    if Cfg.emitprintf then emit_printf
    else emit_put

  let fx ?(out = "out") i = mk_f out i
  let f  ?(out = "out") = mk_f out Indent.indent0
  let fi  ?(out = "out") = mk_f out Indent.indent
  let fii  ?(out = "out") = mk_f out Indent.indent2
  let fiii  ?(out = "out") = mk_f out Indent.indent3
  let fiv  ?(out = "out") = mk_f out Indent.indent4
  let fv  ?(out = "out") = mk_f out Indent.indent5

end
