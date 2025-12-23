(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025 Arm Limited and/or its affiliates                         *)
(* <open-source-office@arm.com>                                             *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let pp_exp : Format.formatter -> AST.exp -> unit =
  let open Format in
  let open AST in
  let wrapped with_parens pp =
    if with_parens then fun fmt -> fprintf fmt "(%a)" pp else pp
  in
  let pp_binop str =
    Format.pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "%s" str)
  in
  let rec do_pp_exp ~(with_parens : bool) (fmt : Format.formatter) :
      AST.exp -> unit = function
    | Op (_, op, expl) ->
        let op_str =
          begin match op with
          | Union -> " | "
          | Seq -> "; "
          | Inter -> " & "
          | Diff -> " \\ "
          | Cartesian -> " * "
          | Add -> " + "
          | Tuple -> ", "
          end
        in
        wrapped with_parens
          (pp_binop op_str (do_pp_exp ~with_parens:true))
          fmt expl
    | Op1 (_, ToId, exp) ->
        fprintf fmt "[%a]" (do_pp_exp ~with_parens:false) exp
    | Op1 (_, Inv, exp) -> fprintf fmt "%a^-1" (do_pp_exp ~with_parens:true) exp
    | Op1 (_, Comp, exp) -> fprintf fmt "~%a" (do_pp_exp ~with_parens:true) exp
    | Op1 (_, Plus, exp) -> fprintf fmt "%a+" (do_pp_exp ~with_parens:true) exp
    | Op1 (_, Star, exp) -> fprintf fmt "%a*" (do_pp_exp ~with_parens:true) exp
    | Op1 (_, Opt, exp) -> fprintf fmt "%a?" (do_pp_exp ~with_parens:true) exp
    | Konst (_, Empty _) -> fprintf fmt "empty"
    | Konst (_, Universe _) -> fprintf fmt "_"
    | App (_, fexp, exp) ->
        fprintf fmt "%a(%a)"
          (do_pp_exp ~with_parens:true)
          fexp
          (do_pp_exp ~with_parens:false)
          exp
    | Try (_, e, e2) ->
        fprintf fmt "try %a with %a"
          (do_pp_exp ~with_parens:false)
          e
          (do_pp_exp ~with_parens:false)
          e2
    | Var (_, v) -> fprintf fmt "%s" v
    | If _ -> fprintf fmt "<if>"
    | Fun _ -> fprintf fmt "<fun>"
    | _ -> fprintf fmt "<exp>"
  in
  do_pp_exp ~with_parens:false

let eval_variant_cond ~(variants : string list) : AST.variant_cond -> bool =
  let open AST in
  let rec go : AST.variant_cond -> bool = function
    | Variant v -> List.mem v variants
    | OpNot v -> not (go v)
    | OpAnd (v1, v2) -> go v1 && go v2
    | OpOr (v1, v2) -> go v1 || go v2
  in
  go
