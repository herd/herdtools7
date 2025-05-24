(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

open AST
open ASTUtils
module TimeFrame = SideEffect.TimeFrame
module SES = SideEffect.SES
module TypingRule = Instrumentation.TypingRule

let ( |: ) = Instrumentation.TypingNoInstr.use_with

type global = {
  declared_types : (ty * TimeFrame.t) IMap.t;
  constant_values : literal Storage.t;
  storage_types : (ty * global_decl_keyword) IMap.t;
  subtypes : identifier IMap.t;
  subprograms : (AST.func * SES.t) IMap.t;
  overloaded_subprograms : ISet.t IMap.t;
  expr_equiv : expr IMap.t;
}

type local = {
  constant_values : literal Storage.t;
  storage_types : (ty * local_decl_keyword) IMap.t;
  expr_equiv : expr IMap.t;
  return_type : ty option;
}

type env = { global : global; local : local }

module PPEnv = struct
  open Format

  let pp_fst pp_elt f (x, _) = pp_elt f x

  let pp_map pp_elt f m =
    let pp_sep f () = fprintf f ",@ " in
    let pp_one f (key, elt) =
      fprintf f "@[<h 2>%s |-> @[%a@]@]" key pp_elt elt
    in
    fprintf f "@[<hv 2>{@ %a}@]"
      (PP.pp_print_seq ~pp_sep pp_one)
      (IMap.to_seq m)

  let pp_iset f s =
    let pp_sep f () = fprintf f ",@ " in
    fprintf f "@[<hv 2>{@ %a}@]"
      (PP.pp_print_seq ~pp_sep pp_print_string)
      (ISet.to_seq s)

  let pp_local f { constant_values; storage_types; return_type; expr_equiv } =
    fprintf f
      "@[<v 2>Local with:@ - @[constants:@ %a@]@ - @[storage:@ %a@]@ - \
       @[return type:@ %a@]@ - @[expr equiv:@ %a@]@]"
      (Storage.pp_print PP.pp_literal)
      constant_values
      (pp_map (fun f (t, _) -> PP.pp_ty f t))
      storage_types
      (pp_print_option ~none:(fun f () -> fprintf f "none") PP.pp_ty)
      return_type (pp_map PP.pp_expr) expr_equiv

  let pp_subprogram f func_sig =
    fprintf f "@[<hov 2>%a@ -> %a@]"
      (pp_print_list ~pp_sep:pp_print_space PP.pp_typed_identifier)
      func_sig.args (pp_print_option PP.pp_ty) func_sig.return_type

  let pp_global f
      {
        constant_values;
        storage_types;
        declared_types;
        subtypes;
        subprograms;
        overloaded_subprograms;
        expr_equiv;
      } =
    fprintf f
      "@[<v 2>Global with:@ - @[constants:@ %a@]@ - @[storage:@ %a@]@ - \
       @[types:@ %a@]@ - @[subtypes:@ %a@]@ - @[subprograms:@ %a@]@ - \
       @[overloaded_subprograms:@ %a@]@ - @[expr equiv:@ %a@]@]"
      (Storage.pp_print PP.pp_literal)
      constant_values
      (pp_map (fun f (t, _) -> PP.pp_ty f t))
      storage_types
      (pp_map (pp_fst PP.pp_ty))
      declared_types (pp_map pp_print_string) subtypes
      (pp_map (fun f (p, _ses) -> pp_subprogram f p))
      subprograms (pp_map pp_iset) overloaded_subprograms (pp_map PP.pp_expr)
      expr_equiv

  let pp_env f { global; local } =
    fprintf f "@[<v 2>Env with:@ - %a@ - %a@]" pp_local local pp_global global
end

let pp_env = PPEnv.pp_env
let pp_global = PPEnv.pp_global
let pp_local = PPEnv.pp_local

(** An empty global static environment. *)
let empty_global =
  {
    declared_types = IMap.empty;
    constant_values = Storage.empty;
    storage_types = IMap.empty;
    subtypes = IMap.empty;
    subprograms = IMap.empty;
    overloaded_subprograms = IMap.empty;
    expr_equiv = IMap.empty;
  }

(** An empty local static env. *)
let empty_local =
  {
    constant_values = Storage.empty;
    storage_types = IMap.empty;
    return_type = None;
    expr_equiv = IMap.empty;
  }

let empty_local_return_type return_type = { empty_local with return_type }

(** An empty static env. *)
let empty = { local = empty_local; global = empty_global }

(* Begin WithEmptyLocal *)
let with_empty_local global =
  { global; local = empty_local } |: TypingRule.WithEmptyLocal
(* End *)

(** [lookup x env] is the value of x as defined in environment.

(* Begin LookupConstant *)
      @raise Not_found if it is not defined inside. *)
let lookup_constant env x =
  try Storage.find x env.local.constant_values
  with Not_found ->
    Storage.find x env.global.constant_values |: TypingRule.LookupConstant
(* End *)

let lookup_constant_opt env x =
  try Some (lookup_constant env x) with Not_found -> None

(* Begin TypeOf *)

(** [type_of env "x"] is the type of ["x"] in the environment [env]. *)
let type_of env x =
  try IMap.find x env.local.storage_types |> fst |: TypingRule.TypeOf
  with Not_found -> IMap.find x env.global.storage_types |> fst
(* End *)

let type_of_opt env x = try Some (type_of env x) with Not_found -> None

(* Begin LookupImmutableExpr *)
let lookup_immutable_expr env x =
  try IMap.find x env.local.expr_equiv |: TypingRule.LookupImmutableExpr
  with Not_found -> IMap.find x env.global.expr_equiv
(* End *)

let lookup_immutable_expr_opt env x =
  try Some (lookup_immutable_expr env x) with Not_found -> None

let mem_constants env x =
  Storage.mem x env.global.constant_values
  || Storage.mem x env.local.constant_values

let add_subprogram name func_def ses env =
  let () =
    if false then
      Format.eprintf "@[Adding func %s with side effects:@ @[%a]@]@." name
        SideEffect.SES.pp_print ses
  in
  {
    env with
    global =
      {
        env.global with
        subprograms = IMap.add name (func_def, ses) env.global.subprograms;
      };
  }

let set_renamings name set env =
  {
    env with
    global =
      {
        env.global with
        overloaded_subprograms =
          IMap.add name set env.global.overloaded_subprograms;
      };
  }

let add_global_storage x ty gdk (genv : global) =
  { genv with storage_types = IMap.add x (ty, gdk) genv.storage_types }

let add_type x ty time_frame env =
  let () =
    if false then Format.eprintf "Adding type %s as %a.@." x PP.pp_ty ty
  in
  {
    env with
    global =
      {
        env.global with
        declared_types = IMap.add x (ty, time_frame) env.global.declared_types;
      };
  }

(* Begin AddLocalConstant *)
let add_local_constant name v env =
  {
    env with
    local =
      {
        env.local with
        constant_values = Storage.add name v env.local.constant_values;
      };
  }
(* End *)

(* Begin AddGlobalConstant *)
let add_global_constant name v (genv : global) =
  { genv with constant_values = Storage.add name v genv.constant_values }
(* End *)

let add_local x ty ldk env =
  let () =
    if false then Format.eprintf "Adding to env %S <- %a@." x PP.pp_ty ty
  in
  {
    env with
    local =
      {
        env.local with
        storage_types = IMap.add x (ty, ldk) env.local.storage_types;
      };
  }

(* Begin AddLocalImmutableExpr *)
let add_local_immutable_expr x e env =
  let () =
    if false then Format.eprintf "Adding to env %S <- %a@." x PP.pp_expr e
  in
  {
    env with
    local = { env.local with expr_equiv = IMap.add x e env.local.expr_equiv };
  }
  |: TypingRule.AddLocalImmutableExpr
(* End *)

(* Begin AddGlovalImmutableExpr *)
let add_global_immutable_expr x e env =
  let () =
    if false then Format.eprintf "Adding to env %S <- %a@." x PP.pp_expr e
  in
  {
    env with
    global = { env.global with expr_equiv = IMap.add x e env.global.expr_equiv };
  }
  |: TypingRule.AddGlobalImmutableExpr
(* End *)

let add_subtype s t env =
  {
    env with
    global = { env.global with subtypes = IMap.add s t env.global.subtypes };
  }

(* Begin IsGlobalUndefined *)
let is_global_undefined x (genv : global) =
  (not (IMap.mem x genv.storage_types || IMap.mem x genv.declared_types))
  |: TypingRule.IsGlobalUndefined
(* End *)

(* Begin IsLocalUndefined *)
let is_local_undefined x (lenv : local) =
  (not (IMap.mem x lenv.storage_types)) |: TypingRule.IsLocalUndefined
(* End *)

(* Begin IsUndefined *)
let is_undefined x env =
  is_global_undefined x env.global
  && is_local_undefined x env.local |: TypingRule.IsUndefined
(* End *)

(* Begin IsSubprogram *)
let is_subprogram x env =
  IMap.mem x env.global.subprograms |: TypingRule.IsSubprogram
(* End *)
