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

type global = {
  declared_types : ty IMap.t;
  constants_values : literal IMap.t;
  storage_types : (ty * global_decl_keyword) IMap.t;
  subtypes : identifier IMap.t;
  subprograms : AST.func IMap.t;
  subprogram_renamings : ISet.t IMap.t;
}

type local = {
  constants_values : literal IMap.t;
  storage_types : (ty * local_decl_keyword) IMap.t;
  return_type : ty option;
}

type env = { global : global; local : local }

module PPEnv = struct
  open Format

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

  let pp_local f { constants_values; storage_types; return_type } =
    fprintf f
      "@[<v 2>Local with:@ - @[constants:@ %a@]@ - @[storage:@ %a@]@ - \
       @[return type:@ %a@]@]"
      (pp_map PP.pp_literal) constants_values
      (pp_map (fun f (t, _) -> PP.pp_ty f t))
      storage_types
      (pp_print_option ~none:(fun f () -> fprintf f "none") PP.pp_ty)
      return_type

  let pp_subprogram f func_sig =
    fprintf f "@[<hov 2>%a@ -> %a@]"
      (pp_print_list ~pp_sep:pp_print_space PP.pp_typed_identifier)
      func_sig.args (pp_print_option PP.pp_ty) func_sig.return_type

  let pp_global f
      {
        constants_values;
        storage_types;
        declared_types;
        subtypes;
        subprograms;
        subprogram_renamings;
      } =
    fprintf f
      "@[<v 2>Global with:@ - @[constants:@ %a@]@ - @[storage:@ %a@]@ - \
       @[types:@ %a@]@ - @[subtypes:@ %a@]@ - @[subprograms:@ %a@]@ - \
       @[subprogram_renamings:@ %a@]@]"
      (pp_map PP.pp_literal) constants_values
      (pp_map (fun f (t, _) -> PP.pp_ty f t))
      storage_types (pp_map PP.pp_ty) declared_types (pp_map pp_print_string)
      subtypes (pp_map pp_subprogram) subprograms (pp_map pp_iset)
      subprogram_renamings

  let pp_env f { global; local } =
    fprintf f "@[<v 2>Env with:@ - %a@ - %a@]" pp_local local pp_global global
end

let pp_env = PPEnv.pp_env
let pp_global = PPEnv.pp_global

(** An empty global static environment. *)
let empty_global =
  {
    declared_types = IMap.empty;
    constants_values = IMap.empty;
    storage_types = IMap.empty;
    subtypes = IMap.empty;
    subprograms = IMap.empty;
    subprogram_renamings = IMap.empty;
  }

(** An empty local static env. *)
let empty_local =
  {
    constants_values = IMap.empty;
    storage_types = IMap.empty;
    return_type = None;
  }

let empty_local_return_type return_type = { empty_local with return_type }

(** An empty static env. *)
let empty = { local = empty_local; global = empty_global }

(** [lookup x env] is the value of x as defined in environment.

      @raise Not_found if it is not defined inside. *)
let lookup_constants env x =
  try IMap.find x env.local.constants_values
  with Not_found -> IMap.find x env.global.constants_values

(** [type_of env "x"] is the type of ["x"] in the environment [env]. *)
let type_of env x =
  try IMap.find x env.local.storage_types |> fst
  with Not_found -> IMap.find x env.global.storage_types |> fst

let type_of_opt env x =
  try IMap.find x env.local.storage_types |> fst |> Option.some
  with Not_found -> IMap.find_opt x env.global.storage_types |> Option.map fst

let mem_constants env x =
  IMap.mem x env.global.constants_values
  || IMap.mem x env.local.constants_values

let add_subprogram name func_sig env =
  {
    env with
    global =
      {
        env.global with
        subprograms = IMap.add name func_sig env.global.subprograms;
      };
  }

let set_renamings name set env =
  {
    env with
    global =
      {
        env.global with
        subprogram_renamings = IMap.add name set env.global.subprogram_renamings;
      };
  }

let add_global_storage x ty gdk env =
  {
    env with
    global =
      {
        env.global with
        storage_types = IMap.add x (ty, gdk) env.global.storage_types;
      };
  }

let add_type x ty env =
  let () =
    if false then Format.eprintf "Adding type %s as %a.@." x PP.pp_ty ty
  in
  {
    env with
    global =
      {
        env.global with
        declared_types = IMap.add x ty env.global.declared_types;
      };
  }

let add_local_constant name v env =
  {
    env with
    local =
      {
        env.local with
        constants_values = IMap.add name v env.local.constants_values;
      };
  }

let add_global_constant name v env =
  {
    env with
    global =
      {
        env.global with
        constants_values = IMap.add name v env.global.constants_values;
      };
  }

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

let add_subtype s t env =
  {
    env with
    global = { env.global with subtypes = IMap.add s t env.global.subtypes };
  }

let is_undefined name env =
  not
    (IMap.mem name env.local.storage_types
    || IMap.mem name env.global.storage_types)
