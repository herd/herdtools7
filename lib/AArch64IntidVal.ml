(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2024-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

let pp_field v name l = sprintf "%s:%s" name v |> (fun v -> v :: l)

module Target_Mode = struct
  type t = TARGETED | ONEOFN

  let target_mode_of_string s =
    match s with
    | "targeted" -> TARGETED
    | "1ofN" -> ONEOFN
    | _ -> Warn.user_error "Field target_mode should be targeted or 1ofN" s

  let string_of_target_mode v =
    match v with
    | TARGETED -> "targeted"
    | ONEOFN -> "1ofN"
end

module Trigger_Mode = struct
  type t = EDGE | LEVEL

  let trigger_mode_of_string s =
    match s with
    | "edge-triggered" -> EDGE
    | "level-sensitive" -> LEVEL
    | _ -> Warn.user_error "Field trigger_mode should be edge or level" s

  let string_of_trigger_mode v =
    match v with
    | EDGE -> "edge-triggered"
    | LEVEL -> "level-sensitive"
end

type t = {
  pending : bool;
  enabled : bool;
  priority : int;
  target : Proc.t;
  target_mode : Target_Mode.t;
  trigger_mode : Trigger_Mode.t;
  }

let default = {
  pending = false;
  enabled = true;
  priority = 0;
  target = 0; (* corresponds to process P0 *)
  target_mode = Target_Mode.TARGETED;
  trigger_mode = Trigger_Mode.EDGE;
  }

let my_bool_of_string k s =
  match s with
  | "1" -> true
  | "0" -> false
  | _ -> Warn.user_error "Field %s should be 1 or 0" k

let my_string_of_bool v =
  match v with
  | true -> "1"
  | false -> "0"

let my_int_of_string k s =
  let v = try int_of_string s with
  | _ -> Warn.user_error "INTID field %s should be an integer" k
  in v

let my_string_of_int v = sprintf "%d" v

let pp_or_skip v get_field format name =
  let v_field = get_field v in
  let default_field = get_field default in
  if v_field != default_field then
    pp_field (format v_field) name
  else
    Fun.id

let pp_pending v = pp_or_skip v (fun v -> v.pending) my_string_of_bool "pending"
let pp_enabled v = pp_or_skip v (fun v -> v.enabled) my_string_of_bool "enabled"
let pp_priority v = pp_or_skip v (fun v -> v.priority) my_string_of_int "priority"
let pp_target v = pp_field (Proc.pp v.target) "affinity"
let pp_target_mode v = pp_or_skip v (fun v -> v.target_mode) Target_Mode.string_of_target_mode "target_mode"
let pp_trigger_mode v = pp_or_skip v (fun v -> v.trigger_mode) Trigger_Mode.string_of_trigger_mode "trigger_mode"

let pp v =
  let l = []
    |> pp_trigger_mode v
    |> pp_target_mode v
    |> pp_target v
    |> pp_priority v
    |> pp_enabled v
    |> pp_pending v in
  let fs = String.concat ", " l in
  sprintf "(%s)" fs

let compare =
  (fun v1 v2 -> Bool.compare v1.pending v2.pending)
    |> Misc.lex_compare (fun v1 v2 -> Bool.compare v1.enabled v2.enabled)
    |> Misc.lex_compare (fun v1 v2 -> Int.compare v1.priority v2.priority)
    |> Misc.lex_compare (fun v1 v2 -> Proc.compare v1.target v2.target)
    |> Misc.lex_compare (fun v1 v2 -> compare v1.target_mode v2.target_mode)
    |> Misc.lex_compare (fun v1 v2 -> compare v1.trigger_mode v2.trigger_mode)

let eq v1 v2 =
  Bool.equal v1.pending v2.pending &&
  Bool.equal v1.enabled v2.enabled &&
  Int.equal v1.priority v2.priority &&
  Proc.equal v1.target v2.target &&
  v1.target_mode == v2.target_mode &&
  v1.trigger_mode == v2.trigger_mode

let add_field k v p =
  match k with
  | "pending" -> { p with pending = my_bool_of_string k v }
  | "enabled" -> { p with enabled = my_bool_of_string k v }
  | "priority" -> { p with priority = my_int_of_string k v }
  | "target_mode" -> { p with target_mode = Target_Mode.target_mode_of_string v }
  | "trigger_mode" -> { p with trigger_mode = Trigger_Mode.trigger_mode_of_string v }
  | _ -> Warn.user_error "Illegal AArch64 INTID property"

let add_target_field v p = { p with target = v }

let tr p =
  let open ParsedIntidVal in
  let r = add_target_field p.target default in
  StringMap.fold add_field p.params r

let pp_norm v = pp (tr v)
