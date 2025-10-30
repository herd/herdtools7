(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

let arch = Archs.java
let endian = Endian.Little

(* a local variable, like "x" or "y", etc. *)
type reg = string
let parse_reg s = Some s
let pp_reg r = r
let reg_compare = String.compare

type varhandle = string

type barrier = AccessModes.t

let barrier_compare = compare

let pp_barrier b = match b with
| AccessModes.Volatile    -> sprintf "VarHandle.fullFence();"
| AccessModes.Acquire     -> sprintf "VarHandle.acquireFence();"
| AccessModes.Release     -> sprintf "VarHandle.releaseFence();"
| _ -> sprintf "A fence that is not included in Java VarHandle API"

let symb_reg_name r =
  let len = String.length r in
  assert (len > 0) ;
  match r.[0] with
  | '%' -> Some (String.sub r 1 (len-1))
  | _ -> None

let symb_reg r = sprintf "%%%s" r

type arch_extra_op = ArchOp.no_extra_op
type 'a arch_constr_op = 'a ArchOp.no_constr_op
type arch_op = arch_extra_op arch_constr_op
type op = arch_op Op.op

let pp_phantom_archop _ = assert false
let pp_op op = Op.pp_op op pp_phantom_archop

(* (M operation, access_mode for R, access_mode for W) *)
type rw = AccessModes.t * AccessModes.t
type rmw = op * AccessModes.t

type expression =
  | Const of int
  | LoadReg of reg
  | LoadMem of varhandle * AccessModes.t
  | CAS of varhandle * rw * expression * expression
  | Rmw of varhandle * rmw * expression
  | Op of op * expression * expression


type instruction =
  | If of expression * instruction * instruction option
  | StoreReg of reg * expression
  | StoreMem of varhandle * AccessModes.t * expression
  | Fence of barrier
  | DeclReg of reg
  | Seq of instruction list

type parsedInstruction = instruction

let dump_op =
  let open Op in
  function
    | Add -> "add"
    | Sub -> "sub"
    | Or -> "or"
    | Xor -> "xor"
    | And -> "and"
    | _ -> assert false

let rec dump_expr = function
  | Const c -> sprintf "%d" c
  | LoadReg r -> r
  | LoadMem (varhandle, accessmode) ->
      sprintf "%s.get%s()" varhandle (AccessModes.pp_access_modes accessmode)
  | Op (op , e1 , e2) ->
      sprintf "%s %s %s" (dump_expr e1) (pp_op op) (dump_expr e2)
  | CAS (varhandle, rw, expect, target) ->
      (sprintf "%s.compareAndExchange%s(%s, %s)"
        varhandle
        (match rw with
         | (AccessModes.Volatile ,  AccessModes.Volatile) -> ""
         | (AccessModes.Acquire,    AccessModes.Plain)    -> "Acquire"
         | (AccessModes.Plain,      AccessModes.Release)  -> "Release"
         | _ -> assert false)
        (dump_expr expect)
        (dump_expr target))
  | Rmw (varhandle, rmw, value) ->
    (sprintf "%s.getAnd%s(%s)"
      varhandle
      (match rmw with
       | (Op.Add, AccessModes.Volatile) ->
         "Add"
       | (Op.Add, AccessModes.Acquire) ->
         "AddAcquire"
       | (Op.Add, AccessModes.Release) ->
         "AddRelease"
       | (Op.Or, AccessModes.Volatile) ->
         "BitwiseOr"
       | (Op.Or, AccessModes.Acquire) ->
         "BitwiseOrAcquire"
       | (Op.Or, AccessModes.Release) ->
         "BitwiseOrRelease"
       | (Op.And, AccessModes.Volatile) ->
         "BitwiseAnd"
       | (Op.And, AccessModes.Acquire) ->
         "BitwiseAndAcquire"
       | (Op.And, AccessModes.Release) ->
         "BitwiseAndRelease"
       | (Op.Xor, AccessModes.Volatile) ->
         "BitwiseXor"
       | (Op.Xor, AccessModes.Acquire) ->
         "BitwiseXorAcquire"
       | (Op.Xor, AccessModes.Release) ->
         "BitwiseXorRelease"
       | _ -> assert false)
      (dump_expr value))


let rec dump_instruction  i =
  let rec dump_inst_list = function
  | [] -> ""
  | hd :: tl ->  (dump_instruction hd) ^ "\n" ^ (dump_inst_list tl)
  in
  match i with
  | If (grd, thn, els) ->
      (let e = match els with
              | None -> ""
              | Some instr -> "{\n" ^ (dump_instruction instr) ^ "\n}"
      in sprintf "if (%s) {\n %s \n} %s" (dump_expr grd) (dump_instruction thn) e )
  | StoreReg (reg, e) -> sprintf "%s = %s;" reg (dump_expr e)
  | StoreMem (vh, am, e) ->
      sprintf "%s.set%s(%s);" vh (AccessModes.pp_access_modes am) (dump_expr e)
  | Fence barrier -> pp_barrier barrier
  | DeclReg reg -> sprintf "%s;" reg
  | Seq ins_l -> sprintf "%s" (dump_inst_list ins_l)


let dump_parsedInstruction = dump_instruction
and dump_instruction_hash = dump_instruction

let pp_instruction _mode = dump_instruction

let allowed_for_symb = List.map (fun x -> "r"^(string_of_int x))
                                (Misc.interval 0 64)

let fold_regs (_fc,_fs) acc _ins  = acc
let map_regs _fc _fs ins          = ins
let fold_addrs _f acc _ins        = acc
let map_addrs _f ins              = ins
let get_next _ins                 = Warn.fatal "Java get_next not implemented"

include InstrUtils.No(struct type instr = instruction end)

include Pseudo.Make
  (struct
    type ins      = instruction
    type pins     = parsedInstruction
    type reg_arg  = reg

    (* we currently don't do anything to the AST *)
    (* let parsed_expr_tr instr = instr *)
    let parsed_tr e = e

    let get_naccesses i =
      let rec aux_count_exp n = function
      | Const _ -> n
      | LoadReg _ -> n
      | LoadMem _ -> n + 1
      | CAS _ -> n + 3
      | Rmw _ -> n + 2
      | Op (_,e1,e2) -> aux_count_exp (aux_count_exp n e1) e2

      and aux_count_ins n = function
      | If (grd, thn, els) ->
         (let k = (aux_count_exp n grd) in
          match els with
          | None -> aux_count_ins k thn
          | Some ins -> aux_count_ins (aux_count_ins k thn) ins)
      | StoreReg _ -> n
      | StoreMem (_, _, e) -> (aux_count_exp n e) + 1
      | Fence _ -> n
      | DeclReg _ -> n
      | Seq l -> aux_count_inst_list n l

      and aux_count_inst_list n = function
      | [] -> n
      | hd :: tl -> aux_count_inst_list (aux_count_ins n hd) tl
      in

      aux_count_ins 0 i

    let size_of_ins _ = 1
    let fold_labels acc _f _ins = acc
    let map_labels _f ins = ins
  end)

  let get_macro _   = assert false
  let hash_pteval _ = assert false
  let base_type     = CType.Base "int"
  let type_reg _    =  base_type

  module Instr = Instr.No(struct type instr = instruction end)
