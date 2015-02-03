(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Define registers, barriers, and instructions for OpenCL *) 

open Printf
open Constant

(*Initial OpenCL file*)

let arch = Archs.opencl

(*************)
(* Registers *)
(*************)

type reg = string

let pp_reg r = r

let reg_compare = String.compare

let parse_reg s = Some s

(*******************)
(* Mem order stuff *)
(*******************)

type mem_order = 
| Acq
| Rel
| Acq_Rel
| SC
| Rlx
| NA

let pp_mem_order o = 
  match o with 
  | Acq -> "memory_order_acquire"
  | Rel -> "memory_order_release"
  | Acq_Rel -> "memory_order_acq_rel"
  | SC -> "memory_order_seq_cst"
  | Rlx -> "memory_order_relaxed"
  | NA -> "non_atomic"

let pp_mem_order_short = function
  | Acq -> "Acq"
  | Rel -> "Rel"
  | Acq_Rel -> "AR"
  | SC -> "Sc"
  | Rlx -> "Rlx"
  | NA -> "NA"

(*********************)
(* Scope annotations *)
(*********************)

type mem_scope = 
  | S_workitem
  | S_subgroup
  | S_workgroup
  | S_device
  | S_all_svm_devices

let pp_mem_scope s = 
  match s with 
  | S_workitem -> "wi" (* "memory_scope_work_item" *)
  | S_subgroup -> "sg" (* "memory_scope_sub_group" *)
  | S_workgroup -> "wg" (* "memory_scope_work_group" *)
  | S_device -> "dev" (* "memory_scope_device" *)
  | S_all_svm_devices -> "all" (* "memory_scope_all_svm_devices" *)

(****************)
(* Barriers     *)
(****************)

type barrier = unit
    
let all_kinds_of_barriers =  [ ]
  
let pp_barrier _b = assert false

let barrier_compare = Pervasives.compare

(****************)
(* Instructions *)
(****************)

include MemSpaceMap

type expression =
| Econstant of SymbConstant.v
| Eregister of reg
| Eassign of reg * expression
| Eop of Op.op * expression * expression
| Estore  of expression * expression * mem_order * mem_scope
| Eexchange  of expression * expression * mem_order * mem_scope
| Efetch  of Op.op * expression * expression * mem_order * mem_scope
| Eload   of expression * mem_order * mem_scope
| Ecas    of expression * expression * expression * mem_order * mem_order *  mem_scope * bool
| Efence  of gpu_memory_space list * mem_order * mem_scope
| Ecomma of expression * expression
| Eparen of expression

type instruction = 
| Pif      of expression * instruction * instruction
| Pwhile   of expression * instruction
| Pblock   of instruction list
| Pexpr    of expression
| Pbarrier of string * gpu_memory_space list * mem_scope

include Pseudo.Make
    (struct
      type ins = instruction
      type reg_arg = reg
      let get_naccesses = function 
	| _ -> 0 
       (* JPW: maybe locks/unlocks/RMWs should return something other
          than 0, but I'm not sure whether this function is
          used, so I'll leave them at 0 *)

      (* I don't think we have labels yet... *)
      let fold_labels k _ = function 
	| _ -> k

      let map_labels _ = function 
	| ins -> ins

     end)
    
let pp_sop = function
  | Concrete i -> (sprintf "%d" i)
  | _ -> "only concrete store ops supported at this time in OpenCL"

let dump_key op =
  let open Op in
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Or -> "or"
  | Xor -> "xor"
  | And -> "and"
  | _ -> assert false

let rec dump_expression e = match e with
  | Estore(loc,e,mo,ms) ->
    (match mo with 
    | NA -> sprintf "*%s = %s"  (pp_addr loc) (dump_expression e)
    | SC -> sprintf("atomic_store(%s,%s)") 
		  (dump_expression loc) (dump_expression e)
    | _ -> sprintf("atomic_store_explicit(%s,%s,%s,%s)") 
		  (dump_expression loc) (dump_expression e)
                  (pp_mem_order_short mo) (pp_mem_scope ms))
  | Eexchange(loc,e,mo,ms) ->
    (match mo with 
    | NA -> assert false
    | SC -> sprintf("atomic_exchange(%s,%s)") 
		  (dump_expression loc) (dump_expression e)
    | _ -> sprintf("atomic_exchange_explicit(%s,%s,%s,%s)") 
		  (dump_expression loc) (dump_expression e)
                  (pp_mem_order_short mo) (pp_mem_scope ms))
  | Efetch(op,loc,e,mo,ms) ->
    (match mo with 
    | NA -> assert false
    | SC -> 
        sprintf("atomic_fetch_%s(%s,%s)") 
	  (dump_key op) (dump_expression loc) (dump_expression e)
    | _ ->
        sprintf("atomic_fetch_%s_explicit(%s,%s,%s,%s)") 
	  (dump_key op) (dump_expression loc)
          (dump_expression e) (pp_mem_order_short mo)
          (pp_mem_scope ms))  
  | Eload(loc,mo,ms) ->
    (match mo with 
    | NA -> sprintf("*%s") 
		   (pp_addr loc)
    | SC -> sprintf("atomic_load(%s)") 
		  (dump_expression loc)
    | _ -> sprintf("atomic_load_explicit(%s,%s,%s)") 
		  (dump_expression loc) (pp_mem_order_short mo)
                  (pp_mem_scope ms))
  | Ecas(obj,exp,des,mo_success,mo_failure,ms,strong) ->
    sprintf("%sCAS(%s,%s,%s,%s,%s,%s)") 
      (if strong then "S" else "W")
      (dump_expression obj) (dump_expression exp) (dump_expression des) 
      (pp_mem_order_short mo_success) (pp_mem_order_short mo_failure)
      (pp_mem_scope ms)
  | Efence (spaces, mo, ms) -> sprintf("atomic_work_item_fence(%s,%s,%s)") (pp_gpu_memory_spaces spaces) (pp_mem_order_short mo) (pp_mem_scope ms)
  | Econstant i -> pp_sop i
  | Eregister reg -> pp_reg reg
  | Eassign(reg,e) -> sprintf "%s = %s" (pp_reg reg) (dump_expression e)
  | Eop (op,e1,e2) ->
      sprintf "%s %s %s" (dump_expression e1) (Op.pp_op op) (dump_expression e2)
  | Ecomma (e1,e2) -> sprintf "%s, %s" (dump_expression e1) (dump_expression e2)
  | Eparen e -> sprintf "(%s)" (dump_expression e)


and pp_addr e = dump_expression e

let rec dump_instruction i = match i with
  | Pif(e,i1,i2) -> 
    sprintf ("if(%s)%s else %s") (dump_expression e)  
      (dump_instruction i1) (dump_instruction i2)
  | Pwhile(e,i) -> 
    sprintf ("while(%s)%s") (dump_expression e) 
      (dump_instruction i)
  | Pblock insts ->
    sprintf ("{%s}") (List.fold_left (fun z i -> z ^ dump_instruction i) "" insts)
  | Pexpr e -> sprintf "%s;" (dump_expression e)
  | Pbarrier (lbl,spaces,ms) -> sprintf("work_group_barrier(%s,%s,%s)") lbl (pp_gpu_memory_spaces spaces) (pp_mem_scope ms)
   
(* We don't have symbolic registers. This should be enough *)
let fold_regs (f_reg,_f_sreg) = 
  let _fold_reg reg (y_reg, y_sreg) = f_reg reg y_reg, y_sreg in  
  fun (_y_reg,_y_sreg as c) ins -> match ins with
  | _ -> c

let map_regs f_reg _f_symb = 
  let _map_reg reg = f_reg reg in
  fun ins -> match ins with
  | _ -> ins



(* This is what ARM and X86 do *)
let norm_ins ins = ins

(*unimplemented so far*)
let get_reg_list _ins = ([], [])

let get_macro _name = Warn.fatal "OpenCL get_macro has not been implemented"
let is_data _reg _ins = Warn.fatal "OpenCL is_data has not been implemented"

let map_addrs _f _ins = Warn.fatal "OpenCL map_addrs has not been implemented"

(* No address in code, addresses are declared parameters *)
let fold_addrs _f c _i = c

let pp_instruction _m ins = dump_instruction ins

let get_next _ins = Warn.fatal "OpenCL get_next not implemented"

let allowed_for_symb = []

include ScopeTree

