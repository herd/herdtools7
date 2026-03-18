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

let comment = "#"

module Make(O:Arch_litmus.Config)(V:Constant.S) = struct

  include MakeAArch64Base.Make(MakeAArch64Base.NoMorello)

  module V = V

(* t1 is declared (or inferred) type, t2 is type from instruction *)
  let error t1 t2 =
    let open CType in
(*          Printf.eprintf "Error %s and %s\n" (debug t1) (debug t2) ; *)
    match t1,t2 with
    | (Base
         ("int"|"ins_t"|"int16_t"|"uint16_t"|"int8_t"|"uint8_t"),
       Pointer _)
      | (Pointer _,
         Base ("int"|"ins_t"|"int16_t"|"uint16_t"|"int8_t"|"uint8_t"))  ->
       true
    | _ -> false

  let warn t1 t2 =
    let open CType in
    match t1,t2 with
    | Base ("ins_t"|"int"|"int32_t"|"uint32_t"
            |"int16_t"|"uint16_t"
            |"int8_t"|"uint8_t"),
      Base ("ins_t"|"int"|"int32_t"|"uint32_t") -> false
    | (Base "int",_)|(_,Base "int") -> true
    | _ -> false

  module FaultType = FaultType.AArch64

  let tab = Hashtbl.create 17
  let () = List.iter (fun (r,s) -> Hashtbl.add tab r s) regs

  let reg_to_string r =  match r with
  | Symbolic_reg _ -> assert false
  | Internal i -> sprintf "i%i" i
  | Vreg (vr, _) | SIMDreg vr ->
      (try Misc.lowercase (List.assoc vr vvrs) with Not_found -> assert false)
  | Preg (pr, _) | PMreg (pr, _) ->
      (try Misc.lowercase (List.assoc pr pvrs) with Not_found -> assert false)
  | Zreg (zr, _) ->
      (try Misc.lowercase (List.assoc zr vvrs) with Not_found -> assert false) (*Intentionally 'vvrs' instead of 'zvrs'*)
  | ZAreg _ -> "za"
  | _ ->
      try Misc.lowercase (Hashtbl.find tab r) with Not_found -> assert false

  include
      ArchExtra_litmus.Make(O)
      (struct
        module V = V

        type arch_reg = reg
        let arch = `AArch64
        let forbidden_regs = []
        let pp_reg = pp_reg
        let reg_compare = reg_compare
        let reg_to_string = reg_to_string
        let internal_init r v =
          let some s = Some (s,"int") in
          if reg_compare r base = 0 then some "_a->_scratch"
          else if reg_compare r max_idx = 0 then some "max_idx"
          else if reg_compare r loop_idx = 0 then some "max_loop"
          else
            let v = match v with
            | Some v -> v
            | None -> "0"
            in
            match r with
            | (Vreg _ | SIMDreg _ ) -> Some (sprintf "vdupq_n_s32(%s)" v, "int32x4_t")
            | Zreg _ -> Some (sprintf "svdup_s32(%s)" v, "svint32_t")
            | Preg _ | PMreg _ -> Some (sprintf "svdup_b32(%s)" v, "svbool_t")
            | _ ->  None

        let reg_class reg = match reg with
          | Vreg _ | SIMDreg _ | Zreg _ -> "=&w"
          | Preg _ | PMreg _ -> "=&Upl"
          | _ -> "=&r"

        let check_init b s = (if b then "+" else "=&") ^ s

        let reg_class_stable init reg = match reg with
          (* Certain Neon instructions do not affect the whole register, so we need to
             guarantee that unaffected parts are initialized to zero which basically means
             that we need to initialize whole register to zero. Several options have been
             evaluated and it seems the only robust way to achieve that is using the
             constraint "+" and the explicit initialization of the 'stable_*' variables.
             The same applies to SVE instruction with P/M (merging predicate) *)
          | Vreg _ | SIMDreg _  | Zreg _ -> "+w"
          | Preg _ | PMreg _ -> check_init init "Upl"
          | _ -> check_init init "r"
        let comment = comment

      end)

  let features = [is_atomic,"atomic"]

  let user_handler_clobber = "x29"
  let user_handler_clobbers = [ user_handler_clobber; ]

  let default_sync_handler user =
    if user then "el0_sync_64"
    else "el1h_sync"


  let vector_table is_user name =
    let el1h_sync,el0_sync_64 =
      if is_user then default_sync_handler false,name
      else name, default_sync_handler true in
    let ventry label k = ".align 7"::Printf.sprintf "b %s" label::k
    and wentry _label k =
      ".align 7"
      ::Printf.sprintf "br %s" user_handler_clobber
      ::k in
    let ( ** ) label k = ventry label k in
    let ( *** ) label k =
      if is_user then wentry label k else ventry label k in
    "adr %0,2f"::"b 1f"::
    ".align 11"::"2:"::
    "el1t_sync" ** "el1t_irq" ** "el1t_fiq" ** "el1t_error"
    ** el1h_sync ** "el1h_irq" ** "el1h_fiq" ** "el1h_error"
    ** el0_sync_64 *** "el0_irq_64" ** "el0_fiq_64" ** "el0_error_64"
    ** "el0_sync_32" ** "el0_irq_32" ** "el0_fiq_32" ** "el0_error_32"
    ** ["1:"]

  module GetInstr = struct

    type t = instruction

    let active = true

    let self_instrs = [I_NOP; I_RET None; ]

    let lower_instr i = Misc.lowercase (dump_instruction i)

    let instr_name i =
      MyName.name_as_symbol (Misc.skip_spaces (lower_instr i))

    let fun_name i = sprintf "get%s" (instr_name i)

    let dump_instr dump v =
      let open Constant in
      match v with
      | Instruction i -> instr_name i
      | Symbolic (Virtual {name=Symbol.Label(p,l);_})
          -> SkelUtil.instr_symb_id (OutUtils.fmt_lbl_var p l)
      | _ -> dump v

    module Make(O:Indent.S) = struct

      let dump i =
        O.f "static ins_t %s(void) {" (fun_name i) ;
        O.oi "ins_t *x1;" ;
        O.oi "ins_t r;" ;
        O.oi "asm __volatile__ (" ;
        O.fii "%S" "adr %[x1],0f\n\t" ;
        O.fii "%S" "ldr %w[x2],[%[x1]]\n\t" ;
        O.fii "%S" "b 1f\n" ;
        O.fii "%S" "0:\n\t" ;
        O.fii "\"%s\\n\"" (lower_instr i) ;
        O.fii "%S" "1:\n" ;
        O.oii ":[x1] \"=&r\" (x1),[x2] \"=&r\" (r)" ;
        O.oii ":" ;
        O.oii ": \"cc\", \"memory\"" ;
        O.o ");" ;
        O.oi "return r;" ;
        O.o "}" ;
        O.o ""
    end
  end
end
