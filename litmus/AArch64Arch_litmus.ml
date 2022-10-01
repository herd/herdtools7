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

  let features = [is_atomic,"atomic"]

  module V = V

  let tab = Hashtbl.create 17
  let () = List.iter (fun (r,s) -> Hashtbl.add tab r s) regs

  let reg_to_string r =  match r with
  | Symbolic_reg _ -> assert false
  | Internal i -> sprintf "i%i" i
  | Vreg (vr, _) | SIMDreg vr ->
      (try Misc.lowercase (List.assoc vr vvrs) with Not_found -> assert false)
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
        let internal_init r =
          let some s = Some (s,"int") in
          if reg_compare r base = 0 then some "_a->_scratch"
          else if reg_compare r max_idx = 0 then some "max_idx"
          else if reg_compare r loop_idx = 0 then some "max_loop"
          else None
        let reg_class reg = match reg with
          | Vreg _ | SIMDreg _ -> "=&w"
          | _ -> "=&r"
        let reg_class_stable reg = match reg with
          | Vreg _ | SIMDreg _ -> "+w"
          | _ -> "=r"
        let comment = comment

(* t1 is declared (or inferred) type, t2 is type from instruction *)
        let error t1 t2 =
          let open CType in
(*          Printf.eprintf "Error %s and %s\n" (debug t1) (debug t2) ; *)
          match t1,t2 with
          | (Base "int",Pointer _)
          | (Pointer _,Base "int")  ->
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

      end)

      let nop = I_NOP

      let user_handler_clobber = "x29"
      let user_handler_clobbers = [ user_handler_clobber; ]

      let vector_table is_user name =
        let el1h_sync,el0_sync_64 =
          if is_user then "el1h_sync",name
          else name,"el0_sync_64" in
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

end
