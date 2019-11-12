(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module Make(V:Constant.S)(O:Arch_litmus.Config) =
  struct
    module A = X86_64Arch_litmus.Make(O)(V)
    open A
    open A.Out
    open Printf

    let is_ret _ = assert false

(* Not so nice..., the price of code sharing of
   symbConst.ml with memevents *)

    let abs_to_string abs =  ParsedConstant.vToName abs

(***************************************************)
(* Extract explicit [symbolic] addresses from code *)
(***************************************************)
    let internal_addr name = name = sig_cell

    let extract_rm64 r = match r with
    |  Rm64_reg _
    |  Rm64_deref _ -> StringSet.empty
    |  Rm64_abs v ->
        let name = abs_to_string v in
        if internal_addr name then
          StringSet.empty
        else
          StringSet.singleton name

    let extract_ea ea = match ea with
    | Effaddr_rm64 r -> extract_rm64 r

    let extract_op op = match op with
    | Operand_effaddr ea -> extract_ea ea
    | Operand_immediate _ -> StringSet.empty

    let rec extract_addrs ins = match ins with
      | I_LOCK ins -> extract_addrs ins
      | I_EFF_OP (_, _, ea, op)
        ->  StringSet.union (extract_ea ea) (extract_op op)
      | I_NOP | I_JMP _ | I_MFENCE | I_JCC _
        -> StringSet.empty
      | I_CMPXCHG (_, ea,_)
        | I_EFF (_, _, ea)
        | I_CMOVC (_, _, ea)
        -> extract_ea ea
      | I_EFF_EFF (_, _, ea1, ea2)
        ->  StringSet.union (extract_ea ea1) (extract_ea ea2)

    let stable_regs _ins = A.RegSet.empty

(****************************)
(* compilation to templates *)
(****************************)



    let compile_reg = function
      | RIP -> sprintf "%%[rip]"
      | Ireg (r, t) -> sprintf "%%%s[%s]" (reg_size_to_string t) (reg64_string r)
      | Symbolic_reg s -> s
      | Internal i -> sprintf "%i" i
      | Flag _ as f-> pp_reg f

    let compile_rm64_move i o r =  match r with
    |  Rm64_reg reg -> compile_reg reg,(i,[]),(o+1,[reg])
    |  Rm64_deref (reg,o) -> pp_offset o ^ "(" ^ compile_reg reg ^ ")",(i+1,[reg]),(o,[])
    |  Rm64_abs abs ->
        (let name = abs_to_string abs in
        if internal_addr name then name
        else sprintf "%%[%s]" name),
        (i,[]),(o,[])

    let compile_ea_move i o ea = match ea with
    | Effaddr_rm64 r -> compile_rm64_move i o r


    let compile_rm64_output i o r =  match r with
    |  Rm64_reg reg -> compile_reg reg,(i,[]),(o+1,[reg])
    |  Rm64_deref (reg,o) -> pp_offset o ^ "(" ^ compile_reg reg ^ ")",(i+1,[reg]),(o,[])
    |  Rm64_abs abs ->
        let name = abs_to_string abs in
        sprintf "%%[%s]" name,(i,[]),(o,[])

    let compile_ea_output i o ea = match ea with
    | Effaddr_rm64 r -> compile_rm64_output i o r


    let compile_rm64_input i r = match r with
    |  Rm64_reg reg -> "" ^ compile_reg reg,(i+1,[reg])
    |  Rm64_deref (reg,o) -> pp_offset o ^ "(" ^ compile_reg reg ^ ")",(i+1,[reg])
    |  Rm64_abs abs ->
        let name = abs_to_string abs in
        sprintf "%%[%s]" name,(i,[])

    let compile_ea_input i ea = match ea with
    | Effaddr_rm64 r -> compile_rm64_input i r


    let compile_op i op = match op with
    | Operand_immediate x -> sprintf "$%i" x,(i,[])
    | Operand_effaddr ea -> compile_ea_input i ea


    let op_ea_output_op memo ea op =
      let op,(i,ins1) = compile_op 0 op in
      let ea,(_,ins2),(_,outs) = compile_ea_output i 0 ea in
      { empty_ins with
        memo = sprintf "%s %s,%s" memo op ea;
        inputs = ins1@ins2;
        outputs = outs; }

    let op_ea_input_op memo ea op =
      let op,(i,ins1) = compile_op 0 op in
      let ea,(_,ins2) = compile_ea_input i ea in
      { empty_ins with
        memo = sprintf "%s %s,%s" memo op ea;
        inputs = ins1@ins2;
        outputs = [] ; }

    let move memo ea op =
       let op,(i,ins1) = compile_op 0 op in
       let ea,(_,ins2),(_,outs2) = compile_ea_move i 0 ea in
       { empty_ins with
         memo = sprintf "%s %s,%s" memo op ea;
         inputs = ins1@ins2;
         outputs=outs2; }

    let move_addr a i =
      move "movl"
        (Effaddr_rm64 (Rm64_abs (ParsedConstant.nameToV a)))
        (Operand_immediate i)

    let cmpxchg memo ea r =
      let ea1, (_,ins1),(_,outs1) = compile_ea_output 0 0 ea in
      let ea2, ins2 = compile_reg r, [r] in
      let reg_ax = match r with
        | Ireg (_, t) -> Ireg (AX, t)
        | _ -> Ireg (AX, R32b) (* default size EAX *) in
      { empty_ins with
        memo = sprintf "%s %s,%s" memo ea2 ea1;
        inputs = ins1@ins2@[reg_ax] ;
        outputs = outs1@[reg_ax] ; }

    let op_ea_ea memo ea1 ea2 =
      let ea1,(i,ins1),(o,outs1) = compile_ea_output 0 0 ea1 in
      let ea2,(_,ins2),(_,outs2) = compile_ea_output i o ea2 in
(* For exchange, order of operands is irrelevant,
   let us swap them anyway... good idea since operands
   as asymmetric in practice (ea2 must be a reg) *)
      { empty_ins with
        memo = sprintf "%s %s,%s" memo ea2 ea1;
        inputs = ins1@ins2;
        outputs = outs1@outs2; }

    let op_ea memo ea =
      let ea,(_,ins),(_,outs) = compile_ea_output 0 0 ea in
      { empty_ins with
        memo = sprintf "%s %s" memo ea ;
        inputs = ins ;
        outputs = outs ; }

    let op_none memo =
      { empty_ins with
        memo = memo;
        inputs=[];
        outputs=[]; }

    let emit_lbl lbl =
      { empty_ins with
        memo=sprintf "%s:" (A.Out.dump_label lbl) ;
        label = Some lbl ; branch=[Next] ; }

    let next_label () = Label.next_label "__L"

    let cmp r i =
      op_ea_input_op "cmpl"
        (Effaddr_rm64 (Rm64_reg r)) (Operand_immediate i)

    let jcc tr_lab cond lbl =
      {empty_ins with
       memo =
       sprintf "j%s %s"
         (Misc.lowercase (pp_condition cond))
         (A.Out.dump_label (tr_lab lbl)) ;
       label=None ; branch=[Next ; Branch lbl]; }

    let jmp tr_lab lbl =
      { empty_ins with
       memo = sprintf "jmp %s" (A.Out.dump_label (tr_lab lbl)) ;
       label=None ; branch=[Branch lbl]; }

    let no_tr lbl = lbl

    let emit_loop _k = assert false

    let inst_string inst =
      let inst_str =
        match inst with
        | I_EFF_OP (inst, size, _, _) -> pp_inst_eff_op inst size
        | I_EFF_EFF (inst, size, _, _) -> pp_inst_eff_eff inst size
        | I_EFF (inst, size, _) -> pp_inst_eff inst size
        | I_CMPXCHG (size, _, _) -> "CMPXCHG" ^ pp_inst_size size
        | _ -> assert false
      in
           String.lowercase_ascii inst_str

    let rec do_compile_ins tr_lab ins = match ins with
   | I_NOP ->
        { empty_ins with memo = "nop"; }
    | I_EFF_OP (inst, _, ea, op) as i ->
       begin
         match inst with
         | I_OR | I_ADD | I_XOR
           -> op_ea_output_op (inst_string i) ea op
         (* as usual, move is quite special *)
         | I_MOV -> move (inst_string i) ea op
         (* Trap!! ea is input only... *)
         | I_CMP
           -> op_ea_input_op (inst_string i) ea op
       end
    | I_JMP lbl -> jmp tr_lab lbl
    | I_JCC (cond, lbl) -> jcc tr_lab cond lbl
    | I_MFENCE -> op_none "mfence"
    | I_LOCK ins ->
        let r = do_compile_ins tr_lab ins in
        { r with memo = "lock; " ^ r.memo ; }
    | I_EFF (_, _, ea) as i -> op_ea (inst_string i) ea
    | I_EFF_EFF (_, _, ea1, ea2) as i
      -> op_ea_ea (inst_string i) ea1 ea2
    | I_CMPXCHG (_, ea, r) as i-> cmpxchg (inst_string i) ea r
(* here I fail to know *)
    | I_CMOVC _ -> Warn.user_error "CMOVC ??"

    let debug_regs chan rs =
      fprintf chan "%s"
        (String.concat "," (List.map reg_to_string rs))

    let debug chan t =
      fprintf chan "memo={%s}, inputs={%a}, output={%a}\n"
        t.memo
        debug_regs t.inputs
        debug_regs t.outputs

    let compile_ins tr_lab ins k =
      let r = do_compile_ins tr_lab ins in
      (*
      debug stderr r ;
       *)
      r::k

    let branch_neq _ _ _ _= Warn.fatal "Not implemented"
    let branch_eq _ _ _ _ = Warn.fatal "Not implemented"

    let signaling_write i k = move_addr sig_cell i::k

    let emit_tb_wait _ = Warn.fatal "no time base for X86"
end
