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
    module A = X86Arch_litmus.Make(O)(V)
    open A
    open A.Out
    open Printf

    let is_ret _ = assert false

(* Not so nice..., the price of code sharing of
   symbConst.ml with memevents *)

(***************************************************)
(* Extract explicit [symbolic] addresses from code *)
(***************************************************)
    module G = Global_litmus      

    let internal_addr name = match name with
    | G.Addr n -> Misc.string_eq n sig_cell
    | G.Pte _|G.Phy _ -> false

    let extract_rm32 r = match r with
    |  Rm32_reg _
    |  Rm32_deref _ -> G.Set.empty
    |  Rm32_abs v ->
        let name = A.tr_global v in
        if internal_addr name then
          G.Set.empty
        else
          G.Set.singleton name

    let extract_ea ea = match ea with
    | Effaddr_rm32 r -> extract_rm32 r

    let extract_op op = match op with
    | Operand_effaddr ea -> extract_ea ea
    | Operand_immediate _ -> G.Set.empty

    let rec extract_addrs ins = match ins with
    | I_LOCK ins -> extract_addrs ins
    | I_XOR (ea,op)
    | I_OR (ea,op)
    | I_ADD (ea,op)
    | I_MOV (ea,op)
    | I_MOVB (ea,op)
    | I_MOVW (ea,op)
    | I_MOVL (ea,op)
    | I_MOVQ (ea,op)
    | I_MOVT (ea,op)
    | I_CMP (ea,op)
        ->  G.Set.union (extract_ea ea) (extract_op op)
    | I_XCHG (ea1,ea2)
    | I_XCHG_UNLOCKED (ea1,ea2)
        ->  G.Set.union (extract_ea ea1) (extract_ea ea2)
    | I_CMPXCHG (ea,_)
    | I_DEC ea
    | I_INC ea
    | I_SETNB ea
    | I_CMOVC (_,ea)
        -> extract_ea ea
    | I_NOP
    | I_READ _
    | I_JMP _
    | I_JCC _
    | I_MFENCE|I_SFENCE|I_LFENCE
    | I_MOVSD
      -> G.Set.empty

    let stable_regs _ins = A.RegSet.empty

(****************************)
(* compilation to templates *)
(****************************)

    let compile_rm32_move i o r =  match r with
    |  Rm32_reg reg ->  sprintf "^o%i" o,(i,[]),(o+1,[reg])
    |  Rm32_deref reg -> sprintf "(^i%i)" i,(i+1,[reg]),(o,[])
    |  Rm32_abs abs ->
        (let name = A.tr_global abs in
        if internal_addr name then G.pp name
        else sprintf "%%[%s]" (G.pp name)),
        (i,[]),(o,[])

    let compile_ea_move i o ea = match ea with
    | Effaddr_rm32 r -> compile_rm32_move i o r


    let compile_rm32_output i o r =  match r with
    |  Rm32_reg reg ->  sprintf "^o%i" o,(i,[reg]),(o+1,[reg])
    |  Rm32_deref reg -> sprintf "(^i%i)" i,(i+1,[reg]),(o,[])
    |  Rm32_abs abs ->
        let name = A.tr_global abs in
        sprintf "%%[%s]" (G.pp name),(i,[]),(o,[])

    let compile_ea_output i o ea = match ea with
    | Effaddr_rm32 r -> compile_rm32_output i o r


    let compile_rm32_input i r = match r with
    |  Rm32_reg reg ->  sprintf "^i%i" i,(i+1,[reg])
    |  Rm32_deref reg -> sprintf "(^i%i)" i,(i+1,[reg])
    |  Rm32_abs abs ->
        let name = A.tr_global abs in
        sprintf "%%[%s]" (G.pp name),(i,[])

    let compile_ea_input i ea = match ea with
    | Effaddr_rm32 r -> compile_rm32_input i r


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
        (Effaddr_rm32 (Rm32_abs (ParsedConstant.nameToV a)))
        (Operand_immediate i)

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

    let cmpxchg memo ea r =
      let ea1, (i,ins1),(_,outs1) = compile_ea_output 0 0 ea in
      let ea2, ins2 = sprintf "^i%i" i,[r] in
      { empty_ins with
        memo = sprintf "%s %s,%s" memo ea2 ea1;
        inputs = ins1@ins2@[EAX] ;
        outputs = outs1@[EAX] ; }
      
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

(****************)

    let emit_lbl lbl =
      { empty_ins with
        memo=sprintf "%s:" (A.Out.dump_label lbl) ;
        label = Some lbl ; branch=[Next] ; }

    let next_label () = Label.next_label "__L"

    let dec r = op_ea "decl" (Effaddr_rm32 (Rm32_reg r))

    let cmp r i =
      op_ea_input_op "cmpl"
        (Effaddr_rm32 (Rm32_reg r)) (Operand_immediate i)

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

    let emit_loop code =
      let lbl1 = next_label () in
      let lbl2 = next_label () in
      cmp loop_idx 0::
      jmp no_tr lbl2::
      emit_lbl lbl1::
      code@   
      [dec loop_idx;
       emit_lbl lbl2;
       jcc no_tr C_GT lbl1;]

    let rec do_compile_ins tr_lab ins = match ins with
    | I_NOP ->
        { empty_ins with memo = "nop"; }
    | I_LOCK ins ->
        let r = do_compile_ins tr_lab ins in
        { r with memo = "lock; "^r.memo ; }
    | I_XOR (ea,op) -> op_ea_output_op "xorl" ea op
    | I_OR (ea,op) -> op_ea_output_op "orl" ea op
    | I_ADD (ea,op) -> op_ea_output_op "addl" ea op
(* as usual, move is quite special *)
    | I_MOV (ea,op) ->  move "movl" ea op
    | I_MOVB (ea,op) ->  move "movb" ea op
    | I_MOVW (ea,op) ->  move "movw" ea op
    | I_MOVL (ea,op) ->  move "movl" ea op
    | I_MOVQ (ea,op) ->  move "movq" ea op
    | I_MOVT (ea,op) ->  move "movt" ea op
(* Trap!! ea is input only... *)
    | I_CMP (ea,op) -> op_ea_input_op "cmpl" ea op
(* ea is always output here *)
    | I_INC ea -> op_ea "incl" ea
    | I_DEC ea -> op_ea "decl" ea
    | I_SETNB ea -> op_ea "setnb" ea
(* here I fail to know *)
    | I_CMOVC (_r,_ea) -> Warn.user_error "CMOC ??"
(* here both ea are output (xchg registers ?) *)
    | I_XCHG (ea1,ea2) ->  op_ea_ea "xchgl" ea1 ea2
    | I_XCHG_UNLOCKED (ea1,ea2) -> op_ea_ea "uxch" ea1 ea2
    | I_CMPXCHG (ea,r) -> cmpxchg "cmpxchgl" ea r
(* Ouf *)
    | I_READ _ -> op_none "nop"
    | I_JMP lbl -> jmp tr_lab lbl
    | I_JCC (cond,lbl) -> jcc tr_lab cond lbl
    | I_MFENCE -> op_none "mfence"
    | I_SFENCE -> op_none "sfence"
    | I_LFENCE -> op_none "lfence"
    | I_MOVSD ->  op_none "movsd"

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
(*      debug stderr r ; *)
      r::k

    let branch_neq r i lab k = cmp r i::jcc no_tr C_NE lab::k
    let branch_eq r i lab k = cmp r i::jcc no_tr C_EQ lab::k

    let signaling_write i k = move_addr sig_cell i::k

    let emit_tb_wait _ = Warn.fatal "no time base for X86"
end
