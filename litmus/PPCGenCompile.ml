(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val word : Word.t
  val memory : Memory.t
  val syncmacro : int option
  val syncconst : int
  val cautious : bool
end

module Make(V:Constant.S)(C:Config) =
  struct
    module A = PPCGenArch.Make(C)(V)
    open A
    open A.Out
    open Printf

(* Ready for template compilation *)
    let op3regs memo set rD rA rB =
      let memo = match set with
      | SetCR0 -> memo ^"."
      | DontSetCR0 -> memo in
      { empty_ins with
        memo=memo^ " ^o0,^i0,^i1";
        inputs=[rA; rB];
        outputs=[rD]; }

    let op2regs memo set rD rS =
      let memo = match set with
      | SetCR0 -> memo ^"."
      | DontSetCR0 -> memo in
      { empty_ins with
        memo=memo^ " ^o0,^i0";
        inputs=[rS];
        outputs=[rD]; }

    let op3or =  op3regs "or" DontSetCR0

    let op2regsI memo rD rA i =
      { empty_ins with
        memo= sprintf "%s ^o0,^i0,%i" memo i;
        inputs=[rA];
        outputs=[rD]; }

    let justOp memo = { empty_ins with memo=memo ; }

    let tr_ins = match C.word with
    | Word.W64 -> fun i -> i
    | Word.W32|Word.WXX ->
        fun i -> match i with
        | `Pld (a1,a2,a3) -> `Plwz (a1,a2,a3)
        | `Pstd (a1,a2,a3) -> `Pstw (a1,a2,a3)
        | `Pldx (a1,a2,a3) -> `Plwzx (a1,a2,a3)
        | `Pstdx (a1,a2,a3) -> `Pstwx (a1,a2,a3) 
        | _ -> i

    let emit_lbl lbl =
      { empty_ins with
        memo=sprintf "%s:" (A.Out.dump_label lbl) ;
        label = Some lbl ; branch=[Next] ; }
    
    let next_label =
      let count = ref 0 in
      fun () ->
        let lbl = sprintf "__L%i" !count in
        incr count ;
        lbl

    let li rD v =
      { empty_ins with
        memo=sprintf "li ^o0,%i" v;
        inputs=[];
        outputs=[rD]; }
      
    let mr rD rS =
      { empty_ins with
        memo="mr ^o0,^i0" ;
        inputs=[rS];
        outputs=[rD]; }

    let bcc tr_lab cond lbl =
      { empty_ins with
        memo = sprintf "b%s %s" (pp_cond cond) (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Next; Branch lbl] ; }

    let jump tr_lab lbl =
      { empty_ins with
        memo = sprintf "b %s" (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Branch lbl] ; }

(* This assumes the called subroutine does not alter any register...
    Must be wrong *)
    let jump_and_link tr_lab lbl =
      { empty_ins with
        memo = sprintf "bl %s" (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Next] ;
        outputs = [A.LR]; }

    let incr r i = op2regsI "addi" r r i
    let decr r i =  incr r (-i)

    let cmpwi rS v = 
      { empty_ins with
        memo=sprintf "cmpwi ^i0,%i" v;
        inputs=[rS];
        outputs=[]; } (* no modeling of cc needed here *)

    let cmpw rA rB =
        { empty_ins with
          memo="cmpw ^i0,^i1" ;
          inputs=[rA;rB];
          outputs=[]; }

    let cmpld rA rB =
        { empty_ins with
          memo="cmpld ^i0,^i1" ;
          inputs=[rA;rB];
          outputs=[]; }

    let dcbf rA rB =
      { empty_ins with
        memo = "dcbf ^i0,^i1" ;
        inputs=[rA;rB] ; }

    let ld rD d rA =
      { empty_ins with
        memo = sprintf "ld ^o0,%i(^i0)"  d;
        inputs = [rA];
        outputs= [rD]; }

    let std rS d rA =
      { empty_ins with
        memo = sprintf "std ^i0,%i(^i1)" d;
        inputs=[rS;rA];
        outputs=[]; }

    let stwx rS rA rB =
      { empty_ins with
        memo = "stwx ^i0,^i1,^i2";
        inputs=[rS;rA;rB];
        outputs=[]; }

    let stw rS d rA =
      { empty_ins with
        memo = sprintf "stw ^i0,%i(^i1)" d;
        inputs=[rS;rA];
        outputs=[]; }

    let mftb r =
      { empty_ins with
        memo = "mftb ^o0";
        inputs=[];
        outputs=[r]; }

    let mftbu r =
      { empty_ins with
        memo = "mftbu ^o0";
        inputs=[];
        outputs=[r]; }

    let tr_nolab lbl = lbl
      
    let emit_sync_macro k =
      let delta = C.syncconst  in
      let lbl_loop = next_label () in
      let lbl_out = next_label () in
      li idx 0::
      li ephemeral 1::
      cmpwi max_idx 0::
      bcc tr_nolab Gt lbl_loop::
      justOp "sync"::
      jump tr_nolab lbl_out::
      emit_lbl lbl_loop::
      dcbf idx base::
      justOp "sync"::
      stwx ephemeral idx base::
      incr idx delta::
      cmpw idx max_idx::
      bcc tr_nolab Lt lbl_loop::
      emit_lbl lbl_out::k

    let emit_loop k = 
        let lbl1 = next_label () in
        let lbl2 = next_label () in
        jump tr_nolab lbl2::
        emit_lbl lbl1::
        k@
        [ decr loop_idx 1;
          emit_lbl lbl2 ;
          cmpwi loop_idx 0;
          bcc tr_nolab Gt lbl1; ]

    let do_compile_ins tr_lab (ins : A.instruction) k = match tr_ins ins with
    (* Generated fix-point instructions *)
    (* #include "src_power_gen/compile.gen" *)

    | `Pb_lbl lbl -> jump tr_lab lbl::k
    | `Pbl_lbl lbl -> jump_and_link  tr_lab lbl::k 
    | `Pbcc_lbl (cond, lbl) -> bcc tr_lab cond lbl::k 
    | `Pblr_lbl -> 
        { empty_ins with
          memo = "blr" ;
          inputs = [A.LR];
          branch = []; (* Hum *) }::k

    | `Psync (0) ->
        begin match C.syncmacro with
        | None  ->justOp "sync"::k
        | Some _ -> emit_sync_macro k
        end
    | `Psync (1) -> justOp "lwsync"::k 
    | `Pdcbf (rA,rB,0) ->
        begin match rA with
        | A.Ireg A.GPR0 -> (* Yes, it's this way cf. ISA p. 415 *)
            { empty_ins with
              memo = "dcbf 0,^i0" ;
              inputs=[rB] ; }
        | _ -> dcbf rA rB
        end::k
(*     | `Plwarx (rT,rA,rB,0) -> *)
(*         begin match rA with *)
(*         | A.Ireg A.GPR0 -> *)
(*             { empty_ins with *)
(*               memo = "lwarx ^o0,0,^i0" ; *)
(*               inputs=[rB] ; *)
(*               outputs=[rT] ; } *)
(*         | _ -> *)
(*             { empty_ins with *)
(*               memo = "lwarx ^o0,^i0,^i1" ; *)
(*               inputs=[rA; rB] ; *)
(*               outputs=[rT] ; } *)
(*         end::k *)
(*     | `Pstwcx (rS,rA,rB) -> *)
(*         begin match rA with *)
(*         | A.Ireg A.GPR0 -> *)
(*             { empty_ins with *)
(*               memo = "stwcx. ^i0,0,^i1" ; *)
(*               inputs=[rS;rB] ; *)
(*               outputs=[] ; } *)
(*         | _ -> *)
(*             { empty_ins with *)
(*               memo = "stwcx. ^i0,^i1,^i2" ; *)
(*               inputs=[rS ;rA; rB] ; *)
(*               outputs=[] ; } *)
(*         end::k *)

    | `Pcomment c ->
        { empty_ins with memo = c; comment = true; }::k

    let extract_addrs _ins = StringSet.empty
    let stable_regs _ins = assert false
    let compile_ins is_before ins = do_compile_ins is_before ins

    let branch_diffw r1 r2 lab k = cmpw r1 r2::bcc tr_nolab Ne lab::k
    let branch_neq r i lab k = cmpwi r i::bcc tr_nolab Ne lab::k
    let branch_eq r i lab k = cmpwi r i::bcc tr_nolab Eq lab::k
    let signaling_write i k = li ephemeral i::stw ephemeral 0 A.signal::k


(*
sldi 9,9,32
rldicl 3,3,0,32
or 3,3,9
*)
    let sldi rD rS i =
      {empty_ins with
       memo = sprintf "sldi ^o0,^i0,%i" i ;
       inputs = [rS] ;
       outputs = [rD] ;}

    let rldicl rD rS i1 i2 =
      {empty_ins with
       memo = sprintf "rldicl ^o0,^i0,%i,%i" i1 i2 ;
       inputs = [rS] ;
       outputs = [rD] ;}


      
    let emit_mftb =
      let open Word in
      match C.word with
      | W64 ->
          fun lbl r k -> mftb tb1::k
      | W32|WXX ->
          fun lbl r k ->
            let r1 = base
            and r2 = max_idx
            and r3 = idx in
            mftbu r1::
            mftb r2::
            mftbu r3::
            branch_diffw r1 r3 lbl
              (sldi r3 r3 32::
               rldicl r2 r2 0 32::
               op3or r r3 r2::k)
                
              
          
      
    let emit_tb_wait k =
      let lbl_loop = next_label () in
      ld tb0 0 tb_addr0::
      emit_lbl lbl_loop::
      emit_mftb lbl_loop tb1
        (cmpld tb1 tb0::
         bcc tr_nolab Le lbl_loop::
         std tb1 0 tb_addr1::k)

  end
