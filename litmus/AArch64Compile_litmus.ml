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

module type Config = sig
  val word : Word.t
  val memory : Memory.t
  val cautious : bool
  val asmcomment : string option
  val hexa : bool
end

module Make(V:Constant.S)(C:Config) =
  struct
    module  A = AArch64Arch_litmus.Make(C)(V)
    open A
    open A.Out
    open CType
    open Printf

(* No addresses in code *)
    let extract_addrs _ins = StringSet.empty

    let stable_regs _ins = A.RegSet.empty


(* Handle zero reg *)
    let arg1 ppz fmt r = match r with
      | ZR -> [],ppz
      | _  -> [r],fmt "0"

    let args2 ppz fmt r1 r2 = match r1,r2 with
    | ZR,ZR -> [],ppz,[],ppz
    | ZR,_  -> [],ppz,[r2],fmt "0"
    | _,ZR  -> [r1],fmt "0",[],ppz
    | _,_ -> [r1],fmt "0",[r2],fmt "1"

    let add_type t rs = List.map (fun r -> r,t) rs
    let add_w = add_type word
    let add_q = add_type quad
(************************)
(* Template compilation *)
(************************)


(* Branches *)
    let pp_cond = function
      | EQ -> "eq"
      | NE -> "ne"


    let b tr_lab lbl =
      { empty_ins with
        memo = sprintf "b %s" (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Branch lbl] ; }

    let bcc tr_lab cond lbl =
      { empty_ins with
        memo = sprintf "b.%s %s"
          (pp_cond cond) (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Next; Branch lbl] ; }

    let cbz tr_lab memo v r lbl =
      let memo =
        sprintf
          (match v with
          | V32 -> "%s ^wi0,%s"
          | V64 -> "%s ^i0,%s")
          memo (A.Out.dump_label (tr_lab lbl)) in
      { empty_ins with
        memo; inputs=[r;]; outputs=[];
        branch=[Next; Branch lbl] ; }

(* Load and Store *)

    let ldr_memo t = Misc.lowercase (ldr_memo t)
    let str_memo t = Misc.lowercase (str_memo t)

    let load memo v rD rA kr = match v,kr with
      | V32,K 0 ->
          { empty_ins with
            memo= sprintf "%s ^wo0,[^i0]" memo;
            inputs=[rA];
            outputs=[rD];
            reg_env=[(rA,voidstar);(rD,word)]; }
      | V32,K k ->
          { empty_ins with
            memo= sprintf "%s ^wo0,[^i0,#%i]" memo k;
            inputs=[rA];
            outputs=[rD];
            reg_env=[(rA,voidstar);(rD,word)];}
      | V32,RV (V32,rB) ->
          let rB,fB = match rB with
          | ZR -> [],"wzr"
          | _  -> [rB],"^wi1" in
          { empty_ins with
            memo=memo^ sprintf " ^wo0,[^i0,%s,sxtw]" fB;
            inputs=[rA]@rB;
            outputs=[rD];
            reg_env=add_w rB@[(rA,voidstar); (rD,word);]; }
      | V64,K 0 ->
          { empty_ins with
            memo=memo ^ sprintf " ^o0,[^i0]";
            inputs=[rA];
            outputs=[rD];
            reg_env=[rA,voidstar;rD,quad;]; }
      | V64,K k ->
          { empty_ins with
            memo=memo ^ sprintf " ^o0,[^i0,#%i]" k;
            inputs=[rA];
            outputs=[rD];
            reg_env=[rA,voidstar; rD,quad;]; }
      | V64,RV (V64,rB) ->
          let rB,fB = match rB with
          | ZR -> [],"xzr"
          | _  -> [rB],"^i1" in
          { empty_ins with
            memo=memo^ sprintf " ^o0,[^i0,%s]" fB;
            inputs=[rA;]@rB;
            outputs=[rD];
            reg_env=add_q rB@[rA,voidstar;rD,quad]; }
      | V64,RV (V32,rB) ->
          let rB,fB = match rB with
          | ZR -> [],"wzr"
          | _  -> [rB],"^wi1" in
          { empty_ins with
            memo=memo ^ sprintf " ^o0,[^i0,%s,sxtw]" fB;
            inputs=[rA]@rB;
            outputs=[rD];
            reg_env=add_w rB@[rA,voidstar;rD,quad;]; }
      | V32,RV (V64,rB) ->
          let rB,fB = match rB with
          | ZR -> [],"xzr"
          | _  -> [rB],"^i1" in
          { empty_ins with
            memo=memo^ sprintf " ^wo0,[^i0,%s]" fB;
            inputs=[rA;]@rB;
            outputs=[rD];
            reg_env=add_q rB@[rA,voidstar;rD,word]; }

    let load_pair memo v rD1 rD2 rA kr = match v,kr with
      | V32,K 0 ->
          { empty_ins with
            memo= sprintf "%s ^wo0,^wo1,[^i0]" memo;
            inputs=[rA];
            outputs=[rD1;rD2;];
            reg_env=[(rA,voidstar);(rD1,word);(rD2,word);]; }
      | V32,K k ->
          { empty_ins with
            memo= sprintf "%s ^wo0,^wo1,[^i0,#%i]" memo k;
            inputs=[rA];
            outputs=[rD1;rD2;];
            reg_env=[(rA,voidstar);(rD1,word);(rD2,word);];}
      | V32,RV (V32,rB) ->
          { empty_ins with
            memo=memo^ " ^wo0,^wo1,[^i0,^wi1,sxtw]";
            inputs=[rA; rB];
            outputs=[rD1;rD2;];
            reg_env=[(rA,voidstar); (rB,word); (rD1,word);(rD2,word);]; }
      | V64,K 0 ->
          { empty_ins with
            memo=memo ^ sprintf " ^o0,^o1,[^i0]";
            inputs=[rA];
            outputs=[rD1;rD2;];
            reg_env=[rA,voidstar;(rD1,quad);(rD2,quad);]; }
      | V64,K k ->
          { empty_ins with
            memo=memo ^ sprintf " ^o0,^o1,[^i0,#%i]" k;
            inputs=[rA];
            outputs=[rD1;rD2;];
            reg_env=[rA,voidstar; (rD1,quad);(rD2,quad);]; }
      | V64,RV (V64,rB) ->
          { empty_ins with
            memo=memo^ " ^o0,^o1,[^i0,^i1]";
            inputs=[rA; rB];
            outputs=[rD1;rD2;];
            reg_env=[rA,voidstar;rB,quad;(rD1,quad);(rD2,quad)]; }
      | V64,RV (V32,rB) ->
          { empty_ins with
            memo=memo^ " ^o0,^o1,[^i0,^wi1,sxtw]";
            inputs=[rA; rB];
            outputs=[rD1;rD2;];
            reg_env=[rA,voidstar;rB,word;(rD1,quad);(rD2,quad);]; }
      | V32,RV (V64,_) -> assert false

    let store_pair memo v rD1 rD2 rA kr = match v,kr with
      | V32,K 0 ->
          { empty_ins with
            memo= sprintf "%s ^wi1,^wi2,[^i0]" memo;
            inputs=[rA;rD1;rD2;];
            outputs=[];
            reg_env=[(rA,voidstar);(rD1,word);(rD2,word);]; }
      | V32,K k ->
          { empty_ins with
            memo= sprintf "%s ^wi1,^wi2,[^i0,#%i]" memo k;
            inputs=[rA;rD1;rD2;];
            outputs=[];
            reg_env=[(rA,voidstar);(rD1,word);(rD2,word);];}
      | V32,RV (V32,rB) ->
          { empty_ins with
            memo=memo^ " ^wi2,^wi3,[^i0,^wi1,sxtw]";
            inputs=[rA;rB;rD1;rD2;];
            outputs=[];
            reg_env=[(rA,voidstar); (rB,word); (rD1,word);(rD2,word);]; }
      | V64,K 0 ->
          { empty_ins with
            memo=memo ^ sprintf " ^i1,^i2,[^i0]";
            inputs=[rA;rD1;rD2;];
            outputs=[];
            reg_env=[rA,voidstar;(rD1,quad);(rD2,quad);]; }
      | V64,K k ->
          { empty_ins with
            memo=memo ^ sprintf " ^i1,^i2,[^i0,#%i]" k;
            inputs=[rA;rD1;rD2;];
            outputs=[];
            reg_env=[rA,voidstar; (rD1,quad);(rD2,quad);]; }
      | V64,RV (V64,rB) ->
          { empty_ins with
            memo=memo^ " ^i2,^i3,[^i0,^i1]";
            inputs=[rA; rB;rD1;rD2;];
            outputs=[];
            reg_env=[rA,voidstar;rB,quad;(rD1,quad);(rD2,quad)]; }
      | V64,RV (V32,rB) ->
          { empty_ins with
            memo=memo^ " ^i2,^i3,[^i0,^wi1,sxtw]";
            inputs=[rA; rB;rD1;rD2;];
            outputs=[];
            reg_env=[rA,voidstar;rB,word;(rD1,quad);(rD2,quad);]; }
      | V32,RV (V64,_) -> assert false


    let store memo v rA rB kr = match v,kr with
      | V32,K 0 ->
          { empty_ins with
            memo=memo ^ " ^wi0,[^i1]";
            inputs=[rA;rB]; reg_env=[rB,voidstar;rA,word;]; }
      | V32,K k ->
          { empty_ins with
            memo=memo ^ sprintf " ^wi0,[^i1,#%i]" k;
            inputs=[rA;rB]; reg_env=[rB,voidstar;rA,word;]; }
      | V32,RV (V32,rC) ->
          let rC,fC = match rC with
          | ZR -> [],"wzr"
          | _  -> [rC],"^wi2" in
          { empty_ins with
            memo=memo^ sprintf " ^wi0,[^i1,%s,sxtw]" fC;
            inputs=[rA; rB;]@rC; reg_env=add_w rC@[rB,voidstar; rA,word;]; }
      | V64,K 0 ->
          { empty_ins with
            memo=memo ^ " ^i0,[^i1]";
            inputs=[rA;rB]; reg_env=[rB,voidstar; rA,quad;]; }
      | V64,K k ->
          { empty_ins with
            memo=memo ^ sprintf " ^i0,[^i1,#%i]" k;
            inputs=[rA;rB]; reg_env=[rB,voidstar; rA,quad;]; }
      | V64,RV (V64,rC) ->
          let rC,fC = match rC with
          | ZR -> [],"xzr"
          | _  -> [rC],"^i2" in
          { empty_ins with
            memo=memo ^ sprintf " ^i0,[^i1,%s]" fC;
            inputs=[rA; rB;]@rC; reg_env=add_q rC@[rB,voidstar; rA,quad;]; }
      | V64,RV (V32,rC) ->
          let rC,fC = match rC with
          | ZR -> [],"wzr"
          | _  -> [rC],"^wi2" in
          { empty_ins with
            memo=memo ^ sprintf " ^i0,[^i1,%s,sxtw]" fC;
            inputs=[rA; rB;]@rC; reg_env=add_w rC@[rB,voidstar; rA,quad;]; }
      | V32,RV (V64,rC) ->
          let rC,fC = match rC with
          | ZR -> [],"xzr"
          | _  -> [rC],"^i2" in
          { empty_ins with
            memo=memo ^ sprintf " ^wi0,[^i1,%s]" fC;
            inputs=[rA; rB;]@rC; reg_env=add_q rC@[rB,voidstar; rA,word;]; }

    let stxr memo v r1 r2 r3 = match v with
    | V32 ->
        { empty_ins with
          memo = sprintf "%s ^wo0,^wi0,[^i1]" memo ;
          inputs = [r2;r3;];
          outputs = [r1;]; reg_env=[r3,voidstar; r1,word; r2,word; ]; }
    | V64 ->
        { empty_ins with
          memo = sprintf "%s ^wo0,^i0,[^i1]" memo ;
          inputs = [r2;r3;];
          outputs = [r1;]; reg_env=[r3,voidstar; r2,quad; r1,quad; ]}

(* Compare and swap *)

    let cas_memo rmw = Misc.lowercase (cas_memo rmw)
    let casbh_memo bh rmw = Misc.lowercase (casbh_memo bh rmw)

    let cas memo v r1 r2 r3 =
      let t = match v with | V32 -> word | V64 -> quad in
      let r1,f1,r2,f2 = match v with
      | V32 -> args2 "wzr" (fun s -> "^wi"^s) r1 r2
      | V64 -> args2 "xzr" (fun s -> "^i"^s) r1 r2 in
      let idx = match r1,r2 with
      | [],[] -> "0"
      | (_::_,[])|([],_::_) -> "1"
      | _::_,_::_ -> "2" in
      { empty_ins with
        memo = sprintf "%s %s,%s,[^i%s]" memo f1 f2 idx;
        inputs = r1@r2@[r3]; outputs = r1;
        reg_env = (r3,voidstar)::add_type t (r1@r2); }

(* Swap *)
    let swp_memo rmw = Misc.lowercase (swp_memo rmw)
    let swpbh_memo bh rmw = Misc.lowercase (swpbh_memo bh rmw)

    let swp memo v r1 r2 r3 =
      let t = match v with | V32 -> word | V64 -> quad in
      let r1,f1 =
        match v with
        |  V32 -> arg1 "wzr" (fun s -> "wi"^s) r1
        |  V64 -> arg1 "xzr" (fun s -> "i"^s) r1 in
      let idx = match r1 with | [] -> "0" | _::_ -> "1" in
      let r2,f2 =
        match v with
        |  V32 -> arg1 "wzr" (fun s -> "wo"^s) r2
        |  V64 -> arg1 "xzr" (fun s -> "o"^s) r2 in
      { empty_ins with
        memo = sprintf "%s %s,^%s,[^i%s]" memo f1 f2 idx;
        inputs = r1@[r3;]; outputs =r2;
        reg_env = (r3,voidstar)::add_type t (r1@r2); }

(* Arithmetic *)
    let movk v r k =
      let memo =
        sprintf
          (match v with | V32 ->  "mov ^wo0,#%i" | V64 ->  "mov ^o0,#%i")
          k in
      { empty_ins with memo; outputs=[r;]; reg_env = ((match v with V32 -> add_w | V64 -> add_q) [r;])}

    let movr v r1 r2 = match v with
    | V32 ->
        let r1,f1 = arg1 "wzr" (fun s -> "^wo"^s) r1
        and r2,f2 = arg1 "wzr" (fun s -> "^wi"^s) r2 in
        { empty_ins with
          memo=sprintf "mov %s,%s" f1 f2;
          inputs = r2; outputs=r1; reg_env=add_w (r1@r2);}
    | V64 ->
        let r1,f1 = arg1 "xzr" (fun s -> "^o"^s) r1
        and r2,f2 = arg1 "xzr" (fun s -> "^i"^s) r2 in
        { empty_ins with
          memo=sprintf "mov %s,%s" f1 f2;
          inputs = r2; outputs=r1; reg_env=add_q (r1@r2);}


    let sxtw r1 r2 =
      { empty_ins with
        memo = "sxtw ^o0,^wi1";
        inputs = [r2;]; outputs=[r1;]; reg_env=[r1,word; r2,voidstar];}

    let cmpk v r k = match v with
    | V32 ->
        { empty_ins with
          memo = sprintf "cmp ^wi0,#%i" k ;
          inputs = [r;]; reg_env=[r,word];}
    | V64 ->
        { empty_ins with
          memo = sprintf "cmp ^i0,#%i" k ;
          inputs = [r;]; reg_env=[r,quad;];}

    let cmp v r1 r2 = match v with
    | V32 ->
        let r1,fm1,r2,fm2 = args2 "wzr" (fun s -> "^wi"^s) r1 r2 in
        let rs = r1 @ r2 in
        { empty_ins with
          memo = sprintf "cmp %s,%s" fm1 fm2;
          inputs = rs; reg_env=List.map (fun r -> r,word) rs; }
    | V64 ->
        let r1,fm1,r2,fm2 = args2 "xzr" (fun s -> "^i"^s) r1 r2 in
        let rs = r1 @ r2 in
        { empty_ins with
          memo = sprintf "cmp %s,%s" fm1 fm2;
          inputs = rs; reg_env=List.map (fun r -> r,quad) rs; }

    let memo_of_op op = Misc.lowercase (pp_op op)

    let op3 v op rD rA kr =
      let memo = memo_of_op op in
      match v,kr with
      | V32,K k ->
          let rD,fD = arg1 "wzr" (fun s -> "^wo"^s) rD
          and rA,fA = arg1 "wzr" (fun s -> "^wi"^s) rA in
          { empty_ins with
            memo=sprintf "%s %s,%s,#%i" memo fD fA k;
            inputs=rA;
            outputs=rD; reg_env = add_w (rA@rD);}
      | V32,RV (V32,rB) ->
          let rD,fD = arg1 "wzr" (fun s -> "^wo"^s) rD
          and rA,fA,rB,fB = args2 "wzr"  (fun s -> "^wi"^s) rA rB in
          let inputs = rA@rB in
          { empty_ins with
            memo=sprintf "%s %s,%s,%s" memo fD fA fB;
            inputs=inputs;
            outputs=rD; reg_env = add_w (rD@inputs);}
      | V64,K k ->
          let rD,fD = arg1 "xzr" (fun s -> "^o"^s) rD
          and rA,fA = arg1 "xzr" (fun s -> "^i"^s) rA in
          { empty_ins with
            memo=sprintf "%s %s,%s,#%i" memo fD fA k;
            inputs=rA;
            outputs=rD; reg_env = add_q (rA@rD);}
      | V64,RV (V64,rB) ->
          let rD,fD = arg1 "xzr" (fun s -> "^o"^s) rD
          and rA,fA,rB,fB = args2 "xzr"  (fun s -> "^i"^s) rA rB in
          let inputs = rA@rB in
          { empty_ins with
            memo=sprintf "%s %s,%s,%s" memo fD fA fB;
            inputs=inputs;
            outputs=rD; reg_env=add_q (inputs@rD);}
      | V64,RV (V32,rB) ->
          let rD,fD = arg1 "xzr" (fun s -> "^o"^s) rD
          and rA,fA,rB,fB = args2 "xzr"  (fun s -> "^i"^s) rA rB in
          { empty_ins with
            memo=sprintf "%s %s,%s,%s,sxtw" memo fD fA fB;
            inputs=rA@rB;
            outputs=rD; reg_env=add_q (rD@rA)@add_w rB; }
      | V32,RV (V64,_) -> assert false


    let fence f =
      { empty_ins with memo = Misc.lowercase (A.pp_barrier f); }

(* Not that useful *)
    let emit_loop _k = assert false

    let compile_ins tr_lab ins k = match ins with
(* Branches *)
    | I_B lbl -> b tr_lab lbl::k
    | I_BC (c,lbl) -> bcc tr_lab c lbl::k
    | I_CBZ (v,r,lbl) -> cbz tr_lab "cbz" v r lbl::k
    | I_CBNZ (v,r,lbl) -> cbz tr_lab "cbnz" v r lbl::k
(* Load and Store *)
    | I_LDR (v,r1,r2,kr) -> load "ldr" v r1 r2 kr::k
    | I_LDP (t,v,r1,r2,r3,kr) ->
        load_pair (match t with TT -> "ldp" | NT -> "ldnp") v r1 r2 r3 kr::k
    | I_STP (t,v,r1,r2,r3,kr) ->
        store_pair (match t with TT -> "stp" | NT -> "stnp") v r1 r2 r3 kr::k
    | I_LDRBH (B,r1,r2,kr) -> load "ldrb" V32 r1 r2 kr::k
    | I_LDRBH (H,r1,r2,kr) -> load "ldrh" V32 r1 r2 kr::k
    | I_LDAR (v,t,r1,r2) -> load (ldr_memo t) v r1 r2 k0::k
    | I_STR (v,r1,r2,kr) -> store "str" v r1 r2 kr::k
    | I_STRBH (B,r1,r2,kr) -> store "strb" V32 r1 r2 kr::k
    | I_STRBH (H,r1,r2,kr) -> store "strh" V32 r1 r2 kr::k
    | I_STLR (v,r1,r2) -> store "stlr" v r1 r2 k0::k
    | I_STXR (v,t,r1,r2,r3) -> stxr (str_memo t) v r1 r2 r3::k
    | I_CAS (v,rmw,r1,r2,r3) -> cas (cas_memo rmw) v r1 r2 r3::k
    | I_CASBH (bh,rmw,r1,r2,r3) -> cas (casbh_memo bh rmw) V32 r1 r2 r3::k
    | I_SWP (v,rmw,r1,r2,r3) -> swp (swp_memo rmw) v r1 r2 r3::k
    | I_SWPBH (bh,rmw,r1,r2,r3) -> swp (swpbh_memo bh rmw) V32 r1 r2 r3::k
(* Arithmetic *)
    | I_MOV (v,r,K i) ->  movk v r i::k
    | I_MOV (v,r1,RV (_,r2)) ->  movr v r1 r2::k
    | I_SXTW (r1,r2) -> sxtw r1 r2::k
    | I_OP3 (v,SUBS,ZR,r,K i) ->  cmpk v r i::k
    | I_OP3 (v,SUBS,ZR,r2,RV (v3,r3)) when v=v3->  cmp v r2 r3::k
    | I_OP3 (v,op,r1,r2,kr) ->  op3 v op r1 r2 kr::k
(* Fence *)
    | I_FENCE f -> fence f::k
(* Conditional selection *)
    | I_CSEL (v,r1,r2,r3,c,op) ->
        let inputs,memo,t = match v with
        | V32 ->
            let r2,f2,r3,f3 = args2 "wzr" (fun s -> "^wi"^s) r2 r3 in
            r2@r3,sprintf " ^wo0,%s,%s,%s" f2 f3 (pp_cond c),word
        | V64 ->
            let r2,f2,r3,f3 = args2 "xzr" (fun s -> "^i"^s) r2 r3 in
            r2@r3,sprintf " ^o0,%s,%s,%s" f2 f3 (pp_cond c),quad in
        let memo = String.lowercase (sel_memo op) ^ memo in
        {
         empty_ins with
         memo = memo; inputs=inputs; outputs=[r1;];
         reg_env=add_type t (r1::inputs);
        }::k

    let no_tr lbl = lbl
    let branch_neq r i lab k = cmpk V32 r i::bcc no_tr NE lab::k
    let branch_eq r i lab k = cmpk V32 r i::bcc no_tr EQ lab::k

    let signaling_write _i _k = Warn.fatal "no signaling write for ARM"

    let emit_tb_wait _ = Warn.fatal "no time base for ARM"
  end
