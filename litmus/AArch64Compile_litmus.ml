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
  val verbose : int
  val word : Word.t
  val memory : Memory.t
  val cautious : bool
  val asmcomment : string option
  val hexa : bool
  val mode : Mode.t
  val precision : Fault.Handling.t
end

module Make(V:Constant.S)(C:Config) =
  struct
    module  A = AArch64Arch_litmus.Make(C)(V)
    open A
    open A.Out
    open CType
    open Printf

(* Return instruction *)
    let is_ret = function
      | A.I_RET None -> true
      | _ -> false

    let is_nop = function
      | A.I_NOP -> true
      | _ -> false

    let branch lbl = A.I_B (BranchTarget.Lbl lbl)

(* No addresses in code *)
    let extract_addrs _ins = Global_litmus.Set.empty

    let stable_regs _ins = match _ins with
    | I_LDAP1 (rs,_,_,_)
    | I_LD1SP (_,rs,_,_,_)
    | I_LD2SP (_,rs,_,_,_)
    | I_LD3SP (_,rs,_,_,_)
    | I_LD4SP (_,rs,_,_,_)
    | I_ST1SP (_,rs,_,_,_)
    | I_ST2SP (_,rs,_,_,_)
    | I_ST3SP (_,rs,_,_,_)
    | I_ST4SP (_,rs,_,_,_)
    | I_LD1 (rs,_,_,_) | I_LD1M (rs,_,_)
    | I_LD2 (rs,_,_,_) | I_LD2M (rs,_,_) | I_LD2R (rs,_,_)
    | I_LD3 (rs,_,_,_) | I_LD3M (rs,_,_) | I_LD3R (rs,_,_)
    | I_LD4 (rs,_,_,_) | I_LD4M (rs,_,_) | I_LD4R (rs,_,_)
    | I_ST1 (rs,_,_,_) | I_ST1M (rs,_,_)
    | I_ST2 (rs,_,_,_) | I_ST2M (rs,_,_)
    | I_ST3 (rs,_,_,_) | I_ST3M (rs,_,_)
    | I_ST4 (rs,_,_,_) | I_ST4M (rs,_,_) ->
        A.RegSet.of_list rs
    | I_MOV_FG (r,_,_,_) -> A.RegSet.of_list [r]
    | I_MOV_VE (r,_,_,_) -> A.RegSet.of_list [r]
    (* To allow view of Scalable register via Neon one *)
    | I_ADD_SV (r,_,_)
    | I_DUP_SV (r,_,_)
    | I_MOV_SV (r,_,_)
    | I_INDEX_SI (r,_,_,_) | I_INDEX_IS (r,_,_,_) | I_INDEX_SS (r,_,_,_) | I_INDEX_II (r,_,_)
    (* Accept P/M register so assume partial update of register *)
    | I_NEG_SV (r,_,_)
    | I_MOVPRFX (r,_,_)
    | I_OP3_SV (_,r,_,_)
      -> A.RegSet.of_list [r]
    (* Reserve slice index *)
    | I_LD1SPT (_,_,r,_,_,_,_)
    | I_ST1SPT (_,_,r,_,_,_,_)
    | I_MOVA_VT (_,r,_,_,_)
      -> A.RegSet.of_list [r]
    | I_MOVA_TV (r1,_,_,r2,_)
      -> A.RegSet.of_list [r1;r2]
    | _ ->  A.RegSet.empty

(* Generic funs for zr *)
    let do_arg1 ppz fmt r k = match r with
      | ZR -> [],ppz
      | _  -> [r],fmt k

    let do_arg1i v r k =
      match v with
      | V32 -> do_arg1 "wzr" (sprintf "^wi%i") r k
      | V64 -> do_arg1 "xzr" (sprintf"^i%i") r k
      | V128 -> assert false


    let do_arg2 ppz fmt r1 r2 k =
      match r1,r2 with
      | ZR,ZR ->
         [],ppz,ppz
      | ZR,_ ->
         [r2],ppz,fmt k
      | _,ZR ->
         [r1],fmt k,ppz
      | _,_ ->
         [r1;r2],fmt k,fmt (k+1)

    let do_arg2i v r1 r2 k =
      match v with
      | V32 -> do_arg2 "wzr" (sprintf "^wi%i") r1 r2 k
      | V64 -> do_arg2 "xzr" (sprintf "^i%i") r1 r2 k
      | V128 -> assert false

    let do_arg1o v r k =
      match v with
      | V32 -> do_arg1 "wzr" (sprintf "^wo%i") r k
      | V64 -> do_arg1 "xzr" (sprintf "^o%i") r k
      | V128 -> assert false

(* Handle zero reg *)

    let arg1 ppz fmt r =  do_arg1 ppz fmt r 0

    let arg1o v r = match v with
      | V32 -> arg1 "wzr" (sprintf "^wo%i") r
      | V64 -> arg1 "xzr" (sprintf"^o%i") r
      | V128 -> assert false

    and arg1i v r =
      match v with
      | V32 -> arg1 "wzr" (sprintf "^wi%i") r
      | V64 -> arg1 "xzr" (sprintf "^i%i") r
      | V128 -> assert false

    let args2 ppz fmt r1 r2 = match r1,r2 with
    | ZR,ZR -> [],ppz,[],ppz
    | ZR,_  -> [],ppz,[r2],fmt 0
    | _,ZR  -> [r1],fmt 0,[],ppz
    | _,_ -> [r1],fmt 0,[r2],fmt 1

    let args2i v r1 r2 =
      match v with
      | V32 -> args2 "wzr" (sprintf "^wi%i") r1 r2
      | V64 -> args2 "xzr" (sprintf "^i%i") r1 r2
      | V128 -> assert false

    let args2o v r1 r2 =
      match v with
      | V32 -> args2 "wzr" (sprintf "^wo%i") r1 r2
      | V64 -> args2 "xzr" (sprintf "^o%i") r1 r2
      | V128 -> assert false

    let v2type = function
      | V32 -> word
      | V64 -> quad
      | V128 -> int32x4_t

    let add_type t rs = List.map (fun r -> r,t) rs
    let add_w = add_type word
    let add_q = add_type quad
    let add_p = add_type voidstar (* pointer *)
    let add_128 = add_type int32x4_t
    let add_v v = v2type v |> add_type
    let add_svbool_t = add_type svbool_t
    let add_svint32_t = add_type svint32_t

    (* pretty prints barrel shifters *)
    let pp_shifter = function
      | S_LSL s -> sprintf "LSL #%d" s
      | S_LSR s -> sprintf "LSR #%d" s
      | S_MSL s -> sprintf "MSL #%d" s
      | S_ASR s -> sprintf "ASR #%d" s
      | S_NOEXT  -> ""

(************************)
(* Template compilation *)
(************************)


(* Branches *)
    let pp_cond = function
      | EQ -> "eq"
      | NE -> "ne"
      | CS -> "cs"
      | CC -> "cc"
      | MI -> "mi"
      | PL -> "pl"
      | VS -> "vs"
      | VC -> "vc"
      | HI -> "hi"
      | LS -> "ls"
      | GE -> "ge"
      | LT -> "lt"
      | GT -> "gt"
      | LE -> "le"
      | AL -> "al"

    let dump_tgt tr_lab =
      let open BranchTarget in
      function
      | Lbl lbl -> Branch lbl,A.Out.dump_label (tr_lab lbl)
      | Offset o ->
         if o mod 4 <>0 then  Warn.user_error "Non aligned branch" ;
         Disp (o/4),"." ^ pp_offset o

    let b tr_lab lbl =
      let b,lbl = dump_tgt tr_lab lbl in
      { empty_ins with
        memo = sprintf "b %s" lbl;
        branch=[b;]; }

    let br r =
      { empty_ins with
        memo = "br ^i0";
        inputs = [r;]; reg_env = [r,voidstar];
        branch=[Any] ; }

    let ret r =
      { empty_ins with
        memo = "ret ^i0";
        inputs = [r;]; reg_env = [r,voidstar];
        branch=[Any] ; }

    let bl tr_lab lbl =
      let b,lbl = dump_tgt tr_lab lbl in
      { empty_ins with
        memo = sprintf "bl %s" lbl;
        inputs=[]; outputs=[];
        branch= add_next b;
        clobbers=[linkreg;]; }

    let blr r =
      { empty_ins with
        memo = "blr ^i0";
        inputs=[r;]; outputs=[];
        reg_env = [r,voidstar;];
        branch=[Any] ;  clobbers=[linkreg;]; }

    let bcc tr_lab cond lbl =
      let b,lbl = dump_tgt tr_lab lbl in
      { empty_ins with
        memo = sprintf "b.%s %s" (pp_cond cond) lbl ;
        branch=add_next b; }

    let cbz tr_lab memo v r lbl =
      let b,lbl = dump_tgt tr_lab lbl in
      let r,f = do_arg1i v r 0 in
      let memo =
        sprintf
          (match v with
          | V32|V64 -> "%s %s,%s"
          | V128 -> assert false)
          memo f lbl in
      { empty_ins with
        memo; inputs=r; outputs=[];
        reg_env=add_v v r;
        branch=add_next b; }

    let tbz tr_lab memo v r k lbl =
      let b,lbl = dump_tgt tr_lab lbl in
      let memo =
        sprintf
          (match v with
          | V32 -> "%s ^wi0,#%d, %s"
          | V64 -> "%s ^i0, #%d, %s"
          | V128 -> assert false)
          memo k lbl in
      { empty_ins with
        memo; inputs=[r;]; outputs=[];
        branch=add_next b ; }


(* Lowercase mnemonics *)
    let ldr_memo t = Misc.lowercase (ldr_memo t)
    let ldrbh_memo bh t = Misc.lowercase (ldrbh_memo bh t)
    let str_memo t = Misc.lowercase (str_memo t)
    let strbh_memo bh t = Misc.lowercase (strbh_memo bh t)
    let stp_memo t = Misc.lowercase (stp_memo t)
    let ldp_memo t = Misc.lowercase (ldp_memo t)
    let ldxp_memo t = Misc.lowercase (ldxp_memo t)
    let stxp_memo t = Misc.lowercase (stxp_memo t)
    let ldrs_memo bh = Misc.lowercase (ldrs_memo bh)
(* Load and Store *)

    let pp_mem_sext se = MemExt.pp_sext se |> Misc.lowercase

    let do_load fd td memo rD rA idx =
      let open MemExt in
      match idx with
      | Imm (0,Idx) ->
        { empty_ins with
          memo= sprintf "%s %s,[^i0]" memo fd;
          inputs=[rA];
          outputs=rD;
          reg_env=(rA,voidstar)::add_type td rD; }
      | Imm (k,Idx) ->
         { empty_ins with
           memo= sprintf "%s %s,[^i0,#%i]" memo fd k;
           inputs=[rA];
           outputs=rD;
           reg_env=(rA,voidstar)::add_type td rD; }
      | Imm (k,PostIdx) ->
         { empty_ins with
           memo= sprintf "%s %s,[^i0],#%i" memo fd k;
           inputs=[rA];
           outputs=rD@[rA;];
           reg_env=(rA,voidstar)::add_type td rD; }
      | Imm (k,PreIdx) ->
         { empty_ins with
           memo= sprintf "%s %s,[^i0,#%i]!" memo fd k;
           inputs=[rA];
           outputs=rD@[rA;];
           reg_env=(rA,voidstar)::add_type td rD; }
      | Reg (v,rB,LSL,0) ->
         let i,fi = do_arg1i v rB 1 in
         { empty_ins with
          memo=sprintf "%s %s,[^i0,%s]" memo fd fi;
          inputs=[rA]@i;
          outputs=rD;
          reg_env=add_v v i@((rA,voidstar)::add_type td rD;);}
      | Reg (v,rB,se,0) ->
         let i,fi = do_arg1i v rB 1 in
         { empty_ins with
          memo=sprintf "%s %s,[^i0,%s,%s]" memo fd fi (pp_mem_sext se);
          inputs=[rA]@i;
          outputs=rD;
          reg_env=add_v v i@((rA,voidstar)::add_type td rD;);}
      | Reg (v,rB,se,k) ->
         let i,fi = do_arg1i v rB 1 in
         { empty_ins with
           memo=
             sprintf "%s %s,[^i0,%s,%s #%d]"
               memo fd fi (pp_mem_sext se) k;
           inputs=[rA]@i;
           outputs=rD;
           reg_env=add_v v i@((rA,voidstar)::add_type td rD;);}
      | _ -> assert false

    let load memo v rD =
      let rd,fd = do_arg1o v rD 0 in
      match v with
      | V32 -> do_load fd word memo rd
      | V64 -> do_load fd quad memo rd
      | V128 -> assert false

    let load_pair memo v rD1 rD2 rA idx =
      let rD1,f1,rD2,f2 = args2o v rD1 rD2 in
      let rDS = rD1@rD2 in
      let reg_env = (rA,voidstar)::add_v v rDS in
      match idx with
    | (0,Idx) ->
        { empty_ins with
          memo= sprintf "%s %s,%s,[^i0]" memo f1 f2;
          inputs=[rA];
          outputs=rDS;
          reg_env; }
    | (k,Idx) ->
        { empty_ins with
          memo= sprintf "%s %s,%s,[^i0,#%i]" memo f1 f2 k;
          inputs=[rA];
          outputs=rDS;
          reg_env; }
    | (k,PostIdx) ->
        { empty_ins with
          memo= sprintf "%s %s %s,[^i0],#%i" memo f1 f2 k;
          inputs=[rA];
          outputs=rDS@[rA];
          reg_env; }
    | (k,PreIdx) ->
        { empty_ins with
          memo= sprintf "%s %s,%s,[^i0,#%i]!" memo f1 f2 k;
          inputs=[rA];
          outputs=rDS@[rA];
          reg_env; }

    let ldpsw rD1 rD2 rA idx = load_pair "ldpsw" V64 rD1 rD2 rA idx

    let loadx_pair memo v rD1 rD2 rA = match v with
      | V32 ->
         { empty_ins with
           memo= sprintf "%s ^wo0,^wo1,[^i0]" memo;
           inputs=[rA];
           outputs=[rD1;rD2;];
           reg_env=[(rA,voidstar);(rD1,word);(rD2,word);]; }
      | V64 ->
        { empty_ins with
           memo= sprintf "%s ^o0,^o1,[^i0]" memo;
           inputs=[rA];
           outputs=[rD1;rD2;];
           reg_env=[(rA,voidstar);(rD1,quad);(rD2,quad);]; }
      | V128 -> assert false

    let store_pair memo v rD1 rD2 rA idx  = match v,idx with
    | V32,(0,Idx) ->
        { empty_ins with
          memo= sprintf "%s ^wi1,^wi2,[^i0]" memo;
          inputs=[rA;rD1;rD2;];
          outputs=[];
          reg_env=[(rA,voidstar);(rD1,word);(rD2,word);]; }
    | V32,(k,Idx) ->
        { empty_ins with
          memo= sprintf "%s ^wi1,^wi2,[^i0,#%i]" memo k;
          inputs=[rA;rD1;rD2;];
          outputs=[];
          reg_env=[(rA,voidstar);(rD1,word);(rD2,word);];}
    | V64,(0,Idx) ->
        { empty_ins with
          memo=memo ^ sprintf " ^i1,^i2,[^i0]";
          inputs=[rA;rD1;rD2;];
          outputs=[];
          reg_env=[rA,voidstar;(rD1,quad);(rD2,quad);]; }
    | V64,(k,Idx) ->
        { empty_ins with
          memo=memo ^ sprintf " ^i1,^i2,[^i0,#%i]" k;
          inputs=[rA;rD1;rD2;];
          outputs=[];
          reg_env=[rA,voidstar; (rD1,quad);(rD2,quad);]; }
    | V32,(k,PostIdx) ->
        { empty_ins with
          memo= sprintf "%s ^wi1,^wi2,[^i0],#%i" memo k;
          inputs=[rA;rD1;rD2;];
          outputs=[rA;];
          reg_env=[(rA,voidstar);(rD1,word);(rD2,word);];}
    | V64,(k,PostIdx) ->
        { empty_ins with
          memo=memo ^ sprintf " ^i1,^i2,[^i0],#%i" k;
          inputs=[rA;rD1;rD2;];
          outputs=[rA;];
          reg_env=[rA,voidstar; (rD1,quad);(rD2,quad);]; }
    | V32,(k,PreIdx) ->
        { empty_ins with
          memo= sprintf "%s ^wi1,^wi2,[^i0,#%i]!" memo k;
          inputs=[rA;rD1;rD2;];
          outputs=[rA;];
          reg_env=[(rA,voidstar);(rD1,word);(rD2,word);];}
    | V64,(k,PreIdx) ->
        { empty_ins with
          memo=memo ^ sprintf " ^i1,^i2,[^i0,#%i]!" k;
          inputs=[rA;rD1;rD2;];
          outputs=[rA;];
          reg_env=[rA,voidstar; (rD1,quad);(rD2,quad);]; }
    | V128,(_,_) -> assert false

    let storex_pair memo v rs rt1 rt2 rn =
      match v with
      | V32 ->
         { empty_ins with
           memo=memo^ " ^wo0,^wi0,^wi1,[^i2]";
           inputs=[rt1; rt2; rn;];
           outputs=[rs;];
           reg_env=[ rn,voidstar; rs,word; rt1,word; rt2,word;];
         }
      | V64 ->
         { empty_ins with
           memo=memo^ " ^wo0,^i0,^i1,[^i2]";
           inputs=[rt1; rt2; rn;];
           outputs=[rs;];
           reg_env=[ rn,voidstar; rs,word; rt1,quad; rt2,quad; ];
         }
      | V128 -> assert false

    let zr v = match v with
    | V32 -> "wzr"
    | V64 -> "xzr"
    | V128 -> assert false

    let fmt v = match v with
    | V32 -> fun s -> "^w" ^ s
    | V64 -> fun s -> "^" ^ s
    | V128 -> assert false

    let str_arg1 vA rA = match rA with
    | ZR -> [],zr vA ,"^i0"
    | _  -> [rA],fmt vA "i0","^i1"

    let str_arg2 vA rA vC rC = match rA,rC with
    | ZR,ZR -> [],zr vA,"^i0",[],zr vC
    | ZR,_  -> [],zr vA,"^i0",[rC],fmt vC "i1"
    | _,ZR  -> [rA],fmt vA "i0","^i1",[],zr vC
    | _,_   -> [rA],fmt vA "i0","^i1",[rC],fmt vC "i2"

    let store memo v rA rB idx =
      let open MemExt in
      let iC = match rA with ZR -> 1 | _ -> 2 in
      let rA,fA,fB = str_arg1 v rA in
      match idx with
      | Imm (0,Idx) ->
         { empty_ins with
           memo=sprintf "%s %s,[%s]" memo fA fB;
           inputs=rA@[rB;]; reg_env=[rB,voidstar]@add_v v rA; }
      | Imm (k,Idx) ->
         { empty_ins with
           memo=sprintf "%s %s,[%s,#%d]" memo fA fB k;
           inputs=rA@[rB;]; reg_env=[rB,voidstar]@add_v v rA; }
      | Imm (k,PostIdx) ->
         { empty_ins with
           memo=sprintf "%s %s,[%s],#%d" memo fA fB k;
           inputs=rA@[rB;]; outputs=[rB;];
           reg_env=[rB,voidstar]@add_v v rA; }
      | Imm (k,PreIdx) ->
         { empty_ins with
           memo=sprintf "%s %s,[%s,#%d]!" memo fA fB k;
           inputs=rA@[rB;]; outputs=[rB;];
           reg_env=[rB,voidstar]@add_v v rA; }
      | Reg (vC,rC,LSL,0) ->
         let rC,fC = do_arg1i vC rC iC in
         { empty_ins with
           memo= sprintf "%s %s,[%s,%s]" memo fA fB fC;
           inputs=rA@[rB]@rC;
           reg_env=[(rB,voidstar)]@add_v v rA@add_v vC rC; }
      | Reg (vC,rC,se,0) ->
         let rC,fC = do_arg1i vC rC iC in
         { empty_ins with
           memo=
             sprintf "%s %s,[%s,%s,%s]"
               memo fA fB fC (pp_mem_sext se);
           inputs=rA@[rB]@rC;
           reg_env=[(rB,voidstar)]@add_v v rA@add_v vC rC; }
      | Reg (vC,rC,se,k) ->
         let rC,fC = do_arg1i vC rC iC in
         { empty_ins with
           memo=
             sprintf "%s %s,[%s,%s,%s,#%d]"
               memo fA fB fC (pp_mem_sext se) k;
           inputs=rA@[rB]@rC;
           reg_env=[(rB,voidstar)]@add_v v rA@add_v vC rC; }
      | _ -> assert false

    let stxr memo v r1 r2 r3 = match v with
    | V32 ->
        let r2,f2,f3 = str_arg1 V32 r2 in
        { empty_ins with
          memo = sprintf "%s ^wo0,%s,[%s]" memo f2 f3 ;
          inputs = r2@[r3;];
          outputs = [r1;]; reg_env=[r3,voidstar; r1,word;]@add_w r2; }
    | V64 ->
        let r2,f2,f3 = str_arg1 V64 r2 in
        { empty_ins with
          memo = sprintf "%s ^wo0,%s,[%s]" memo f2 f3;
          inputs = r2@[r3;];
          outputs = [r1;]; reg_env=[r3,voidstar; r1,word;]@add_q r2}
    | V128 -> assert false

(* Neon Extension Load and Store *)

    let print_simd_reg io offset i r = match r with
    | Vreg (_,s) -> "^" ^ io ^ string_of_int (i+offset) ^
      (try Misc.lowercase (List.assoc s arrange_specifier) with Not_found -> assert false)
    | _ -> assert false

    let print_vecreg v io i = "^" ^ (match v with
    | VSIMD8 -> "b"
    | VSIMD16 -> "h"
    | VSIMD32 -> "s"
    | VSIMD64 -> "d"
    | VSIMD128 -> "q")
    ^ io ^ string_of_int i

    let print_simd_list rs io offset =
       String.concat "," (List.mapi (print_simd_reg io offset) rs)

    let load_simd memo v r1 r2 idx =
      let open MemExt in
      match idx with
      | Imm(0,Idx) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i0]" memo (print_vecreg v "o" 0);
          inputs = [r2];
          outputs = [r1];
          reg_env = [(r1,int32x4_t);(r2,voidstar)]}
      | Imm (k,Idx) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i0,#%i]" memo (print_vecreg v "o" 0) k;
          inputs = [r2];
          outputs = [r1];
          reg_env = [(r1,int32x4_t);(r2,voidstar)]}
      | Imm (k,PostIdx) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i0],#%i" memo (print_vecreg v "o" 0) k;
          inputs = [r2];
          outputs = [r1];
          reg_env = [(r1,int32x4_t);(r2,voidstar)]}
      | Imm (k,PreIdx) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i0,#%i]!" memo (print_vecreg v "o" 0) k;
          inputs = [r2];
          outputs = [r1];
          reg_env = [(r1,int32x4_t);(r2,voidstar)]}
      | Reg (V32,rk,se,0) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i0,^wi1,%s]" memo (print_vecreg v "o" 0) (pp_mem_sext se);
          inputs = [r2;rk;];
          outputs = [r1];
          reg_env = [(r1,int32x4_t);(r2,voidstar);(rk,word)]}
      | Reg (V32,rk,se,k) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i0,^wi1,%s #%d]" memo (print_vecreg v "o" 0) (pp_mem_sext se) k;
          inputs = [r2;rk;];
          outputs = [r1];
          reg_env = [(r1,int32x4_t);(r2,voidstar);(rk,word)]}
      | Reg (V64,rk,se,0) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i0,^i1,%s]" memo (print_vecreg v "o" 0) (pp_mem_sext se);
          inputs = [r2;rk;];
          outputs = [r1];
          reg_env = [(r1,int32x4_t);(r2,voidstar);(rk,quad)]}
      | Reg (V64,rk,se,k) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i0,^i1,%s #%d]" memo (print_vecreg v "o" 0) (pp_mem_sext se) k;
          inputs = [r2;rk;];
          outputs = [r1];
          reg_env = [(r1,int32x4_t);(r2,voidstar);(rk,quad)]}
      | _ -> assert false


    let load_simd_s memo rs i rA kr = match kr with
    | K 0 ->
        { empty_ins with
          memo = sprintf "%s {%s}[%i],[^i0]" memo (print_simd_list rs "i" 1) i;
          inputs = rA::rs; (* rs is intentionally in 'inputs', see comment for reg_class_stable *)
          outputs = rs;
          reg_env = (add_128 rs) @ [(rA,voidstar)]}
    | K k ->
        { empty_ins with
          memo = sprintf "%s {%s}[%i],[^i0],#%i" memo (print_simd_list rs "i" 1) i k;
          inputs = rA::rs; (* rs is intentionally in 'inputs', see comment for reg_class_stable *)
          outputs = rs;
          reg_env = (add_128 rs) @ [(rA,voidstar)]}
    | RV (V64,rB) ->
        { empty_ins with
          memo = sprintf "%s {%s}[%i],[^i0],^i1" memo (print_simd_list rs "i" 2) i;
          inputs = [rA;rB;]@rs; (* rs is intentionally in 'inputs', see comment for reg_class_stable *)
          outputs = rs;
          reg_env = (add_128 rs) @ [(rA,voidstar);(rB,quad)]}
    | _ -> Warn.fatal "Illegal form of %s instruction" memo

    let load_simd_m memo rs rA kr = match kr with
    | K 0 ->
        { empty_ins with
          memo = sprintf "%s {%s},[^i0]" memo (print_simd_list rs "o" 0);
          inputs = [rA];
          outputs = rs;
          reg_env = (add_128 rs) @ [(rA,voidstar)]}
    | K k ->
        { empty_ins with
          memo = sprintf "%s {%s},[^i0],#%i" memo (print_simd_list rs "o" 0) k;
          inputs = [rA];
          outputs = rs;
          reg_env = (add_128 rs) @ [(rA,voidstar)]}
    | RV (V64,rB) ->
        { empty_ins with
          memo = sprintf "%s {%s},[^i0],^i1" memo (print_simd_list rs "o" 0);
          inputs=[rA;rB;];
          outputs = rs;
          reg_env = (add_128 rs) @ [(rA,voidstar);(rB,quad)]}
    | _ -> Warn.fatal "Illegal form of %s instruction" memo

    let load_pair_simd memo v r1 r2 r3 idx = match idx with
    | 0,Idx ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0]" memo (print_vecreg v "o" 0) (print_vecreg v "o" 1);
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | k,Idx ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0,#%i]" memo (print_vecreg v "o" 0) (print_vecreg v "o" 1) k;
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | k,PostIdx ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0],#%i" memo (print_vecreg v "o" 0) (print_vecreg v "o" 1) k;
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | k,PreIdx ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0,#%i]!" memo (print_vecreg v "o" 0) (print_vecreg v "o" 1) k;
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}

    let store_simd memo v r1 r2 idx =
      let open MemExt in
      match idx with
      | Imm (0,Idx) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i1]" memo (print_vecreg v "i" 0);
          inputs = [r1;r2];
          outputs = [];
          reg_env = [(r1,int32x4_t);(r2,voidstar)]}
      | Imm (k,Idx) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i1,#%i]" memo (print_vecreg v "i" 0) k;
          inputs = [r1;r2];
          outputs = [];
          reg_env = [(r1,int32x4_t);(r2,voidstar)]}
      | Imm (k,PostIdx) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i1],#%i" memo (print_vecreg v "i" 0) k;
          inputs = [r1;r2];
          outputs = [];
          reg_env = [(r1,int32x4_t);(r2,voidstar)]}
      | Imm (k,PreIdx) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i1,#%i]!" memo (print_vecreg v "i" 0) k;
          inputs = [r1;r2];
          outputs = [];
          reg_env = [(r1,int32x4_t);(r2,voidstar)]}
      | Reg (V32,rk,se,0) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i1,^wi2,%s]" memo (print_vecreg v "i" 0) (pp_mem_sext se);
          inputs = [r1;r2;rk];
          outputs = [];
          reg_env = [(r1,int32x4_t);(r2,voidstar);(rk,word)]}
      | Reg (V32,rk,se,k) ->
       { empty_ins with
          memo = sprintf "%s %s,[^i1,^wi2,%s #%d]" memo (print_vecreg v "i" 0) (pp_mem_sext se) k;
          inputs = [r1;r2;rk];
          outputs = [];
          reg_env = [(r1,int32x4_t);(r2,voidstar);(rk,word)]}
      | Reg (V64,rk,se,0) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i1,^i2,%s]" memo (print_vecreg v "i" 0) (pp_mem_sext se);
          inputs = [r1;r2;rk];
          outputs = [];
          reg_env = [(r1,int32x4_t);(r2,voidstar);(rk,quad)]}
      | Reg (V64,rk,se,k) ->
        { empty_ins with
          memo = sprintf "%s %s,[^i1,^i2,%s #%d]" memo (print_vecreg v "i" 0) (pp_mem_sext se) k;
          inputs = [r1;r2;rk];
          outputs = [];
          reg_env = [(r1,int32x4_t);(r2,voidstar);(rk,quad)]}
      | _ -> assert false

    let store_simd_s memo rs i rA kr = match kr with
    | K 0 ->
        { empty_ins with
          memo = sprintf "%s {%s}[%i],[^i0]" memo (print_simd_list rs "i" 1) i;
          inputs = rA :: rs;
          reg_env = (add_128 rs) @ [(rA,voidstar)]}
    | K k ->
        { empty_ins with
          memo = sprintf "%s {%s}[%i],[^i0],#%i" memo (print_simd_list rs "i" 1) i k;
          inputs = rA :: rs;
          reg_env = (add_128 rs) @ [(rA,voidstar)]}
    | RV (V64,rB) ->
        { empty_ins with
          memo = sprintf "%s {%s}[%i],[^i0],^i1" memo (print_simd_list rs "i" 2) i;
          inputs = [rA;rB;] @ rs;
          reg_env = (add_128 rs) @ [(rA,voidstar);(rB,quad)]}
    | _ -> Warn.fatal "Illegal form of %s instruction" memo

    let store_simd_m memo rs rA kr = match kr with
    | K 0 ->
      { empty_ins with
        memo = sprintf "%s {%s},[^i0]" memo (print_simd_list rs "i" 1);
        inputs = rA :: rs;
        reg_env = [(rA,voidstar)] @ (add_128 rs)}
    | K k ->
      { empty_ins with
        memo = sprintf "%s {%s},[^i0],#%i" memo (print_simd_list rs "i" 1) k;
        inputs = rA :: rs;
        reg_env = [(rA,voidstar)] @ (add_128 rs)}
    | RV (V64,rB) ->
      { empty_ins with
        memo = sprintf "%s {%s},[^i0],^i1" memo (print_simd_list rs "i" 2);
        inputs = [rA;rB;] @ rs;
        reg_env = [(rA,voidstar);(rB,quad)] @ (add_128 rs)}
    | _ -> Warn.fatal "Illegal form of %s instruction" memo

    let store_pair_simd memo v r1 r2 r3 idx = match idx with
    | 0,Idx ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2]" memo (print_vecreg v "i" 0) (print_vecreg v "i" 1);
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | k,Idx ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2,#%i]" memo (print_vecreg v "i" 0) (print_vecreg v "i" 1) k;
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | k,PostIdx ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2],#%i" memo (print_vecreg v "i" 0) (print_vecreg v "i" 1) k;
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | k,PreIdx ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2,#%i]!" memo (print_vecreg v "i" 0) (print_vecreg v "i" 1) k;
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}

    let addv_simd v r1 r2 =
      { empty_ins with
        memo = sprintf "addv %s, %s" (print_vecreg v "o" 0) (print_simd_reg "i" 0 0 r2);
        inputs = [r2];
        outputs = [r1];
        reg_env = (add_128 [r1;r2])}

    let dup_simd_v r1 v r2 =
      { empty_ins with
        memo = sprintf "dup %s, %s"
          (print_simd_reg "o" 0 0 r1)
          (match v with | V32 -> "^wi0" | V64 -> "^i0" | V128 -> assert false);
        inputs = [r2];
        outputs = [r1];
        reg_env = (add_128 [r1;]) @ ((
          match v with
          | V32 -> add_w
          | V64 -> add_q
          | V128 -> assert false) [r2;])}

    let fmov_simd_tg v1 r1 v2 r2 =
      { empty_ins with
        memo = sprintf "fmov %s, %s"
          (match v1 with | V32 -> "^wo0" | V64 -> "^o0" | V128 -> assert false)
          (print_vecreg v2 "i" 0);
        inputs = [r2];
        outputs = [r1];
        reg_env = ((match v1 with
          | V32 -> add_w
          | V64 -> add_q
          | V128 -> assert false)
        [r1;]) @ (add_128 [r2;])}

    let mov_simd_ve r1 i1 r2 i2 =
      { empty_ins with
        memo = sprintf "mov %s[%i], %s[%i]" (print_simd_reg "i" 0 0 r1) i1 (print_simd_reg "i" 0 1 r2) i2;
        inputs = [r1;r2]; (* r1 is intentionally in 'inputs', see comment for reg_class_stable *)
        outputs = [r1];
        reg_env = (add_128 [r1;r2;]);}

    let mov_simd_v r1 r2 =
      { empty_ins with
        memo = sprintf "mov %s, %s" (print_simd_reg "o" 0 0 r1) (print_simd_reg "i" 0 0 r2);
        inputs = [r2];
        outputs = [r1];
        reg_env = (add_128 [r1;r2]);}

    let mov_simd_s v r1 r2 i =
      { empty_ins with
        memo = sprintf "mov %s, %s[%i]" (print_vecreg v "o" 0) (print_simd_reg "i" 0 0 r2) i;
        inputs = [r2];
        outputs = [r1];
        reg_env = (add_128 [r1;r2])}

    let mov_simd_tg v r1 r2 i =
      { empty_ins with
        memo = sprintf "mov %s, %s[%i]"
          (match v with | V32 -> "^wo0" | V64 -> "^o0" | V128 -> assert false)
          (print_simd_reg "i" 0 0 r2)
          i;
        inputs = [r2];
        outputs = [r1];
        reg_env = ((match v with
          | V32 -> add_w
          | V64 -> add_q
          | V128 -> assert false)
        [r1;]) @ (add_128 [r2;])}

    let mov_simd_fg r1 i v r2 =
      { empty_ins with
        memo = sprintf "mov %s[%i],%s"
          (print_simd_reg "i" 0 0 r1)
          i
          (match v with | V32 -> "^wi1" | V64 -> "^i1" | V128 -> assert false);
        inputs = [r1;r2]; (* r1 is intentionally in 'inputs', see comment for reg_class_stable *)
        outputs = [r1];
        reg_env = (add_128 [r1;]) @ ((
          match v with
          | V32 -> add_w
          | V64 -> add_q
          | V128 -> assert false) [r2;])}

    let movi_s v r k = match v with
    | VSIMD64 ->
      { empty_ins with
        memo = sprintf "movi %s, #%i" (print_vecreg v "o" 0) k;
        inputs = [];
        outputs = [r;];
        reg_env = (add_128 [r])}
    | _ -> assert false

    let movi_v r k s = match s with
    | S_NOEXT ->
      { empty_ins with
        memo = sprintf "movi %s,#%i" (print_simd_reg "o" 0 0 r) k;
        inputs = [];
        outputs = [r;];
        reg_env = (add_128 [r])}
    | S_LSL(ks) ->
      { empty_ins with
        memo = sprintf "movi %s,#%i,%s" (print_simd_reg "o" 0 0 r) k (pp_shifter (S_LSL ks));
        inputs = [];
        outputs = [r;];
        reg_env = (add_128 [r])}
    | S_MSL(ks) ->
      { empty_ins with
        memo = sprintf "movi %s,#%i,%s" (print_simd_reg "o" 0 0 r) k (pp_shifter (S_MSL ks));
        inputs = [];
        outputs = [r;];
        reg_env = (add_128 [r])}
    | _ -> assert false

    let eor_simd r1 r2 r3 =
      { empty_ins with
        memo = sprintf "eor %s,%s,%s" (print_simd_reg "o" 0 0 r1) (print_simd_reg "i" 0 0 r2) (print_simd_reg "i" 0 1 r3);
        inputs = [r2;r3;];
        outputs = [r1];
        reg_env = (add_128 [r1;r2;r3;])}

    let add_simd r1 r2 r3 =
      { empty_ins with
        memo = sprintf "add %s,%s,%s" (print_simd_reg "o" 0 0 r1) (print_simd_reg "i" 0 0 r2) (print_simd_reg "i" 0 1 r3);
        inputs = [r2;r3];
        outputs = [r1];
        reg_env = (add_128 [r1;r2;r3;])}

    let add_simd_s r1 r2 r3 =
      { empty_ins with
        memo = sprintf "add %s,%s,%s" (print_vecreg VSIMD64 "o" 0) (print_vecreg VSIMD64 "i" 0) (print_vecreg VSIMD64 "i" 1);
        inputs = [r2;r3;];
        outputs = [r1];
        reg_env = (add_128 [r1;r2;r3;])}

(* Scalable vector Extension *)
    let pp_pattern p = A.pp_pattern p |> Misc.lowercase

    let print_preg io offset i r = match r with
    | Preg (_,s) -> "^" ^ io ^ string_of_int (i+offset) ^
       (pp_sve_arrange_specifier s |> Misc.lowercase)
    | PMreg (_,m) -> "^" ^ io ^ string_of_int (i+offset) ^
       (pp_sve_pred_modifier m |> Misc.lowercase)
    | _ -> assert false

    let print_zreg io offset i r = match r with
    | Zreg (_,s) -> "^" ^ io ^ string_of_int (i+offset) ^
      (pp_sve_arrange_specifier s |> Misc.lowercase)
    | _ -> assert false

    let while_op memo r1 v r2 r3 =
      let r2,f2,r3,f3 = args2i v r2 r3 in
      let r2r3 = r2@r3 in
      { empty_ins with
        memo = sprintf "%s %s,%s,%s"
        memo
        (print_preg "o" 0 0 r1)
        f2 f3;
        inputs = r2r3;
        outputs = [r1];
        reg_env = add_svbool_t [r1;]@add_v v (r2r3); }

    let uaddv v r1 r2 r3 =
         { empty_ins with
           memo = sprintf "uaddv %s,%s,%s"
                    (print_vecreg v "o" 0)
                    (print_preg "i" 0 0 r2)
                    (print_zreg "i" 0 1 r3);
        inputs = [r2;r3;];
        outputs = [r1];
        reg_env = (add_128 [r1;])@(add_svbool_t [r2;])@(add_svint32_t [r3;])}

    let ldnsp memo v rs pg rA idx =
      let open MemExt in
      match idx with
      | Imm (0,Idx) ->
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (String.concat "," (List.mapi (print_zreg "o" 0) rs))
                 (print_preg "i" 0 0 pg);
        inputs = [pg;rA];
        outputs = rs;
        reg_env = (add_svint32_t rs)@(add_svbool_t [pg;])@[(rA,voidstar)]}
      | Imm (k,Idx) ->
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1,#%i,mul vl]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (String.concat "," (List.mapi (print_zreg "o" 0) rs))
                 (print_preg "i" 0 0 pg)
                 k;
        inputs = [pg;rA];
        outputs = rs;
        reg_env = (add_svint32_t rs)@(add_svbool_t [pg;])@[(rA,voidstar)]}
      | Reg (V64,rM,LSL,0) ->
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1,^i2]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (String.concat "," (List.mapi (print_zreg "o" 0) rs))
                 (print_preg "i" 0 0 pg);
        inputs = [pg;rA;rM];
        outputs = rs;
        reg_env = (add_svint32_t rs)@(add_svbool_t [pg;])@[(rA,voidstar)]@(add_q [rM;])}
      | Reg (V64,rM,LSL,k) ->
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1,^i2,LSL #%d]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (String.concat "," (List.mapi (print_zreg "o" 0) rs))
                 (print_preg "i" 0 0 pg)
                 k;
          inputs = [pg;rA;rM];
          outputs = rs;
          reg_env = (add_svint32_t rs)@(add_svbool_t [pg;])@[(rA,voidstar)]@(add_q [rM;])}
      | ZReg(rM,LSL,0) ->
        let r = List.hd rs in
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1,%s]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (print_zreg "o" 0 0 r)
                 (print_preg "i" 0 0 pg)
                 (print_zreg "i" 0 2 rM);
          inputs = [pg;rA;rM];
          outputs = [r];
          reg_env = (add_svint32_t [r;rM])@(add_svbool_t [pg;])@[(rA,voidstar)]}
      | ZReg(rM,se,0) ->
        let r = List.hd rs in
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1,%s,%s]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (print_zreg "o" 0 0 r)
                 (print_preg "i" 0 0 pg)
                 (print_zreg "i" 0 2 rM)
                 (pp_mem_sext se);
          inputs = [pg;rA;rM];
          outputs = [r];
          reg_env = (add_svint32_t [r;rM])@(add_svbool_t [pg;])@[(rA,voidstar)]}
      | ZReg(rM,se,k) ->
        let r = List.hd rs in
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1,%s,%s #%d]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (print_zreg "o" 0 0 r)
                 (print_preg "i" 0 0 pg)
                 (print_zreg "i" 0 2 rM)
                 (pp_mem_sext se)
                 k;
          inputs = [pg;rA;rM];
          outputs = [r];
          reg_env = (add_svint32_t [r;rM])@(add_svbool_t [pg;])@[(rA,voidstar)]}
      | _ -> assert false

    let stnsp memo v rs pg rA idx =
      let open MemExt in
      match idx with
      | Imm (0,Idx) ->
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (String.concat "," (List.mapi (print_zreg "i" 2) rs))
                 (print_preg "i" 0 0 pg);
          inputs = [pg;rA]@rs;
          outputs = [];
          reg_env = (add_svint32_t rs)@(add_svbool_t [pg;])@[(rA,voidstar)]}
      | Imm (k,Idx) ->
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1,#%i,mul vl]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (String.concat "," (List.mapi (print_zreg "i" 2) rs))
                 (print_preg "i" 0 0 pg)
                 k;
          inputs = [pg;rA]@rs;
          outputs = [];
          reg_env = (add_svint32_t rs)@(add_svbool_t [pg;])@[(rA,voidstar)]}
      | Reg (V64,rM,LSL,0) ->
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1,^i2]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (String.concat "," (List.mapi (print_zreg "i" 3) rs))
                 (print_preg "i" 0 0 pg);
          inputs = [pg;rA;rM]@rs;
          outputs = [];
          reg_env = (add_svint32_t rs)@(add_svbool_t [pg;])@[(rA,voidstar)]@(add_q [rM;])}
      | Reg (V64,rM,LSL,k) ->
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i1,^i2,LSL #%d]"
                 memo
                 (Misc.lowercase  (pp_simd_variant v))
                 (String.concat "," (List.mapi (print_zreg "i" 3) rs))
                 (print_preg "i" 0 0 pg)
                 k;
          inputs = [pg;rA;rM]@rs;
          outputs = [];
          reg_env = (add_svint32_t rs)@(add_svbool_t [pg;])@[(rA,voidstar)]@(add_q [rM;])}
      | ZReg(rM,LSL,0) ->
        let r = List.hd rs in
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i2,%s]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (print_zreg "i" 0 0 r)
                 (print_preg "i" 0 1 pg)
                 (print_zreg "i" 0 3 rM);
          inputs = [r;pg;rA;rM];
          outputs = [];
          reg_env = (add_svint32_t [r;rM])@(add_svbool_t [pg;])@[(rA,voidstar)]}
      | ZReg(rM,se,0) ->
        let r = List.hd rs in
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i2,%s,%s]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (print_zreg "i" 0 0 r)
                 (print_preg "i" 0 1 pg)
                 (print_zreg "i" 0 3 rM)
                 (pp_mem_sext se);
          inputs = [r;pg;rA;rM];
          outputs = [];
          reg_env = (add_svint32_t [r;rM])@(add_svbool_t [pg;])@[(rA,voidstar)]}
      | ZReg(rM,se,k) ->
        let r = List.hd rs in
        { empty_ins with
          memo = sprintf "%s%s {%s},%s,[^i2,%s,%s #%d]"
                 memo
                 (Misc.lowercase (pp_simd_variant v))
                 (print_zreg "i" 0 0 r)
                 (print_preg "i" 0 1 pg)
                 (print_zreg "i" 0 3 rM)
                 (pp_mem_sext se)
                 k;
          inputs = [r;pg;rA;rM];
          outputs = [];
          reg_env = (add_svint32_t [r;rM])@(add_svbool_t [pg;])@[(rA,voidstar)]}
      | _ -> assert false

    let mov_sv r k s = match s with
      | S_NOEXT ->
        { empty_ins with
          memo = sprintf "mov %s,#%i" (print_zreg "o" 0 0 r) k;
          inputs = [];
          outputs = [r;];
          reg_env = (add_svint32_t [r])}
      | S_LSL(ks) ->
        { empty_ins with
          memo = sprintf "mov %s,#%i,%s" (print_zreg "o" 0 0 r) k (pp_shifter (S_LSL ks));
          inputs = [];
          outputs = [r;];
         reg_env = (add_svint32_t [r])}
        | _ -> assert false

    let dup_sv_v r1 v r2 =
      { empty_ins with
        memo = sprintf "dup %s,%s"
          (print_zreg "o" 0 0 r1)
          (match v with | V32 -> "^wi0" | V64 -> "^i0" | V128 -> assert false);
        inputs = [r2];
        outputs = [r1];
        reg_env = (add_svint32_t [r1;]) @ ((
          match v with
          | V32 -> add_w
          | V64 -> add_q
          | V128 -> assert false) [r2;])}

    let add_sv r1 r2 r3 =
      { empty_ins with
        memo = sprintf "add %s,%s,%s" (print_zreg "o" 0 0 r1) (print_zreg "i" 0 0 r2) (print_zreg "i" 0 1 r3);
        inputs = [r2;r3];
        outputs = [r1];
        reg_env = (add_svint32_t [r1;r2;r3;])}

    let index_si r1 v r2 k =
      { empty_ins with
        memo = sprintf "index %s,%s,#%i"
          (print_zreg "o" 0 0 r1)
          (match v with | V32 -> "^wi0" | V64 -> "^i0" | V128 -> assert false)
          k;
        inputs = [r2];
        outputs = [r1];
        reg_env = (add_svint32_t [r1;]) @ ((
          match v with
          | V32 -> add_w
          | V64 -> add_q
          | V128 -> assert false) [r2;])}

    let index_is r1 v k r2 =
      { empty_ins with
        memo = sprintf "index %s,#%i,%s"
          (print_zreg "o" 0 0 r1)
          k
          (match v with | V32 -> "^wi0" | V64 -> "^i0" | V128 -> assert false);
        inputs = [r2];
        outputs = [r1];
        reg_env = (add_svint32_t [r1;]) @ ((
          match v with
          | V32 -> add_w
          | V64 -> add_q
          | V128 -> assert false) [r2;])}

    let index_ss r1 v r2 r3 =
      { empty_ins with
        memo = sprintf "index %s,%s"
          (print_zreg "o" 0 0 r1)
          (match v with | V32 -> "^wi0,^wi1" | V64 -> "^i0,^i1" | V128 -> assert false);
        inputs = [r2;r3];
        outputs = [r1];
        reg_env = (add_svint32_t [r1;]) @ ((
          match v with
          | V32 -> add_w
          | V64 -> add_q
          | V128 -> assert false) [r2;r3])}


    let index_ii r1 k1 k2 =
      { empty_ins with
        memo = sprintf "index %s,#%i,#%i"
          (print_zreg "o" 0 0 r1) k1 k2;
        inputs = [];
        outputs = [r1];
        reg_env = (add_svint32_t [r1;])}

    let ptrue pred pattern =
      { empty_ins with
        memo = sprintf "ptrue %s,%s" (print_preg "o" 0 0 pred) (pp_pattern pattern);
        inputs = [];
        outputs = [pred];
        reg_env = (add_svbool_t [pred;])}

    let neg_sv r1 pg r3 =
      { empty_ins with
        memo = sprintf "neg %s,%s,%s" (print_zreg "o" 0 0 r1) (print_preg "i" 0 1 pg) (print_zreg "i" 0 2 r3);
        inputs = [r1;pg;r3]; (* r1 is intentionally in 'inputs', see comment for reg_class_stable *)
        outputs = [r1];
        reg_env = (add_svint32_t [r1;r3;])@(add_svbool_t [pg;])}

    let movprfx r1 pg r3 =
      { empty_ins with
        memo = sprintf "movprfx %s,%s,%s" (print_zreg "o" 0 0 r1) (print_preg "i" 0 1 pg) (print_zreg "i" 0 2 r3);
        inputs = [r1;pg;r3]; (* r1 is intentionally in 'inputs', see comment for reg_class_stable *)
        outputs = [r1];
        reg_env = (add_svint32_t [r1;r3;])@(add_svbool_t [pg;])}

    let eor_sv r1 r2 r3 =
      { empty_ins with
        memo = sprintf "eor %s,%s,%s" (print_zreg "o" 0 0 r1) (print_zreg "i" 0 0 r2) (print_zreg "i" 0 1 r3);
        inputs = [r2;r3];
        outputs = [r1];
        reg_env = (add_svint32_t [r1;r2;r3;])}

    let rdvl rd k =
      { empty_ins with
        memo = sprintf "rdvl ^o0,#%i" k;
        outputs = [rd;];
        reg_env = add_q [rd;]; }

    let addvl rd rn k =
      { empty_ins with
        memo = sprintf "addvl ^o0,^i0,#%i" k;
        outputs = [rd;]; inputs=[rn;];
        reg_env = add_q [rd;rn;]; }

    let pp_cnt_inc_op op = pp_cnt_inc_op op |> Misc.lowercase
    and pp_pattern_scaled pat k =
      dump_pattern_scaled pat k |> Misc.lowercase

    let cnt_inc_sve (cnt_inc,_ as op) rd pat k =
      let rd,fd = arg1o V64 rd in
      { empty_ins with
        memo =
          sprintf "%s %s%s"
            (pp_cnt_inc_op op) fd
            (pp_pattern_scaled pat k);
        outputs = rd;
        inputs =
          (match cnt_inc with
           | CNT -> []
           | INC -> rd);
        reg_env = add_q rd; }

    let adda s rD p1 p2 rS =
      { empty_ins with
        memo = sprintf "add%sa %s,%s,%s,%s"
        (Misc.lowercase (pp_za_dirrection_specifier s))
        (Misc.lowercase (pp_zareg rD))
        (print_preg "i" 0 0 p1)
        (print_preg "i" 1 0 p2)
        (print_zreg "i" 2 0 rS);
        outputs = [];
        inputs = [p1;p2;rS];
        reg_env = add_svbool_t [p1;p2]@add_svint32_t[rS;];
        clobbers = [rD]
      }

    let mova_vt rD rI k p rS =
      { empty_ins with
        memo = sprintf "mova %s[^wi0,%d],%s,%s"
        (Misc.lowercase (pp_zareg rD))
        k
        (print_preg "i" 1 0 p)
        (print_zreg "i" 2 0 rS);
        outputs = [];
        inputs = [rI;p;rS];
        reg_env = add_w [rI;]@add_svbool_t [p;]@add_svint32_t[rS;];
        clobbers = [rD]
      }

    let mova_tv rD p rS rI k  =
      { empty_ins with
        memo = sprintf "mova %s,%s,%s[^wi1,%d]"
        (print_zreg "o" 0 0 rD)
        (print_preg "i" 0 0 p)
        (Misc.lowercase (pp_zareg rS))
        k;
        outputs = [rD];
        inputs = [p;rI];
        reg_env = add_w [rI;]@add_svbool_t [p;]@add_svint32_t[rD;];
      }

    let ld1spt v rD rI k p rA idx =
      let open MemExt in
      match idx with
      | Imm (0,Idx) ->
        { empty_ins with
          memo = sprintf "ld1%s {%s[^wi0,%d]},%s,[^i2]"
          (Misc.lowercase (pp_simd_variant v))
          (Misc.lowercase (pp_zareg rD))
          k
          (print_preg "i" 1 0 p);
          outputs = [];
          inputs = [rI;p;rA];
          reg_env = add_w [rI;]@add_svbool_t [p;]@[(rA,voidstar)];
          clobbers = [rD]
        }
      | Reg (V64,rM,LSL,k1) ->
        { empty_ins with
          memo = sprintf "ld1%s {%s[^wi0,%d]},%s,[^i2,^i3,LSL #%d]"
          (Misc.lowercase (pp_simd_variant v))
          (Misc.lowercase (pp_zareg rD))
          k
          (print_preg "i" 1 0 p)
          k1;
          outputs = [];
          inputs = [rI;p;rA;rM];
          reg_env = add_w [rI;]@add_svbool_t [p;]@[(rA,voidstar)]@add_q [rM;];
          clobbers = [rD]
        }
      | _ -> assert false

    let st1spt v rD rI k p rA idx =
      let open MemExt in
      match idx with
      | Imm (0,Idx) ->
        { empty_ins with
          memo = sprintf "st1%s {%s[^wi0,%d]},%s,[^i2]"
          (Misc.lowercase (pp_simd_variant v))
          (Misc.lowercase (pp_zareg rD))
          k
          (print_preg "i" 1 0 p);
          outputs = [];
          inputs = [rI;p;rA];
          reg_env = add_w [rI;]@add_svbool_t [p;]@[(rA,voidstar)];
        }
      | Reg (V64,rM,LSL,k1) ->
        { empty_ins with
          memo = sprintf "st1%s {%s[^wi0,%d]},%s,[^i2,^i3,LSL #%d]"
          (Misc.lowercase (pp_simd_variant v))
          (Misc.lowercase (pp_zareg rD))
          k
          (print_preg "i" 1 0 p)
          k1;
          outputs = [];
          inputs = [rI;p;rA;rM];
          reg_env = add_w [rI;]@add_svbool_t [p;]@[(rA,voidstar)]@add_q [rM;];
        }
      | _ -> assert false

    let pp_sm_op = function 
    | None -> ""
    | Some r -> " " ^ (pp_sm r |> Misc.lowercase)

    let sm_op memo op =
      { empty_ins with memo = sprintf "%s%s" memo (pp_sm_op op); }

(* Compare and swap *)

    let cas_memo rmw = Misc.lowercase (cas_memo rmw)
    let casp_memo rmw = Misc.lowercase (casp_memo rmw)
    let casbh_memo bh rmw = Misc.lowercase (casbh_memo bh rmw)

    let cas memo v r1 r2 r3 =
      let r1,f1,r2,f2 = args2i v r1 r2 in
      let idx = match r1,r2 with
      | [],[] -> 0
      | (_::_,[])|([],_::_) -> 1
      | _::_,_::_ -> 2 in
      { empty_ins with
        memo = sprintf "%s %s,%s,[^i%i]" memo f1 f2 idx;
        inputs = r1@r2@[r3]; outputs = r1;
        reg_env = (r3,voidstar)::add_v v (r1@r2); }

    let casp memo v r1 r2 r3 r4 r5 =
      let rs1,rs2,rs3,rs4,rs5 = match v with
      (* How to output even consecutive registers? *)
      (* Assembler needs this proprty*)
      | V32 -> "^wi0","^wi1","^wi2","^wi3", "^i4"
      | V64 -> "^i0", "^i1", "^i2", "^i3", "^i4"
      | V128 -> assert false in
      { empty_ins with
        memo = sprintf "%s %s,%s,%s,%s,[%s]" memo rs1 rs2 rs3 rs4 rs5;
        inputs = [r1;r2;r3;r4;r5]; outputs = [r1;r2;r3;r4;r5];
        reg_env = (r5,voidstar)::add_v v [r1;r2;r3;r4]@add_type quad [r5] }

(* Swap *)
    let swp_memo rmw = Misc.lowercase (swp_memo rmw)
    let swpbh_memo bh rmw = Misc.lowercase (swpbh_memo bh rmw)

    let swp memo v r1 r2 r3 =
      let r1,f1 = do_arg1i v r1 0 in
      let idx = match r1 with | [] -> 0 | _::_ -> 1 in
      let r2,f2 = do_arg1o v r2 0 in
      { empty_ins with
        memo = sprintf "%s %s,%s,[^i%i]" memo f1 f2 idx;
        inputs = r1@[r3;]; outputs =r2;
        reg_env = (r3,voidstar)::add_v v (r1@r2); }

(* Fetch and Op *)
    let ldop_memo op rmw = Misc.lowercase (ldop_memo op rmw)
    let stop_memo op w = Misc.lowercase (stop_memo op w)
    let ldopbh_memo op bh rmw =  Misc.lowercase (ldopbh_memo op bh rmw)
    let stopbh_memo op bh w =  Misc.lowercase (stopbh_memo op bh w)

    let ldop memo v rs rt rn =
      let rs,fs = do_arg1i v rs 0 in
      let idx = match rs with | [] -> 0 | _::_ -> 1 in
      let rt,ft =  do_arg1o v rt 0 in
      { empty_ins with
        memo = sprintf "%s %s,%s,[^i%i]" memo fs ft idx;
        inputs = rs@[rn]; outputs = rt;
        reg_env = (rn,voidstar)::add_v v (rs@rt);}

    let stop memo v rs rn =
      let t = match v with | V32 -> word | V64 -> quad | V128 -> assert false in
      let rs,fs = do_arg1i v rs 0 in
      let idx = match rs with | [] -> 0 | _::_ -> 1 in
      { empty_ins with
        memo = sprintf "%s %s,[^i%i]" memo fs idx;
        inputs = rs@[rn]; outputs = [];
        reg_env = (rn,voidstar)::add_type t rs;}

(* Arithmetic *)
    let mov_const v r k =
      let memo =
        sprintf
          (match v with
            | V32 ->  "mov ^wo0,#%i"
            | V64 ->  "mov ^o0,#%i"
            | V128 -> assert false)
          k in
      { empty_ins with memo; outputs=[r;];
        reg_env = ((match v with
          | V32 -> add_w
          | V64 -> add_q
          | V128 -> assert false)
        [r;])}

    let adr tr_lab r lbl =
      let _,lbl = dump_tgt tr_lab lbl in
      let r,f = do_arg1o V64 r 0 in
      { empty_ins with
        memo = sprintf "adr %s,%s" f lbl;
        outputs=r; reg_env=add_p r; }

    let do_movr memo v r1 r2 =
      let r1,f1 = do_arg1o v r1 0 and r2,f2 = do_arg1i v r2 0  in
      { empty_ins with
        memo=sprintf "%s %s,%s" memo f1 f2;
        inputs = r2; outputs=r1; reg_env=add_v v (r1@r2);}

    (* First 'fi' argument computes inputs from outputs *)
    let do_movz fi memo v rd k os =
        let r1,f1 = do_arg1o v rd 0 in
        match k, os with
        | k, S_LSL(s) ->
           { empty_ins with
             memo=sprintf "%s %s, #%d, %s" memo f1 k (pp_shifter (S_LSL s));
             inputs=fi r1; outputs=r1; reg_env=add_v v r1;}
        | k, S_NOEXT ->
           { empty_ins with
             memo=sprintf "%s %s, #%d" memo f1 k;
             inputs=fi r1; outputs=r1; reg_env=add_v v r1;}
        | _ -> Warn.fatal "Illegal form of %s instruction" memo

    let movr = do_movr "mov"
    and rbit = do_movr "rbit"
    and abs = do_movr "abs"
    and movz = do_movz (fun _ -> []) (* No input *) "movz"
    and movn = do_movz (fun _ -> []) (* No input *) "movn"
    and movk = do_movz Misc.identity (* Part of register preserved *) "movk"
    and rev rv =
      let memo = Misc.lowercase (memo_of_rev rv)
      and v = variant_of_rev rv in
      do_movr memo v

    let extr v rd rn rm k =
      let rnm,fn,fm =  do_arg2i v rn rm 0
      and rd,fd = do_arg1o v rd 0 in
      { empty_ins with
        memo = sprintf "extr %s,%s,%s,#%d" fd fn fm k;
        inputs=rnm; outputs=rd;
        reg_env =  add_v v (rd@rnm); }

    let sxtw r1 r2 =
      { empty_ins with
        memo = "sxtw ^o0,^wi0";
        inputs = [r2;]; outputs=[r1;]; reg_env=[r1,quad; r2,word];}

    let xbfm s v r1 r2 k1 k2 =
      let rs,fm1,fm2 = do_arg2i v r1 r2 0 in
      { empty_ins with
        memo = sprintf "%s %s,%s,#%i,#%i" s fm1 fm2 k1 k2;
        inputs = rs; reg_env = add_v v rs;}

    let pp_op_ext_shift s = OpExt.pp_shift m_hash s |> Misc.lowercase

    let v2fmt v = match v with | V32 -> "w"| V64 -> "" | V128 -> assert false

    let do_cmp memo v r a =
      let open OpExt in
      match a with
      | Imm (k,s) ->
         let r,f = do_arg1i v r 0 in
         let pp_s = match s with 0 -> "" | _ -> sprintf ",#%i" s in
         { empty_ins with
           memo = sprintf "%s %s,#%i%s" memo f  k pp_s;
           inputs = r; reg_env=add_v v r; }
      | Reg (idx,s) ->
         let rs,f1,f2 = do_arg2i v r idx 0 in
         { empty_ins with
           memo = sprintf "%s %s,%s%s" memo f1 f2 (pp_op_ext_shift s);
           inputs = rs; reg_env=add_v v rs; }

    let cmp = do_cmp "cmp"
    let tst = do_cmp "tst"

    let cmpk v r k = cmp v r (OpExt.Imm (k,0))

    let pp_ext e = Ext.pp_ext m_hash e |> Misc.lowercase

    let do_cmp_ext memo v r2 (v3,r3) e =
      let fmt = v2fmt v in
      let r3,fmt3 = do_arg1i v3 r3 1 in
      { empty_ins with
        memo = sprintf "%s ^%si0,%s%s" memo fmt fmt3 (pp_ext e);
        inputs = r2::r3; outputs = [];
        reg_env = add_v v [r2;]@add_v v3 r3; }

    let cmp_ext = do_cmp_ext "cmp"

    let memo_of_op op = Misc.lowercase (pp_op op)

    let op2 memo v r1 r2 s =
      let r1,f1 = do_arg1o v r1 0 and r2,f2 = do_arg1i v r2 0 in
      { empty_ins with
        memo=sprintf "%s %s,%s%s" memo f1 f2 (pp_op_ext_shift s);
        inputs=r2;
        outputs=r1; reg_env = add_v v (r1@r2);}

    let mvn = op2 "movn"
    and neg = op2  "neg"
    and negs = op2 "negs"

    let op3 v op rD rN e =
      let memo = memo_of_op op
      and rD,fD = do_arg1o v rD 0
      and rN,fN = do_arg1i v rN 0 in
      let open OpExt in
      match e with
      | Imm (k,s) ->
         let pp_s = match s with 0 -> "" | _ -> sprintf ", lsl #%i" s in
         { empty_ins with
           memo=sprintf "%s %s,%s,#%i%s" memo fD fN k pp_s;
           inputs=rN;
           outputs=rD; reg_env = add_v v(rN@rD);}
      | Reg (idx,s) ->
         let rI,fI = do_arg1i v idx (match rN with [] -> 0 | _::_ -> 1) in
         { empty_ins with
           memo = sprintf "%s %s,%s,%s%s" memo fD fN fI (pp_op_ext_shift s);
           inputs = rN@rI; outputs=rD;
           reg_env = add_v v (rD@rN@rI); }

    let moplz sop rd rn rm =
      let rd,fd = do_arg1o V64 rd 0
      and rs,fn,fm = do_arg2i V32 rn rm  0
      and memo = MOPLExt.memo_z sop |> Misc.lowercase in
      { empty_ins with
        memo = sprintf "%s %s,%s,%s" memo fd fn fm;
        inputs=rs; outputs=rd;
        reg_env = add_q rd@ add_w rs; }

    let mopl sop rd rn rm ra =
      let rd,fd = do_arg1o V64 rd 0
      and rs,fn,fm = do_arg2i V32 rn rm  0
      and memo = MOPLExt.memo sop |> Misc.lowercase in
      let ra,fa = do_arg1i V64 ra (List.length rs) in
      { empty_ins with
        memo = sprintf "%s %s,%s,%s,%s" memo fd fn fm fa;
        inputs=rs@ra; outputs=rd;
        reg_env = add_q rd@ add_w rs@add_q ra; }

    let mopz op v rd rn rm =
      let rd,fd = do_arg1o v rd 0
      and rs,fn,fm = do_arg2i v rn rm 0
      and memo = MOPExt.memo_z op |> Misc.lowercase in
      { empty_ins with
        memo = sprintf "%s %s,%s,%s" memo fd fn fm;
        inputs=rs; outputs=rd;
        reg_env = add_v v (rs@rd); }

    let mop op v rd rn rm ra =
      let rd,fd = do_arg1o v rd 0
      and rs,fn,fm = do_arg2i v rn rm 0
      and memo = MOPExt.memo op |> Misc.lowercase in
      let ra,fa = do_arg1i v ra (List.length rs) in
      { empty_ins with
        memo = sprintf "%s %s,%s,%s,%s" memo fd fn fm fa;
        inputs=rs@ra; outputs=rd;
        reg_env = add_v v (rd@rs@ra); }

    let addsub_ext v op r1 r2 (v3,r3) e =
      let r1,f1 = do_arg1o v r1 0
      and r2,f2 = do_arg1i v r2 0
      and r3,f3 = do_arg1i v3 r3 (match r2 with ZR -> 0|_ -> 1) in
      { empty_ins with
        memo =
          sprintf "%s %s,%s,%s,%s"
            (Ext.pp_op op |> Misc.lowercase)
            f1 f2 f3 (pp_ext e);
        inputs = r2@r3; outputs=r1;
        reg_env = add_v v (r1@r2)@add_v v3 r3; }

    let fence f =
      { empty_ins with memo = Misc.lowercase (A.pp_barrier f); }

    let cache memo r =
      match r with
      | ZR ->
          { empty_ins with memo; }
      | _ ->
          let r,f = do_arg1i V64 r 0 in
          { empty_ins with memo = memo ^ "," ^ f; inputs=r; reg_env=add_p r; }

    let tlbi op r =
      let op = Misc.lowercase  (TLBI.pp_op op) in
      match r with
      | ZR ->
          { empty_ins with memo = sprintf "tlbi %s" op; inputs=[]; reg_env=[]; }
      | r ->
         { empty_ins with
           memo = sprintf "tlbi %s,^i0" op; inputs=[r];
           reg_env=add_p [r]; }

(* Not that useful *)
    let emit_loop _k = assert false

    let tr_ins ins =
      match ins.[String.length ins-1] with
      | ':' ->
         let lab = String.sub ins 0 (String.length ins-1) in
         { empty_ins with memo = ins; label = Some lab; }
      | _ -> { empty_ins with memo = ins; }

    let map_ins = List.map tr_ins

    type ins = A.Out.ins

    let max_handler_label = 1 (* Warning label 0 no other is used in handler code *)

    let user_mode has_handler p =
      let ins =
        (fun k ->
          "nop"
          ::"msr sp_el0,%[sp_usr]"
          ::"adr %[tr0],0f"
          ::"msr elr_el1,%[tr0]"
          ::"msr spsr_el1,xzr"
          ::"eret"
          ::"0:"
          ::k)
          (if has_handler then [sprintf "adr x29,asm_handler%d" p;] else []) in
      map_ins ins

    let kernel_mode has_handler =
      map_ins
        ((fun k -> if has_handler then "adr x29,0f"::k else k)
           ["svc #471";])

    let fault_handler_prologue _is_user p =
      map_ins ["b 0f"; sprintf  "asm_handler%d:" p;]

    and fault_handler_epilogue is_user code =
      let ins =
        if
          List.exists
            (fun i -> i.memo = "eret")
            code
        then [] (* handler is complete *)
        else if Fault.Handling.is_skip C.precision then
          [ "mrs %[tr0],elr_el1" ;
            "add %[tr0],%[tr0],#4" ;
            "msr elr_el1,%[tr0]" ;
            "eret" ]
        else if Fault.Handling.is_fatal C.precision then
          (if is_user then []
           else
             [ "adr %[tr0],0f";
               "msr elr_el1,%[tr0]";
               "eret" ])
        else
          [ "eret" ] in
      let ins = map_ins ins in
      ins@[ {empty_ins with memo="0:"; label=Some "0"; } ]

      let compile_ins tr_lab ins k = match ins with
    | I_NOP -> { empty_ins with memo = "nop"; }::k
(* Branches *)
    | I_B lbl -> b tr_lab lbl::k
    | I_BR r -> br r::k
    | I_BL lbl -> bl tr_lab lbl::k
    | I_BLR r -> blr r::k
    | I_RET None -> { empty_ins with memo="ret"; }::k
    | I_RET (Some r) -> ret r::k
    | I_ERET -> { empty_ins with memo="eret"; }::k
    | I_BC (c,lbl) -> bcc tr_lab c lbl::k
    | I_CBZ (v,r,lbl) -> cbz tr_lab "cbz" v r lbl::k
    | I_CBNZ (v,r,lbl) -> cbz tr_lab "cbnz" v r lbl::k
    | I_TBNZ (v,r,k2,lbl) -> tbz tr_lab "tbnz" v r k2 lbl::k
    | I_TBZ (v,r,k2,lbl) -> tbz tr_lab "tbz" v r k2 lbl::k
(* Load and Store *)
    | I_LDR (v,r1,r2,idx) -> load "ldr" v r1 r2 idx::k
    | I_LDRSW (r1,r2,idx) -> load "ldrsw" V64 r1 r2 idx::k
    | I_LDUR (v,r1,r2,k') -> load "ldur" v r1 r2 (MemExt.k2idx k')::k
    | I_LDP (t,v,r1,r2,r3,idx) ->
        load_pair (ldp_memo t) v r1 r2 r3 idx::k
    | I_LDPSW (r1,r2,r3,idx) ->
       ldpsw r1 r2 r3 idx::k
    | I_LDXP (v,t,r1,r2,r3) ->
       loadx_pair (ldxp_memo t) v r1 r2 r3::k
    | I_STP (t,v,r1,r2,r3,idx) ->
        store_pair (stp_memo t) v r1 r2 r3 idx::k
    | I_STXP (v,t,r1,r2,r3,r4) ->
        storex_pair (stxp_memo t) v r1 r2 r3 r4::k
    | I_LDRBH (B,r1,r2,idx) -> load "ldrb" V32 r1 r2 idx::k
    | I_LDRBH (H,r1,r2,idx) -> load "ldrh" V32 r1 r2 idx::k
    | I_LDRS ((var,bh),r1,r2,idx) ->
       load (ldrs_memo bh)  var r1 r2 idx::k
    | I_LDAR (v,t,r1,r2) -> load (ldr_memo t) v r1 r2 MemExt.zero::k
    | I_LDARBH (bh,t,r1,r2) -> load (ldrbh_memo bh t) V32 r1 r2 MemExt.zero::k
    | I_STR (v,r1,r2,idx) -> store "str" v r1 r2 idx::k
    | I_STRBH (B,r1,r2,idx) -> store "strb" V32 r1 r2 idx::k
    | I_STRBH (H,r1,r2,idx) -> store "strh" V32 r1 r2 idx::k
    | I_STLR (v,r1,r2) -> store "stlr" v r1 r2 MemExt.zero::k
    | I_STLRBH (B,r1,r2) -> store "stlrb" V32 r1 r2 MemExt.zero::k
    | I_STLRBH (H,r1,r2) -> store "stlrh" V32 r1 r2 MemExt.zero::k
    | I_STXR (v,t,r1,r2,r3) -> stxr (str_memo t) v r1 r2 r3::k
    | I_STXRBH (bh,t,r1,r2,r3) -> stxr (strbh_memo bh t) V32 r1 r2 r3::k
    | I_CAS (v,rmw,r1,r2,r3) -> cas (cas_memo rmw) v r1 r2 r3::k
    | I_CASP (v,rmw,r1,r2,r3,r4,r5) -> casp (casp_memo rmw) v r1 r2 r3 r4 r5::k
    | I_CASBH (bh,rmw,r1,r2,r3) -> cas (casbh_memo bh rmw) V32 r1 r2 r3::k
    | I_SWP (v,rmw,r1,r2,r3) -> swp (swp_memo rmw) v r1 r2 r3::k
    | I_SWPBH (bh,rmw,r1,r2,r3) -> swp (swpbh_memo bh rmw) V32 r1 r2 r3::k
(* Neon Extension Load and Store *)
    | I_LDAP1 (rs,i,r2,kr) -> load_simd_s "ldap1" rs i r2 kr::k
    | I_LD1 (rs,i,r2,kr) -> load_simd_s "ld1" rs i r2 kr::k
    | I_LD1M (rs,r2,kr) -> load_simd_m "ld1" rs r2 kr::k
    | I_LD1R (rs,r2,kr) -> load_simd_m "ld1r" rs r2 kr::k
    | I_LD2 (rs,i,r2,kr) -> load_simd_s "ld2" rs i r2 kr::k
    | I_LD2M (rs,r2,kr) -> load_simd_m "ld2" rs r2 kr::k
    | I_LD2R (rs,r2,kr) -> load_simd_m "ld2r" rs r2 kr::k
    | I_LD3 (rs,i,r2,kr) -> load_simd_s "ld3" rs i r2 kr::k
    | I_LD3M (rs,r2,kr) -> load_simd_m "ld3" rs r2 kr::k
    | I_LD3R (rs,r2,kr) -> load_simd_m "ld3r" rs r2 kr::k
    | I_LD4 (rs,i,r2,kr) -> load_simd_s "ld4" rs i r2 kr::k
    | I_LD4M (rs,r2,kr) -> load_simd_m "ld4" rs r2 kr::k
    | I_LD4R (rs,r2,kr) -> load_simd_m "ld4r" rs r2 kr::k
    | I_ST1 (rs,i,r2,kr) -> store_simd_s "st1" rs i r2 kr::k
    | I_STL1 (rs,i,r2,kr) -> store_simd_s "stl1" rs i r2 kr::k
    | I_ST1M (rs,r2,kr) -> store_simd_m "st1" rs r2 kr::k
    | I_ST2 (rs,i,r2,kr) -> store_simd_s "st2" rs i r2 kr::k
    | I_ST2M (rs,r2,kr) -> store_simd_m "st2" rs r2 kr::k
    | I_ST3 (rs,i,r2,kr) -> store_simd_s "st3" rs i r2 kr::k
    | I_ST3M (rs,r2,kr) -> store_simd_m "st3" rs r2 kr::k
    | I_ST4 (rs,i,r2,kr) -> store_simd_s "st4" rs i r2 kr::k
    | I_ST4M (rs,r2,kr) -> store_simd_m "st4" rs r2 kr::k
    | I_LDP_SIMD (t,v,r1,r2,r3,idx) ->
        load_pair_simd (match t with TT -> "ldp" | NT -> "ldnp") v r1 r2 r3 idx::k
    | I_STP_SIMD (t,v,r1,r2,r3,idx) ->
        store_pair_simd (match t with TT -> "stp" | NT -> "stnp") v r1 r2 r3 idx::k
    | I_LDR_SIMD (v,r1,r2,idx) -> load_simd "ldr" v r1 r2 idx::k
    | I_STR_SIMD (v,r1,r2,idx) -> store_simd "str" v r1 r2 idx::k
    | I_LDUR_SIMD (v,r1,r2,k1) -> load_simd "ldur" v r1 r2 (MemExt.k2idx k1)::k
    | I_LDAPUR_SIMD (v,r1,r2,k1) -> load_simd "ldapur" v r1 r2 (MemExt.k2idx k1)::k
    | I_STUR_SIMD (v,r1,r2,k1) -> store_simd "stur" v r1 r2 (MemExt.k2idx k1)::k
    | I_STLUR_SIMD (v,r1,r2,k1) -> store_simd "stlur" v r1 r2 (MemExt.k2idx k1)::k
    | I_ADDV (v,r1,r2) -> addv_simd v r1 r2::k
    | I_DUP (r1,v,r2) -> dup_simd_v r1 v r2::k
    | I_FMOV_TG (v1,r1,v2,r2) -> fmov_simd_tg v1 r1 v2 r2::k
    | I_MOV_VE (r1,i1,r2,i2) -> mov_simd_ve r1 i1 r2 i2::k
    | I_MOV_V (r1,r2) -> mov_simd_v r1 r2::k
    | I_MOV_FG (r1,i,v,r2) -> mov_simd_fg r1 i v r2::k
    | I_MOV_TG (v,r1,r2,i) -> mov_simd_tg v r1 r2 i::k
    | I_MOVI_S (v,r,k1) -> movi_s v r k1::k
    | I_MOVI_V (r,kr,s) -> movi_v r kr s::k
    | I_MOV_S (v,r1,r2,i) -> mov_simd_s v r1 r2 i::k
    | I_OP3_SIMD (EOR,r1,r2,r3) -> eor_simd r1 r2 r3::k
    | I_OP3_SIMD _ -> assert false
    | I_ADD_SIMD (r1,r2,r3) -> add_simd r1 r2 r3::k
    | I_ADD_SIMD_S (r1,r2,r3) -> add_simd_s r1 r2 r3::k
(* Scalable Vector Extension *)
    | I_WHILELT (z,v,r1,r2) -> while_op "whilelt" z v r1 r2::k
    | I_WHILELE (z,v,r1,r2) -> while_op "whilele" z v r1 r2::k
    | I_WHILELO (z,v,r1,r2) -> while_op "whilelo" z v r1 r2::k
    | I_WHILELS (z,v,r1,r2) -> while_op "whilels" z v r1 r2::k
    | I_UADDV (v,r1,r2,r3) -> uaddv v r1 r2 r3::k
    | I_LD1SP (v,rs,r2,r3,idx) -> ldnsp "ld1" v rs r2 r3 idx::k
    | I_LD2SP (v,rs,r2,r3,idx) -> ldnsp "ld2" v rs r2 r3 idx::k
    | I_LD3SP (v,rs,r2,r3,idx) -> ldnsp "ld3" v rs r2 r3 idx::k
    | I_LD4SP (v,rs,r2,r3,idx) -> ldnsp "ld4" v rs r2 r3 idx::k
    | I_ST1SP (v,rs,r2,r3,idx) -> stnsp "st1" v rs r2 r3 idx::k
    | I_ST2SP (v,rs,r2,r3,idx) -> stnsp "st2" v rs r2 r3 idx::k
    | I_ST3SP (v,rs,r2,r3,idx) -> stnsp "st3" v rs r2 r3 idx::k
    | I_ST4SP (v,rs,r2,r3,idx) -> stnsp "st4" v rs r2 r3 idx::k
    | I_MOV_SV (r,kr,s) -> mov_sv r kr s::k
    | I_DUP_SV (r1,v,r2) -> dup_sv_v r1 v r2::k
    | I_ADD_SV (r1,r2,r3) -> add_sv r1 r2 r3::k
    | I_INDEX_SI (r1,v,r2,kr) -> index_si r1 v r2 kr::k
    | I_INDEX_IS (r1,v,kr,r2) -> index_is r1 v kr r2::k
    | I_INDEX_SS (r1,v,r2,r3) -> index_ss r1 v r2 r3::k
    | I_INDEX_II (r1,k1,k2) -> index_ii r1 k1 k2::k
    | I_PTRUE (pred,pat) -> ptrue pred pat::k
    | I_NEG_SV (r1,r2,r3) -> neg_sv r1 r2 r3::k
    | I_MOVPRFX (r1,r2,r3) -> movprfx r1 r2 r3::k
    | I_OP3_SV (EOR,r1,r2,r3) -> eor_sv r1 r2 r3::k
    | I_OP3_SV _ -> assert false
    | I_RDVL (r1,k1) -> rdvl r1 k1::k
    | I_ADDVL (r1,r2,k1) -> addvl r1 r2 k1::k
    | I_CNT_INC_SVE  (op,rd,pat,k1) -> cnt_inc_sve op rd pat k1::k
(* Scalable Matrix Extension *)
    | I_LD1SPT (v,r,ri,k1,p,ra,idx) -> ld1spt v r ri k1 p ra idx::k
    | I_ST1SPT (v,r,ri,k1,p,ra,idx) -> st1spt v r ri k1 p ra idx::k
    | I_MOVA_VT (r,ri,k1,p,rs) -> mova_vt r ri k1 p rs::k
    | I_MOVA_TV (r,p,rs,ri,k1) -> mova_tv r p rs ri k1::k
    | I_ADDA (s,r,p1,p2,rs) -> adda s r p1 p2 rs::k
    | I_SMSTART op -> sm_op "smstart" op::k
    | I_SMSTOP op -> sm_op "smstop" op::k
(* Arithmetic *)
    | I_MOV (v,r,K i) ->  mov_const v r i::k
    | I_MOV (v,r1,RV (_,r2)) ->  movr v r1 r2::k
    | I_MOVZ (v,rd,i,os) -> movz v rd i os::k
    | I_MOVN (v,rd,i,os) -> movn v rd i os::k
    | I_MOVK (v,rd,i,os) -> movk  v rd i os::k
    | I_ADR (r,lbl) -> adr tr_lab r lbl::k
    | I_RBIT (v,rd,rs) -> rbit v rd rs::k
    | I_ABS (v,rd,rs) -> abs v rd rs::k
    | I_REV (v,rd,rs) -> rev v rd rs::k
    | I_EXTR (v,rd,rn,rm,lsb) -> extr v rd rn rm lsb::k
    | I_SXTW (r1,r2) -> sxtw r1 r2::k
    | I_SBFM (v,r1,r2,k1,k2) -> xbfm "sbfm" v r1 r2 k1 k2::k
    | I_UBFM (v,r1,r2,k1,k2) -> xbfm "ubfm" v r1 r2 k1 k2::k
    | I_OP3 (v,SUBS,ZR,r,e) -> cmp v r e::k
    | I_OP3 (v,SUB,r1,ZR,OpExt.Reg (r2,s)) -> neg v r1 r2 s::k
    | I_OP3 (v,SUBS,r1,ZR,OpExt.Reg (r2,s)) -> negs v r1 r2 s::k
    | I_ADDSUBEXT (v,Ext.SUBS,ZR,r2,vr3,e) -> cmp_ext v r2  vr3 e::k
    | I_OP3 (v,ANDS,ZR,r,e) -> tst v r e::k
    | I_OP3 (v,ORN,r1,ZR,OpExt.Reg (r2,s)) -> mvn v r1 r2 s::k
    | I_OP3 (v,op,r1,r2,e) ->  op3 v op r1 r2 e::k
    | I_MOPL (sop,rd,rn,rm,ZR) -> moplz sop rd rn rm::k
    | I_MOPL (sop,rd,rn,rm,ra) -> mopl sop rd rn rm ra::k
    | I_MOP (op,v,rd,rn,rm,ZR) -> mopz op v rd rn rm::k
    | I_MOP (op,v,rd,rn,rm,ra) -> mop op v rd rn rm ra::k
    | I_ADDSUBEXT (v,op,r1,r2,vr3,e) -> addsub_ext v op r1 r2 vr3 e::k
(* Fence *)
    | I_FENCE f -> fence f::k
(* Fetch and Op *)
    |I_LDOP (op,v,(RMW_P|RMW_L as rmw),rs,ZR,rn) ->
        stop (stop_memo op (rmw_to_w rmw)) v rs rn::k
    |I_LDOP (op,v,rmw,rs,rt,rn) ->
        ldop (ldop_memo op rmw) v rs rt rn::k
    |I_LDOPBH  (op,v,(RMW_P|RMW_L as rmw),rs,ZR,rn) ->
        stop (stopbh_memo op v (rmw_to_w rmw)) V32 rs rn::k
    |I_LDOPBH  (op,v,rmw,rs,rt,rn) ->
        ldop (ldopbh_memo op v rmw) V32 rs rt rn::k
    | I_STOP (op,v,w,rs,rn) ->
        stop (stop_memo op w) v rs rn::k
    | I_STOPBH (op,v,w,rs,rn) ->
        stop (stopbh_memo op v w) V32 rs rn::k
(* Conditional selection *)
    | I_CSEL (v,r,ZR,ZR,c,(Inc|Inv as op)) ->
        let o,f = arg1o v r
        and t =
          match v with V32 -> word | V64 -> quad | V128 -> assert false in
        let memo =
          let op =
            match op with
            | Inc -> "cset"
            | Inv -> "csetm"
            | _ -> assert false in
          sprintf "%s %s,%s" op f (pp_cond (inverse_cond c)) in
        { empty_ins with memo; outputs=o; reg_env=add_type t o; }::k
    | I_CSEL (v,r1,r2,r3,c,Inc) when r2=r3 ->
       let o,fo = arg1o v r1
       and i,fi = arg1i v r2
       and t = v2type v in
       let memo =
         sprintf "cinc %s,%s,%s" fo fi  (pp_cond (inverse_cond c)) in
           { empty_ins
           with memo; inputs=i;
           outputs=o; reg_env=add_type t (i@o); }::k
    | I_CSEL (v,r1,r2,r3,c,op) ->
       let ri,f2,f3 = do_arg2i v r2 r3 0
       and ro,f1 = do_arg1o v r1 0 in
       {
         empty_ins with
         memo =
           sprintf "%s %s,%s,%s,%s"
             (sel_memo op |> Misc.lowercase) f1 f2 f3 (pp_cond c);
         inputs=ri; outputs=ro;
         reg_env=add_v v (ro@ri); }::k
    | I_IC (op,r) ->
        cache (sprintf "ic %s" (Misc.lowercase (IC.pp_op op))) r::k
    | I_DC (op,r) ->
        cache (sprintf "dc %s" (Misc.lowercase (DC.pp_op op))) r::k
    | I_TLBI (op,r) ->
        tlbi op r::k
    | I_MRS (r,sr) ->
        let r,f = do_arg1o V64 r 0 in
        let memo =
          sprintf "mrs %s,%s" f (Misc.lowercase (pp_sysreg sr)) in
        {empty_ins with
         memo; outputs=r; reg_env=add_type quad r;}::k
    | I_MSR (sr,r) ->
       let r,f = do_arg1o V64 r 0 in
       let memo =
         sprintf "msr %s,%s" (Misc.lowercase (pp_sysreg sr)) f in
       {empty_ins with
         memo; outputs=r; reg_env=add_type quad r;}::k
    | I_STG _| I_STZG _|I_STZ2G _|I_LDG _ ->
        Warn.fatal "No litmus output for instruction %s"
          (dump_instruction ins)
    | I_ALIGND _|I_ALIGNU _|I_BUILD _|I_CHKEQ _|I_CHKSLD _|I_CHKTGD _|
      I_CLRTAG _|I_CPYTYPE _|I_CPYVALUE _|I_CSEAL _|I_GC _|I_LDCT _|I_SC _|
      I_SEAL _|I_STCT _|I_UNSEAL _ ->
        Warn.fatal "No litmus output for instruction %s"
            (dump_instruction ins)
    | I_SVC kk ->
      let memo = sprintf "svc #%i" kk  in
        { empty_ins with memo; }::k
    | I_UDF _ ->
        { empty_ins with memo = ".word 0"; }::k

    let no_tr lbl = lbl
    let branch_neq r i lab k = cmpk V32 r i::bcc no_tr NE lab::k
    let branch_eq r i lab k = cmpk V32 r i::bcc no_tr EQ lab::k

    let signaling_write _i _k = Warn.fatal "no signaling write for ARM"

    let emit_tb_wait _ = Warn.fatal "no time base for ARM"
  end
