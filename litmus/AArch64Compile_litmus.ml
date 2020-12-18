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

(* Return instruction *)
    let is_ret = function
      | A.I_RET None -> true
      | _ -> false

(* No addresses in code *)
    let extract_addrs _ins = StringSet.empty

    let stable_regs _ins = match _ins with
    | I_LD1M (rs,_,_)
    | I_LD2 (rs,_,_,_) | I_LD2M (rs,_,_) | I_LD2R (rs,_,_)
    | I_LD3 (rs,_,_,_) | I_LD3M (rs,_,_) | I_LD3R (rs,_,_)
    | I_LD4 (rs,_,_,_) | I_LD4M (rs,_,_) | I_LD4R (rs,_,_)
    | I_ST1M (rs,_,_)
    | I_ST2 (rs,_,_,_) | I_ST2M (rs,_,_)
    | I_ST3 (rs,_,_,_) | I_ST3M (rs,_,_)
    | I_ST4 (rs,_,_,_) | I_ST4M (rs,_,_) ->
        A.RegSet.of_list rs
    | I_LD1 (r,_,_,_) -> A.RegSet.of_list [r]
    | _ ->  A.RegSet.empty

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
    let add_v = add_type voidstar
    let add_128 = add_type int128

(* pretty prints barrel shifters *)
    let pp_shifter = function
      | S_LSL(s) -> Printf.sprintf "LSL #%d" s
      | S_LSR(s) -> Printf.sprintf "LSR #%d" s
      | S_MSL(s) -> Printf.sprintf "MSL #%d" s
      | S_ASR(s) -> Printf.sprintf "ASR #%d" s
      | S_SXTW -> "SXTW"
      | S_UXTW -> "UXTW"
      | S_NOEXT  -> ""

(* handle the temporary hack to accept `ins ..,[X1,W2]`
   as a shorthand for `ins ..,[X1,W2,SXTW]`.
   Applies to instructions LDRB and LDRH *)

    let default_shift = function
      | RV (V32,_) -> S_SXTW
      | _ -> S_NOEXT
            
(************************)
(* Template compilation *)
(************************)


(* Branches *)
    let pp_cond = function
      | EQ -> "eq"
      | NE -> "ne"
      | GE -> "ge"
      | GT -> "gt"
      | LE -> "le"
      | LT -> "lt"

    let b tr_lab lbl =
      { empty_ins with
        memo = sprintf "b %s" (A.Out.dump_label (tr_lab lbl)) ;
        branch=[Branch lbl] ; }

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
      { empty_ins with
        memo = sprintf "bl %s" (A.Out.dump_label (tr_lab lbl)) ;
        inputs=[]; outputs=[];
        branch=[Next;Branch lbl] ; clobbers=[linkreg;]; }

    let blr r =
      { empty_ins with
        memo = "blr ^i0";
        inputs=[r;]; outputs=[];
        reg_env = [r,voidstar;];
        branch=[Any] ;  clobbers=[linkreg;]; }

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
          | V64 -> "%s ^i0,%s"
          | V128 -> assert false)
          memo (A.Out.dump_label (tr_lab lbl)) in
      { empty_ins with
        memo; inputs=[r;]; outputs=[];
        branch=[Next; Branch lbl] ; }

    let tbz tr_lab memo v r k lbl =
      let memo =
        sprintf
          (match v with
          | V32 -> "%s ^wi0,#%d, %s"
          | V64 -> "%s ^i0, #%d, %s"
          | V128 -> assert false)
          memo k (A.Out.dump_label (tr_lab lbl)) in
      { empty_ins with
        memo; inputs=[r;]; outputs=[];
        branch=[Next; Branch lbl] ; }

(* Load and Store *)

    let ldr_memo t = Misc.lowercase (ldr_memo t)
    let str_memo t = Misc.lowercase (str_memo t)

    let load memo v rD rA kr os = match v,kr,os with
    | V32,K 0, S_NOEXT ->
        { empty_ins with
          memo= sprintf "%s ^wo0,[^i0]" memo;
          inputs=[rA];
          outputs=[rD];
          reg_env=[(rA,voidstar);(rD,word)]; }
    | V32,K k, S_NOEXT ->
        { empty_ins with
          memo= sprintf "%s ^wo0,[^i0,#%i]" memo k;
          inputs=[rA];
          outputs=[rD];
          reg_env=[(rA,voidstar);(rD,word)];}
    | V32,RV (V32,rB), s ->
        let rB,fB = match rB with
        | ZR -> [],"wzr"
        | _  -> [rB],"^wi1" in
        let shift = match s with
        | S_NOEXT -> ""
        | s -> "," ^ pp_shifter s in
        { empty_ins with
          memo=memo^ sprintf " ^wo0,[^i0,%s%s]" fB shift;
          inputs=[rA]@rB;
          outputs=[rD];
          reg_env=add_w rB@[(rA,voidstar); (rD,word);]; }
    | V64,K 0, S_NOEXT ->
        { empty_ins with
          memo=memo ^ sprintf " ^o0,[^i0]";
          inputs=[rA];
          outputs=[rD];
          reg_env=[rA,voidstar;rD,quad;]; }
    | V64,K k, s ->
        let shift = match s with
        | S_NOEXT -> ""
        | s -> "," ^ pp_shifter s in
        { empty_ins with
          memo=memo ^ sprintf " ^o0,[^i0,#%i%s]" k shift;
          inputs=[rA];
          outputs=[rD];
          reg_env=[rA,voidstar; rD,quad;]; }
    | V64,RV (V64,rB), s ->
        let rB,fB = match rB with
        | ZR -> [],"xzr"
        | _  -> [rB],"^i1" in
        let shift = match s with
        | S_NOEXT -> ""
        | s       -> "," ^ pp_shifter s in
        { empty_ins with
          memo=memo^ sprintf " ^o0,[^i0,%s%s]" fB shift;
          inputs=[rA;]@rB;
          outputs=[rD];
          reg_env=add_q rB@[rA,voidstar;rD,quad]; }
    | V64,RV (V32,rB), s ->
        let rB,fB = match rB with
        | ZR -> [],"wzr"
        | _  -> [rB],"^wi1" in
        let shift = match s with
        | S_NOEXT -> ""
        | s -> "," ^ pp_shifter s in
        { empty_ins with
          memo=memo ^ sprintf " ^o0,[^i0,%s%s]" fB shift;
          inputs=[rA]@rB;
          outputs=[rD];
          reg_env=add_w rB@[rA,voidstar;rD,quad;]; }
    | V32,RV (V64,rB), s ->
        let rB,fB = match rB with
        | ZR -> [],"xzr"
        | _  -> [rB],"^i1" in
        let shift = match s with
        | S_NOEXT -> ""
        | s -> "," ^ pp_shifter s in
        { empty_ins with
          memo=memo^ sprintf " ^wo0,[^i0,%s%s]" fB shift;
          inputs=[rA;]@rB;
          outputs=[rD];
          reg_env=add_q rB@[rA,voidstar;rD,word]; }
    | _,_,_ -> assert false

    let load_p memo v rD rA k = match v with
    | V32 ->
        { empty_ins with
          memo= sprintf "%s ^wo0,[^i0],#%i" memo k;
          inputs=[rA];
          outputs=[rD];
          reg_env=[(rA,voidstar);(rD,word)];}
    | V64 ->
        { empty_ins with
          memo=memo ^ sprintf " ^o0,[^i0],#%i" k;
          inputs=[rA];
          outputs=[rD];
          reg_env=[rA,voidstar; rD,quad;]; }
    | V128 -> assert false

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
    | V128,_
    | _,RV (V128,_) -> assert false

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
    | V128,_
    | _,RV (V128,_) -> assert false


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
    | V128,_
    | _,RV (V128,_) -> assert false

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
          outputs = [r1;]; reg_env=[r3,voidstar; r2,quad; r1,word; ]}
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

    let load_simd memo v r1 r2 k os = match k,os with
    | K 0,S_NOEXT ->
      { empty_ins with
        memo = sprintf "%s %s,[^i0]" memo (print_vecreg v "o" 0);
        inputs = [r2];
        outputs = [r1];
        reg_env = [(r1,int128);(r2,voidstar)]}
    | K k,S_NOEXT ->
      { empty_ins with
        memo = sprintf "%s %s,[^i0,#%i]" memo (print_vecreg v "o" 0) k;
        inputs = [r2];
        outputs = [r1];
        reg_env = [(r1,int128);(r2,voidstar)]}
    | RV (V32,rk),S_NOEXT ->
      { empty_ins with
        memo = sprintf "%s %s,[^i0,^wi1]" memo (print_vecreg v "o" 0);
        inputs = [r2;rk;];
        outputs = [r1];
        reg_env = [(r1,int128);(r2,voidstar);(rk,word)]}
    | RV (V32,rk),s ->
      { empty_ins with
        memo = sprintf "%s %s,[^i0,^wi1,%s]" memo (print_vecreg v "o" 0) (pp_shifter s);
        inputs = [r2;rk;];
        outputs = [r1];
        reg_env = [(r1,int128);(r2,voidstar);(rk,word)]}
    | RV (V64,rk), S_NOEXT ->
      { empty_ins with
        memo = sprintf "%s %s,[^i0,^i1]" memo (print_vecreg v "o" 0);
        inputs = [r2;rk;];
        outputs = [r1];
        reg_env = [(r1,int128);(r2,voidstar);(rk,quad)]}
    | RV (V64,rk), s ->
      { empty_ins with
        memo = sprintf "%s %s,[^i0,^i1,%s]" memo (print_vecreg v "o" 0) (pp_shifter s);
        inputs = [r2;rk;];
        outputs = [r1];
        reg_env = [(r1,int128);(r2,voidstar);(rk,quad)]}
    | _ -> assert false

    let load_simd_p v r1 r2 k =
      { empty_ins with
        memo = sprintf "ldr %s, [^i0],#%i" (print_vecreg v "o" 0) k;
        inputs = [r2];
        outputs = [r1];
        reg_env = [(r1,int128);(r2,voidstar)]}

    let load_simd_s memo rs i rA kr = match kr with
    | K 0 ->
        { empty_ins with
          memo = sprintf "%s {%s}[%i],[^i0]" memo (print_simd_list rs "i" 1) i;
          inputs = rA::rs;
          outputs = [];
          reg_env = (add_128 rs) @ [(rA,voidstar)]}
    | K k ->
        { empty_ins with
          memo = sprintf "%s {%s}[%i],[^i0],#%i" memo (print_simd_list rs "i" 1) i k;
          inputs = rA::rs;
          outputs = [];
          reg_env = (add_128 rs) @ [(rA,voidstar)]}
    | RV (V64,rB) ->
        { empty_ins with
          memo = sprintf "%s {%s}[%i],[^i0],^i1" memo (print_simd_list rs "i" 2) i;
          inputs = [rA;rB;]@rs;
          outputs = [];
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

    let load_pair_simd memo v r1 r2 r3 kr = match v,kr with
    | VSIMD32,K 0 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0]" memo (print_vecreg VSIMD32 "o" 0) (print_vecreg VSIMD32 "o" 1);
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD32,K k ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0,#%i]" memo (print_vecreg VSIMD32 "o" 0) (print_vecreg VSIMD32 "o" 1) k;
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD64,K 0 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0]" memo (print_vecreg VSIMD64 "o" 0) (print_vecreg VSIMD64 "o" 1);
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD64,K k ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0,#%i]" memo (print_vecreg VSIMD64 "o" 0) (print_vecreg VSIMD64 "o" 1) k;
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD128,K 0 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0]" memo (print_vecreg VSIMD128 "o" 0) (print_vecreg VSIMD128 "o" 1);
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD128,K k ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0,#%i]" memo (print_vecreg VSIMD128 "o" 0) (print_vecreg VSIMD128 "o" 1) k;
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | _, _ -> assert false

    let load_pair_p_simd memo v r1 r2 r3 k = match v with
    | VSIMD32 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0],#%i" memo (print_vecreg VSIMD32 "o" 0) (print_vecreg VSIMD32 "o" 1) k;
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD64 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0],#%i" memo (print_vecreg VSIMD64 "o" 0) (print_vecreg VSIMD64 "o" 1) k;
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD128 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i0],#%i" memo (print_vecreg VSIMD128 "o" 0) (print_vecreg VSIMD128 "o" 1) k;
          inputs=[r3];
          outputs=[r1;r2;];
          reg_env= (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | _ -> assert false

    let store_simd memo v r1 r2 k os = match k,os with
    | K 0,S_NOEXT ->
      { empty_ins with
        memo = sprintf "%s %s,[^i1]" memo (print_vecreg v "i" 0);
        inputs = [r1;r2];
        outputs = [];
        reg_env = [(r1,int128);(r2,voidstar)]}
    | K k,S_NOEXT ->
      { empty_ins with
        memo = sprintf "%s %s,[^i1,#%i]" memo (print_vecreg v "i" 0) k;
        inputs = [r1;r2];
        outputs = [];
        reg_env = [(r1,int128);(r2,voidstar)]}
    | RV (V32,rk),S_NOEXT ->
      { empty_ins with
        memo = sprintf "%s %s,[^i1,^wi2]" memo (print_vecreg v "i" 0);
        inputs = [r1;r2;rk];
        outputs = [];
        reg_env = [(r1,int128);(r2,voidstar);(rk,word)]}
    | RV (V32,rk),s ->
      { empty_ins with
        memo = sprintf "%s %s,[^i1,^wi2,%s]" memo (print_vecreg v "i" 0) (pp_shifter s);
        inputs = [r1;r2;rk];
        outputs = [];
        reg_env = [(r1,int128);(r2,voidstar);(rk,word)]}
    | RV (V64,rk),S_NOEXT ->
      { empty_ins with
        memo = sprintf "%s %s,[^i1,^i2]" memo (print_vecreg v "i" 0);
        inputs = [r1;r2;rk];
        outputs = [];
        reg_env = [(r1,int128);(r2,voidstar);(rk,quad)]}
    | RV (V64,rk),s ->
      { empty_ins with
        memo = sprintf "%s %s,[^i1,^i2,%s]" memo (print_vecreg v "i" 0) (pp_shifter s);
        inputs = [r1;r2;rk];
        outputs = [];
        reg_env = [(r1,int128);(r2,voidstar);(rk,quad)]}
    | _ -> assert false

    let store_simd_p v r1 r2 k1 =
      { empty_ins with
        memo = sprintf "str %s, [^i1],%i" (print_vecreg v "i" 0) k1;
        inputs = [r1;r2];
        outputs = [];
        reg_env = [(r1,int128);(r2,voidstar)]}

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

    let store_pair_simd memo v r1 r2 r3 kr = match v,kr with
    | VSIMD32,K 0 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2]" memo (print_vecreg VSIMD32 "i" 0) (print_vecreg VSIMD32 "i" 1);
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD32,K k ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2,#%i]" memo (print_vecreg VSIMD32 "i" 0) (print_vecreg VSIMD32 "i" 1) k;
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD64,K 0 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2]" memo (print_vecreg VSIMD64 "i" 0) (print_vecreg VSIMD64 "i" 1);
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD64,K k ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2,#%i]" memo (print_vecreg VSIMD64 "i" 0) (print_vecreg VSIMD64 "i" 1) k;
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD128,K 0 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2]" memo (print_vecreg VSIMD128 "i" 0) (print_vecreg VSIMD128 "i" 1);
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD128,K k ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2,#%i]" memo (print_vecreg VSIMD128 "i" 0) (print_vecreg VSIMD128 "i" 1) k;
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | _,_ -> assert false

    let store_pair_p_simd memo v r1 r2 r3 k = match v with
    | VSIMD32 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2],#%i" memo (print_vecreg VSIMD32 "i" 0) (print_vecreg VSIMD32 "i" 1) k;
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD64 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2],#%i" memo (print_vecreg VSIMD64 "i" 0) (print_vecreg VSIMD64 "i" 1) k;
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | VSIMD128 ->
        { empty_ins with
          memo = sprintf "%s %s,%s,[^i2],#%i" memo (print_vecreg VSIMD128 "i" 0) (print_vecreg VSIMD128 "i" 1) k;
          inputs = [r1;r2;r3];
          outputs = [];
          reg_env = (add_128 [r1;r2;]) @ [(r3,voidstar)]}
    | _ -> assert false

    let mov_simd_ve r1 i1 r2 i2 =
      { empty_ins with
        memo = sprintf "mov %s[%i], %s[%i]" (print_simd_reg "o" 0 0 r1) i1 (print_simd_reg "i" 0 0 r2) i2;
        inputs = [r2];
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
        inputs = [r1;r2];
        outputs = [];
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

(* Compare and swap *)
    let type_of_variant = function
      | V32 -> word | V64 -> quad | V128 -> assert false

    let cas_memo rmw = Misc.lowercase (cas_memo rmw)
    let casbh_memo bh rmw = Misc.lowercase (casbh_memo bh rmw)

    let cas memo v r1 r2 r3 =
      let t = type_of_variant v in
      let r1,f1,r2,f2 = match v with
      | V32 -> args2 "wzr" (fun s -> "^wi"^s) r1 r2
      | V64 -> args2 "xzr" (fun s -> "^i"^s) r1 r2
      | V128 -> assert false in
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
      let t = type_of_variant v in
      let r1,f1 =
        match v with
        |  V32 -> arg1 "wzr" (fun s -> "^wi"^s) r1
        |  V64 -> arg1 "xzr" (fun s -> "^i"^s) r1
        |  V128 -> assert false in
      let idx = match r1 with | [] -> "0" | _::_ -> "1" in
      let r2,f2 =
        match v with
        |  V32 -> arg1 "wzr" (fun s -> "^wo"^s) r2
        |  V64 -> arg1 "xzr" (fun s -> "^o"^s) r2
        |  V128 -> assert false in
      { empty_ins with
        memo = sprintf "%s %s,%s,[^i%s]" memo f1 f2 idx;
        inputs = r1@[r3;]; outputs =r2;
        reg_env = (r3,voidstar)::add_type t (r1@r2); }

(* Fetch and Op *)
    let ldop_memo op rmw = Misc.lowercase (ldop_memo op rmw)
    let stop_memo op w = Misc.lowercase (stop_memo op w)
    let ldopbh_memo op bh rmw =  Misc.lowercase (ldopbh_memo op bh rmw)
    let stopbh_memo op bh w =  Misc.lowercase (stopbh_memo op bh w)

    let ldop memo v rs rt rn =
      let t = match v with | V32 -> word | V64 -> quad | V128 -> assert false in
      let rs,fs = match v with
      |  V32 -> arg1 "wzr" (fun s -> "^wi"^s) rs
      |  V64 -> arg1 "xzr" (fun s -> "^i"^s) rs
      |  V128 -> assert false in
      let idx = match rs with | [] -> "0" | _::_ -> "1" in
      let rt,ft =  match v with
      |  V32 -> arg1 "wzr" (fun s -> "^wo"^s) rt
      |  V64 -> arg1 "xzr" (fun s -> "^o"^s) rt
      |  V128 -> assert false in
      { empty_ins with
        memo = sprintf "%s %s,%s,[^i%s]" memo fs ft idx;
        inputs = rs@[rn]; outputs = rt;
        reg_env = (rn,voidstar)::add_type t (rs@rt);}

    let stop memo v rs rn =
      let t = match v with | V32 -> word | V64 -> quad | V128 -> assert false in
      let rs,fs = match v with
      |  V32 -> arg1 "wzr" (fun s -> "^wi"^s) rs
      |  V64 -> arg1 "xzr" (fun s -> "^i"^s) rs
      |  V128 -> assert false in
      let idx = match rs with | [] -> "0" | _::_ -> "1" in
      { empty_ins with
        memo = sprintf "%s %s,[^i%s]" memo fs idx;
        inputs = rs@[rn]; outputs = [];
        reg_env = (rn,voidstar)::add_type t rs;}

(* Arithmetic *)
    let movk v r k =
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
      let r,f = arg1 "xzr" (fun s -> "^o"^s) r in
      { empty_ins with
        memo = sprintf "adr %s,%s" f (A.Out.dump_label (tr_lab lbl));
        outputs=r; reg_env=add_v r; }

    let do_movr memo v r1 r2 = match v with
    | V32 ->
        let r1,f1 = arg1 "wzr" (fun s -> "^wo"^s) r1
        and r2,f2 = arg1 "wzr" (fun s -> "^wi"^s) r2 in
        { empty_ins with
          memo=sprintf "%s %s,%s" memo f1 f2;
          inputs = r2; outputs=r1; reg_env=add_w (r1@r2);}
    | V64 ->
        let r1,f1 = arg1 "xzr" (fun s -> "^o"^s) r1
        and r2,f2 = arg1 "xzr" (fun s -> "^i"^s) r2 in
        { empty_ins with
          memo=sprintf "%s %s,%s" memo f1 f2;
          inputs = r2; outputs=r1; reg_env=add_q (r1@r2);}
    | V128 -> assert false

    let do_movz memo v rd k os = match v, k, os with
    | V32, k, S_LSL(s) ->
        let r1,f1 = arg1 "wzr" (fun s -> "^wo"^s) rd in
        { empty_ins with
          memo=sprintf "%s %s, #%d, %s" memo f1 k (pp_shifter (S_LSL s));
          outputs=r1; reg_env=add_w r1;}
    | V32,  k, S_NOEXT ->
        let r1,f1 = arg1 "wzr" (fun s -> "^wo"^s) rd in
        { empty_ins with
          memo=sprintf "%s %s, #%d" memo f1 k;
          outputs=r1; reg_env=add_w r1;}
    | V64, k, S_LSL(s) ->
        let r1,f1 = arg1 "xzr" (fun s -> "^o"^s) rd in
        { empty_ins with
          memo=sprintf "%s %s, #%d, %s" memo f1 k (pp_shifter (S_LSL s));
          outputs=r1; reg_env=add_q r1;}
    | V64, k, S_NOEXT ->
        let r1,f1 = arg1 "xzr" (fun s -> "^o"^s) rd in
        { empty_ins with
          memo=sprintf "%s %s, #%d" memo f1 k;
          outputs=r1; reg_env=add_q r1;}
    | _ -> Warn.fatal "Illegal form of %s instruction" memo

    let movr = do_movr "mov"
    and movz = do_movz "movz"
    and movk' = do_movz "movk"
    and rbit = do_movr "rbit"

    let sxtw r1 r2 =
      { empty_ins with
        memo = "sxtw ^o0,^wi0";
        inputs = [r2;]; outputs=[r1;]; reg_env=[r1,quad; r2,word];}

    let cmpk v r k = match v with
    | V32 ->
        { empty_ins with
          memo = sprintf "cmp ^wi0,#%i" k ;
          inputs = [r;]; reg_env=[r,word];}
    | V64 ->
        { empty_ins with
          memo = sprintf "cmp ^i0,#%i" k ;
          inputs = [r;]; reg_env=[r,quad;];}
    | V128 -> assert false

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
    | V128 -> assert false

    let tst v r i =
      let add,(r,f) =  match v with
      | V32 -> add_w,arg1 "wzr" (fun s -> "^wo"^s) r
      | V64 -> add_q,arg1 "xzr" (fun s -> "^o"^s) r
      | V128 -> assert false in
      { empty_ins with
        memo = sprintf "tst %s,#%i" f i;
        outputs=r; reg_env = add r;}

    let memo_of_op op = Misc.lowercase (pp_op op)

    let op3 v op rD rA kr s =
      let memo = memo_of_op op in
      let shift = Misc.lowercase (pp_barrel_shift "," s pp_imm) in
      match v,kr with
      | V32,K k ->
          let rD,fD = arg1 "wzr" (fun s -> "^wo"^s) rD
          and rA,fA = arg1 "wzr" (fun s -> "^wi"^s) rA in
          { empty_ins with
            memo=sprintf "%s %s,%s,#%i%s" memo fD fA k shift;
            inputs=rA;
            outputs=rD; reg_env = add_w (rA@rD);}
      | V32,RV (V32,rB) ->
          let rD,fD = arg1 "wzr" (fun s -> "^wo"^s) rD
          and rA,fA,rB,fB = args2 "wzr"  (fun s -> "^wi"^s) rA rB in
          let inputs = rA@rB in
          { empty_ins with
            memo=sprintf "%s %s,%s,%s%s" memo fD fA fB shift;
            inputs=inputs;
            outputs=rD; reg_env = add_w (rD@inputs);}
      | V64,K k ->
          let rD,fD = arg1 "xzr" (fun s -> "^o"^s) rD
          and rA,fA = arg1 "xzr" (fun s -> "^i"^s) rA in
          { empty_ins with
            memo=sprintf "%s %s,%s,#%i%s" memo fD fA k shift;
            inputs=rA;
            outputs=rD; reg_env = add_q (rA@rD);}
      | V64,RV (V64,rB) ->
          let rD,fD = arg1 "xzr" (fun s -> "^o"^s) rD
          and rA,fA,rB,fB = args2 "xzr"  (fun s -> "^i"^s) rA rB in
          let inputs = rA@rB in
          { empty_ins with
            memo=sprintf "%s %s,%s,%s%s" memo fD fA fB shift;
            inputs=inputs;
            outputs=rD; reg_env=add_q (inputs@rD);}
      | V64,RV (V32,rB) ->
          let rD,fD = arg1 "xzr" (fun s -> "^o"^s) rD
          and rA,fA = arg1 "xzr"  (fun s -> "^i"^s) rA in
          let rB,fB = match rB with
          | ZR -> [],"wzr"
          | _ -> begin match rA with
            | [] -> [rB],"^wi0"
            | _ -> [rB],"^wi1"
          end in
          { empty_ins with
            memo=sprintf "%s %s,%s,%s%s" memo fD fA fB shift;
            inputs=rA@rB;
            outputs=rD; reg_env=add_q (rD@rA)@add_w rB; }
      | V32,RV (V64,_) -> assert false
      | V128,_
      | _,RV (V128,_) -> assert false

    let fence f =
      { empty_ins with memo = Misc.lowercase (A.pp_barrier f); }

    let cache memo r =
      let r,f = arg1 "xzr" (fun s -> "^i"^s) r in
      { empty_ins with memo = memo ^ "," ^ f; inputs=r; reg_env=add_v r; }

(* Not that useful *)
    let emit_loop _k = assert false

    let compile_ins tr_lab ins k = match ins with
    | I_NOP -> { empty_ins with memo = "nop"; }::k
(* Branches *)
    | I_B lbl -> b tr_lab lbl::k
    | I_BR r -> br r::k
    | I_BL lbl -> bl tr_lab lbl::k
    | I_BLR r -> blr r::k
    | I_RET None -> { empty_ins with memo="ret"; }::k
    | I_RET (Some r) -> ret r::k
    | I_BC (c,lbl) -> bcc tr_lab c lbl::k
    | I_CBZ (v,r,lbl) -> cbz tr_lab "cbz" v r lbl::k
    | I_CBNZ (v,r,lbl) -> cbz tr_lab "cbnz" v r lbl::k
    | I_TBNZ (v,r,k2,lbl) -> tbz tr_lab "tbnz" v r k2 lbl::k
    | I_TBZ (v,r,k2,lbl) -> tbz tr_lab "tbz" v r k2 lbl::k
(* Load and Store *)
    | I_LDR (v,r1,r2,kr,os) -> load "ldr" v r1 r2 kr os::k
    | I_LDUR (v,r1,r2,Some(k')) -> load "ldur" v r1 r2 (K k') S_NOEXT ::k
    | I_LDUR (v,r1,r2,None) -> load "ldur" v r1 r2 (K 0) S_NOEXT ::k
    | I_LDR_P (v,r1,r2,k1) -> load_p "ldr" v r1 r2 k1::k
    | I_LDP (t,v,r1,r2,r3,kr) ->
        load_pair (match t with TT -> "ldp" | NT -> "ldnp") v r1 r2 r3 kr::k
    | I_STP (t,v,r1,r2,r3,kr) ->
        store_pair (match t with TT -> "stp" | NT -> "stnp") v r1 r2 r3 kr::k
    | I_LDRBH (B,r1,r2,kr) -> load "ldrb" V32 r1 r2 kr (default_shift kr)::k
    | I_LDRBH (H,r1,r2,kr) -> load "ldrh" V32 r1 r2 kr (default_shift kr)::k
    | I_LDAR (v,t,r1,r2) -> load (ldr_memo t) v r1 r2 k0 S_NOEXT::k
    | I_LDARBH (bh,t,r1,r2) -> load (ldrbh_memo bh t) V32 r1 r2 k0 S_NOEXT::k
    | I_STR (v,r1,r2,kr) -> store "str" v r1 r2 kr::k
    | I_STRBH (B,r1,r2,kr) -> store "strb" V32 r1 r2 kr::k
    | I_STRBH (H,r1,r2,kr) -> store "strh" V32 r1 r2 kr::k
    | I_STLR (v,r1,r2) -> store "stlr" v r1 r2 k0::k
    | I_STLRBH (B,r1,r2) -> store "stlrb" V32 r1 r2 k0::k
    | I_STLRBH (H,r1,r2) -> store "stlrh" V32 r1 r2 k0::k
    | I_STXR (v,t,r1,r2,r3) -> stxr (str_memo t) v r1 r2 r3::k
    | I_STXRBH (bh,t,r1,r2,r3) -> stxr (strbh_memo bh t) V32 r1 r2 r3::k
    | I_CAS (v,rmw,r1,r2,r3) -> cas (cas_memo rmw) v r1 r2 r3::k
    | I_CASBH (bh,rmw,r1,r2,r3) -> cas (casbh_memo bh rmw) V32 r1 r2 r3::k
    | I_SWP (v,rmw,r1,r2,r3) -> swp (swp_memo rmw) v r1 r2 r3::k
    | I_SWPBH (bh,rmw,r1,r2,r3) -> swp (swpbh_memo bh rmw) V32 r1 r2 r3::k
(* Neon Extension Load and Store *)
    | I_LD1 (r1,i,r2,kr) -> load_simd_s "ld1" [r1] i r2 kr::k
    | I_LD1M (rs,r2,kr) -> load_simd_m "ld1" rs r2 kr::k
    | I_LD1R (r1,r2,kr) -> load_simd_m "ld1r" [r1] r2 kr::k
    | I_LD2 (rs,i,r2,kr) -> load_simd_s "ld2" rs i r2 kr::k
    | I_LD2M (rs,r2,kr) -> load_simd_m "ld2" rs r2 kr::k
    | I_LD2R (rs,r2,kr) -> load_simd_m "ld2r" rs r2 kr::k
    | I_LD3 (rs,i,r2,kr) -> load_simd_s "ld3" rs i r2 kr::k
    | I_LD3M (rs,r2,kr) -> load_simd_m "ld3" rs r2 kr::k
    | I_LD3R (rs,r2,kr) -> load_simd_m "ld3r" rs r2 kr::k
    | I_LD4 (rs,i,r2,kr) -> load_simd_s "ld4" rs i r2 kr::k
    | I_LD4M (rs,r2,kr) -> load_simd_m "ld4" rs r2 kr::k
    | I_LD4R (rs,r2,kr) -> load_simd_m "ld4r" rs r2 kr::k
    | I_ST1 (r1,i,r2,kr) -> store_simd_s "st1" [r1] i r2 kr::k
    | I_ST1M (rs,r2,kr) -> store_simd_m "st1" rs r2 kr::k
    | I_ST2 (rs,i,r2,kr) -> store_simd_s "st2" rs i r2 kr::k
    | I_ST2M (rs,r2,kr) -> store_simd_m "st2" rs r2 kr::k
    | I_ST3 (rs,i,r2,kr) -> store_simd_s "st3" rs i r2 kr::k
    | I_ST3M (rs,r2,kr) -> store_simd_m "st3" rs r2 kr::k
    | I_ST4 (rs,i,r2,kr) -> store_simd_s "st4" rs i r2 kr::k
    | I_ST4M (rs,r2,kr) -> store_simd_m "st4" rs r2 kr::k
    | I_LDP_SIMD (t,v,r1,r2,r3,kr) ->
        load_pair_simd (match t with TT -> "ldp" | NT -> "ldnp") v r1 r2 r3 kr::k
    | I_STP_SIMD (t,v,r1,r2,r3,kr) ->
        store_pair_simd (match t with TT -> "stp" | NT -> "stnp") v r1 r2 r3 kr::k
    | I_LDP_P_SIMD (t,v,r1,r2,r3,k1) ->
        load_pair_p_simd (match t with TT -> "ldp" | NT -> "ldnp") v r1 r2 r3 k1::k
    | I_STP_P_SIMD (t,v,r1,r2,r3,k1) ->
        store_pair_p_simd (match t with TT -> "stp" | NT -> "stnp") v r1 r2 r3 k1::k
    | I_LDR_SIMD (v,r1,r2,k1,s) -> load_simd "ldr" v r1 r2 k1 s::k
    | I_LDR_P_SIMD (v,r1,r2,k1) -> load_simd_p v r1 r2 k1::k
    | I_STR_SIMD (v,r1,r2,k1,s) -> store_simd "str" v r1 r2 k1 s::k
    | I_STR_P_SIMD (v,r1,r2,k1) -> store_simd_p v r1 r2 k1::k
    | I_LDUR_SIMD (v,r1,r2,Some(k1)) -> load_simd "ldur" v r1 r2 (K k1) S_NOEXT::k
    | I_LDUR_SIMD (v,r1,r2,None) -> load_simd "ldur" v r1 r2 (K 0) S_NOEXT::k
    | I_STUR_SIMD (v,r1,r2,Some(k1)) -> store_simd "stur" v r1 r2 (K k1) S_NOEXT::k
    | I_STUR_SIMD (v,r1,r2,None) -> store_simd "stur" v r1 r2 (K 0) S_NOEXT::k
    | I_MOV_VE (r1,i1,r2,i2) -> mov_simd_ve r1 i1 r2 i2::k
    | I_MOV_V (r1,r2) -> mov_simd_v r1 r2::k
    | I_MOV_FG (r1,i,v,r2) -> mov_simd_fg r1 i v r2::k
    | I_MOV_TG (v,r1,r2,i) -> mov_simd_tg v r1 r2 i::k
    | I_MOVI_S (v,r,k1) -> movi_s v r k1::k
    | I_MOVI_V (r,kr,s) -> movi_v r kr s::k
    | I_MOV_S (v,r1,r2,i) -> mov_simd_s v r1 r2 i::k
    | I_EOR_SIMD (r1,r2,r3) -> eor_simd r1 r2 r3::k
    | I_ADD_SIMD (r1,r2,r3) -> add_simd r1 r2 r3::k
    | I_ADD_SIMD_S (r1,r2,r3) -> add_simd_s r1 r2 r3::k
(* Arithmetic *)
    | I_MOV (v,r,K i) ->  movk v r i::k
    | I_MOV (v,r1,RV (_,r2)) ->  movr v r1 r2::k
    | I_MOVZ (v,rd,i,os) -> movz v rd i os::k
    | I_MOVK (v,rd,i,os) -> movk' v rd i os::k
    | I_ADDR (r,lbl) -> adr tr_lab r lbl::k
    | I_RBIT (v,rd,rs) -> rbit v rd rs::k
    | I_SXTW (r1,r2) -> sxtw r1 r2::k
    | I_OP3 (v,SUBS,ZR,r,K i, S_NOEXT) ->  cmpk v r i::k
    | I_OP3 (v,SUBS,ZR,r2,RV (v3,r3), S_NOEXT) when v=v3->  cmp v r2 r3::k
    | I_OP3 (v,ANDS,ZR,r,K i, S_NOEXT) -> tst v r i::k
    | I_OP3 (V64,_,_,_,RV(V32,_),S_NOEXT) ->
        Warn.fatal "Instruction %s is illegal (extension required)"
          (dump_instruction ins)
    | I_OP3 (v,op,r1,r2,kr,s) ->  op3 v op r1 r2 kr s::k
(* Fence *)
    | I_FENCE f -> fence f::k
(* Fetch and Op *)
    |I_LDOP (op,v,rmw,rs,rt,rn) ->
        ldop (ldop_memo op rmw) v rs rt rn::k
    |I_LDOPBH  (op,v,rmw,rs,rt,rn) ->
        ldop (ldopbh_memo op v rmw) V32 rs rt rn::k
    | I_STOP (op,v,w,rs,rn) ->
        stop (stop_memo op w) v rs rn::k
    | I_STOPBH (op,v,w,rs,rn) ->
        stop (stopbh_memo op v w) V32 rs rn::k
(* Conditional selection *)
    | I_CSEL (v,r,ZR,ZR,c,Inc) ->
        let o,f = match v with
        | V32 -> arg1 "wzr" (fun s -> "^wo"^s) r
        | V64 -> arg1 "xzr" (fun s -> "^o"^s) r
        | V128 -> assert false
        and t = match v with V32 -> word | V64 -> quad | V128 -> assert false in
        let memo =
          sprintf "cset %s,%s" f (pp_cond (inverse_cond c)) in
        { empty_ins with memo; outputs=o; reg_env=add_type t o; }::k
    | I_CSEL (v,r1,r2,r3,c,op) ->
        let inputs,memo,t = match v with
        | V32 ->
            let r2,f2,r3,f3 = args2 "wzr" (fun s -> "^wi"^s) r2 r3 in
            r2@r3,sprintf "%s,%s,%s" f2 f3 (pp_cond c),word
        | V64 ->
            let r2,f2,r3,f3 = args2 "xzr" (fun s -> "^i"^s) r2 r3 in
            r2@r3,sprintf "%s,%s,%s" f2 f3 (pp_cond c),quad
        | V128 -> assert false in
        let o,f = match v with
        | V32 -> arg1 "wzr" (fun s -> "^wo"^s) r1
        | V64 -> arg1 "xzr" (fun s -> "^o"^s) r1
        | V128 -> assert false in
        let memo = Misc.lowercase (sel_memo op) ^ " " ^ f ^ "," ^ memo in
        {
         empty_ins with
         memo = memo; inputs=inputs; outputs=o;
         reg_env=add_type t (o@inputs);
       }::k
    | I_IC (op,r) ->
        cache (sprintf "ic %s" (Misc.lowercase (IC.pp_op op))) r::k
    | I_DC (op,r) ->
        cache (sprintf "dc %s" (Misc.lowercase (DC.pp_op op))) r::k
    | I_MRS (r,sr) ->
        let r,f = arg1 "xzr" (fun s -> "^o"^s) r in
        let memo =
          sprintf "mrs %s,%s" f (Misc.lowercase (pp_sysreg sr)) in
        {empty_ins with
         memo; outputs=r; reg_env=add_type quad r;}::k
    | I_STG _| I_STZG _|I_LDG _ ->
        Warn.fatal "No litmus output for instruction %s"
          (dump_instruction ins)
    | I_ALIGND _|I_ALIGNU _|I_BUILD _|I_CHKEQ _|I_CHKSLD _|I_CHKTGD _|
      I_CLRTAG _|I_CPYTYPE _|I_CPYVALUE _|I_CSEAL _|I_GC _|I_LDCT _|I_SC _|
      I_SEAL _|I_STCT _|I_UNSEAL _ ->
        Warn.fatal "No litmus output for instruction %s"
            (dump_instruction ins)

    let no_tr lbl = lbl
    let branch_neq r i lab k = cmpk V32 r i::bcc no_tr NE lab::k
    let branch_eq r i lab k = cmpk V32 r i::bcc no_tr EQ lab::k

    let signaling_write _i _k = Warn.fatal "no signaling write for ARM"

    let emit_tb_wait _ = Warn.fatal "no time base for ARM"
  end
