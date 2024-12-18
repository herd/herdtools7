(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

let aarch64_iico_ctrl = "aarch64_iico_ctrl"
let aarch64_iico_data = "aarch64_iico_data"
let aarch64_iico_order = "aarch64_iico_order"

let return_i i =
  let open Asllib.AST in
  let open Asllib.ASTUtils in
  add_dummy_annotation (S_Return (Some (expr_of_int i)))

let return_0 = return_i 0

let catch_silent_exit body =
  let open Asllib.AST in
  let open Asllib.ASTUtils in
  let exit_type : Asllib.AST.ty =
    add_dummy_annotation (T_Named "SilentExit") in
  let catcher = (None,exit_type,return_0) in
  add_dummy_annotation (S_Try (body,[catcher],None))

module Make (TopConf : AArch64Sig.Config) (V : Value.AArch64ASL) :
  AArch64Sig.Semantics with module A.V = V = struct
  module AArch64S = AArch64Sem.Make (TopConf) (V)
  include AArch64S

  let ( ||| ) = M.( ||| )
  let ( let* ) = M.( >>= )
  let return = M.unitT
  let _dbg = TopConf.C.debug.Debug_herd.monad

  module Mixed (SZ : ByteSize.S) : sig
    val build_semantics : test -> A.inst_instance_id -> (proc * branch) M.t
    val spurious_setaf : V.v -> unit M.t
  end = struct
    module AArch64Mixed = AArch64S.Mixed (SZ)

    let aarch64_iico =
      StringSet.of_list
        [ aarch64_iico_ctrl; aarch64_iico_data; aarch64_iico_order ]

    module ASLConf = struct
      module C = struct
        include TopConf.C
        module PC = struct
          include TopConf.C.PC

          let doshow = aarch64_iico
          let showevents = PrettyConf.AllEvents
          let showpo = true
          let showraw = aarch64_iico
        end
        let variant = function Variant.ASL_AArch64 -> true | c -> variant c
      end

      let libfind = TopConf.C.libfind
      let dirty = TopConf.dirty
    end

    module ASLS = ASLSem.Make (ASLConf)
    module ASLE = ASLS.E
    module EMap = ASLE.EventMap
    module ESet = ASLE.EventSet
    module ASLVC = ASLS.M.VC
    module ASLTH = Test_herd.Make (ASLS.A)

    module MCConf = struct
      include ASLConf.C

      let byte = SZ.byte
      let dirty = TopConf.dirty
      let initwrites = false
    end

    module MC = Mem.Make (MCConf) (ASLS)
    module MU = MemUtils.Make (ASLS)

    type asl_exec = ASLS.concrete * ASLVC.cnstrnts * ASLS.set_pp * ASLS.rel_pp

    let tr_cond =
      let open AArch64Base in
      (* Cf ARM Architecture Reference Manual, section C1.2.4, table C1-1 *)
      function
      | EQ -> 0b0000
      | NE -> 0b0001
      | CS -> 0b0010
      | CC -> 0b0011
      | MI -> 0b0100
      | PL -> 0b0101
      | VS -> 0b0110
      | VC -> 0b0111
      | HI -> 0b1000
      | LS -> 0b1001
      | GE -> 0b1010
      | LT -> 0b1011
      | GT -> 0b1100
      | LE -> 0b1101
      | AL -> 0b1111 (* Also possible [0b1110] *)

    let barrier_domain =
      let open AArch64Base in
      function
      | NSH -> "MBReqDomain_Nonshareable"
      | ISH -> "MBReqDomain_InnerShareable"
      | OSH -> "MBReqDomain_OuterShareable"
      | SY -> "MBReqDomain_FullSystem"

    and barrier_typ =
      let open AArch64Base in
      function
      | LD -> "MBReqTypes_Reads"
      | ST -> "MBReqTypes_Writes"
      | FULL -> "MBReqTypes_All"

    let unalias ii =
      let i0 = ii.A.inst in
      let i = AArch64Base.unalias i0 in
      if i == i0 then ii else { ii with A.inst = i }

    let opext_decode_shift =
      let open AArch64Base.OpExt in
      function
      | LSL _ -> "ShiftType_LSL"
      | LSR _ -> "ShiftType_LSR"
      | ASR _ -> "ShiftType_ASR"
      | ROR _ -> "ShiftType_ROR"

    let memext_decode_ext =
      let open AArch64Base.MemExt in
      function
      | UXTW -> "ExtendType_UXTW"
      | SXTW -> "ExtendType_SXTW"
      | SXTX -> "ExtendType_SXTX"
      | LSL -> "ExtendType_UXTX"

    let opext_shift_amount =
      let open AArch64Base.OpExt in
      function LSL k | LSR k | ASR k | ROR k -> k

    let decode_acquire =
      let open AArch64 in
      function RMW_P | RMW_L -> false | RMW_A | RMW_AL -> true

    and decode_release =
      let open AArch64 in
      function RMW_P | RMW_A -> false | RMW_L | RMW_AL -> true

    let decode_inst ii =
      let ii = unalias ii in
      let open Asllib.AST in
      let with_pos desc = Asllib.ASTUtils.add_dummy_annotation ~version:V0 desc in
      let ( ^= ) x e = S_Decl (LDK_Let, LDI_Var x, None, Some e) |> with_pos in
      let ( ^^= ) x e =
        let le_x = LE_Var x |> with_pos in
        S_Assign (le_x, e) |> with_pos in
      let lit v = E_Literal v |> with_pos in
      let liti i = lit (L_Int (Z.of_int i)) in
      let litb b = lit (L_Bool b) in
      let litbv v i = lit (L_BitVector (Asllib.Bitvector.of_int_sized v i)) in
      let var x = E_Var x |> with_pos in
      let variant v = AArch64Base.variant_raw v |> liti in
      let cond c = tr_cond c |> liti in
      let stmt = Asllib.ASTUtils.stmt_from_list in
      let open AArch64Base in
      let reg = function
        (* To use with caution, sometimes it doesn't work. *)
        | Ireg r -> ASLBase.arch_reg_to_int r |> liti
        | ZR -> liti 31
        | SP -> liti 31
        | NZCV -> Warn.fatal "NZCV is not an addressable register"
        | r -> Warn.fatal "Unsupported register: %s." (pp_reg r)
      in
      match ii.A.inst with
      | I_NOP ->
         let added =
           (* ASL implementation is "return;" that our interpreter rejects,
              expecting integer return... *)
           ASLBase.stmts_from_string "return 0;" in
         Some
           ("system/hints/NOP_HI_hints.opn",stmt [added;])
      | I_B lab ->
         let off = tgt2offset ii lab in
         Some ("branch/unconditional/immediate/B_only_branch_imm.opn",
               stmt
                 [
                   "offset" ^= litbv 64 off;
                   "_PC" ^^= litbv 64 ii.A.addr; ])
      | I_CBZ (v,rt,lab)
      | I_CBNZ (v,rt,lab) as i
        ->
         let off = tgt2offset ii lab in
         let file =
           match i with
           | I_CBZ _ -> "CBZ_32_compbranch.opn"
           | I_CBNZ _ -> "CBNZ_32_compbranch.opn"
           | _ -> assert false in
         Some
           ("branch/conditional/compare/" ^ file,
            stmt
              [
                "t" ^= reg rt;
                "datasize" ^= variant v;
                "offset" ^= litbv 64 off;
                "_PC" ^^= litbv 64 ii.A.addr;
              ])
      | I_BC (c,lab)
        ->
         let off = tgt2offset ii lab in
         Some
           ("branch/conditional/cond/B_only_condbranch.opn",
            stmt
              [
                "offset" ^= litbv 64 off;
                "cond" ^= cond c;
                "_PC" ^^= litbv 64 ii.A.addr;
              ])

      | I_SWP (v, t, rs, rt, rn) ->
          Some
            ( "memory/atomicops/swp/SWP_32_memop.opn",
              stmt
                [
                  "s" ^= reg rs;
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "datasize" ^= variant v;
                  "regsize" ^= liti 64;
                  "acquire" ^= litb (decode_acquire t && rt <> ZR);
                  "release" ^= litb (decode_release t);
                  "tagchecked" ^= litb (rn <> SP);
                ] )
      | I_CAS (v, t, rs, rt, rn) ->
          Some
            ( "memory/atomicops/cas/single/CAS_C32_comswap.opn",
              stmt
                [
                  "s" ^= reg rs;
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "datasize" ^= variant v;
                  "regsize" ^= variant v;
                  "acquire" ^= litb (decode_acquire t);
                  "release" ^= litb (decode_release t);
                  "tagchecked" ^= litb (rn <> SP);
                ] )
      | I_LDOP (op,v,rmw,rs,rt,rn) ->
         let fname =
           Printf.sprintf
             "memory/atomicops/ld/LD%s_32_memop.opn"
             (pp_aop op) in
         Some
           (fname,
            stmt
              [
                "s" ^= reg rs;
                "t" ^= reg rt;
                "n" ^= reg rn;
                "datasize" ^= variant v;
                "regsize" ^= variant v;
                "acquire" ^= litb (decode_acquire rmw && rt <> ZR);
                "release" ^= litb (decode_release rmw);
                "tagchecked" ^= litb (rn <> SP);
           ])
      | I_CSEL (v, rd, rn, rm, c, opsel) ->
          let fname =
            match opsel with
            | Cpy -> "CSEL_32_condsel.opn"
            | Inc -> "CSINC_32_condsel.opn"
            | Inv -> "CSINV_32_condsel.opn"
            | Neg -> "CSNEG_32_condsel.opn"
          in
          Some
            ( "integer/conditional/select/" ^ fname,
              stmt
                [
                  "d" ^= reg rd;
                  "n" ^= reg rn;
                  "m" ^= reg rm;
                  "datasize" ^= variant v;
                  "cond" ^= cond c;
                ] )
      | ( I_MOVZ (v, rd, k, ((S_NOEXT | S_LSL (0 | 16 | 32 | 48)) as s))
        | I_MOVN (v, rd, k, ((S_NOEXT | S_LSL (0 | 16 | 32 | 48)) as s)) ) as i
        ->
          let datasize = variant_raw v in
          let pos =
            match s with S_NOEXT -> 0 | S_LSL s -> s | _ -> assert false
          in
          let fname =
            match i with
            | I_MOVZ _ -> "MOVZ_32_movewide.opn"
            | I_MOVN _ -> "MOVN_32_movewide.opn"
            | _ -> assert false
          in
          Some
            ( "integer/ins-ext/insert/movewide/" ^ fname,
              stmt
                [
                  "d" ^= reg rd;
                  "imm16" ^= litbv 16 k;
                  "datasize" ^= liti datasize;
                  "pos" ^= liti pos;
                ] )
      | I_ABS (v, rd, rn) ->
          let datasize = variant_raw v in
          Some
            ( "integer/arithmetic/unary/abs/ABS_32_dp_1src.opn",
              stmt [ "d" ^= reg rd; "n" ^= reg rn; "datasize" ^= liti datasize ]
            )
      | I_RBIT (v, rd, rn) ->
          let datasize = variant_raw v in
          Some
            ( "integer/arithmetic/rbit/RBIT_32_dp_1src.opn",
              stmt [ "d" ^= reg rd; "n" ^= reg rn; "datasize" ^= liti datasize ]
            )
      (*
       * Does not work, because instruction code uses the Elem
       * setter `Elem[..] = ...`. This setter relies on passing argument
       * by reference.
       *)
      (*
      | I_REV (rv,rd,rn) ->
         let datasize = variant_of_rev rv |> variant_raw in
         let csz = container_size rv |> MachSize.nbits in
         Printf.eprintf "REV: sz=%i, csz=%i\n%!" datasize csz ;
         let fname =
           match rv with
           | RV16 _ -> "REV16_32_dp_1src.opn"
           | RV32 -> "REV32_64_dp_1src.opn"
           | RV64 _ -> "REV_32_dp_1src.opn" in
         Some
           ("/integer/arithmetic/rev/" ^ fname,
            stmt
              [
                "d" ^= reg rd;
                "n" ^= reg rn;
                "datasize" ^= liti datasize;
                "container_size" ^= liti csz;
           ])
       *)
      | I_EXTR (v,rd,rn,rm,imms) ->
         Some
           ("integer/ins-ext/extract/immediate/EXTR_32_extract.opn",
            stmt
              [
                "d" ^= reg rd;
                "n" ^= reg rn;
                "m" ^= reg rm;
                "datasize" ^= variant v;
                "lsb" ^= liti imms;])
      | I_UBFM (v, rd, rn, immr, imms) | I_SBFM (v, rd, rn, immr, imms) ->
          let datasize = variant_raw v in
          let bitvariant =
            let open AArch64Base in
            match v with V64 -> 1 | V32 -> 0 | V128 -> assert false
          in
          let extend =
            match ii.A.inst with
            | I_SBFM _ -> true
            | I_UBFM _ -> false
            | _ -> assert false
          in
          let added =
            ASLBase.stmts_from_string
              "let r = UInt(immr);\n\
               let s = UInt(imms);\n\
               var wmask : bits(datasize);\n\
               var tmask : bits(datasize) ;\n\
               (wmask,tmask) = DecodeBitMasks{datasize}(N, imms, immr, FALSE, datasize);"
          in
          let fname =
            if extend then "integer/bitfield/SBFM_32M_bitfield.opn"
            else "integer/bitfield/UBFM_32M_bitfield.opn"
          in
          Some
            ( fname,
              stmt
                ([
                   "d" ^= reg rd;
                   "n" ^= reg rn;
                   "immr" ^= litbv 6 immr;
                   "imms" ^= litbv 6 imms;
                   "N" ^= litbv 1 bitvariant;
                   "datasize" ^= liti datasize;
                   "inzero" ^= litb true;
                 ]
                @ [ added ]) )
      | I_ADDSUBEXT
          (v, Ext.((ADD | ADDS | SUB | SUBS) as op), rd, rn, (_vm, rm), (e, ko))
        ->
          let datasize = variant_raw v in
          let fname =
            let open Ext in
            match op with
            | ADD -> "ADD_32_addsub_ext.opn"
            | ADDS -> "ADDS_32_addsub_ext.opn"
            | SUB -> "SUB_32_addsub_ext.opn"
            | SUBS -> "SUBS_32_addsub_ext.opn"
          in
          let base = "integer/arithmetic/add-sub/extendedreg/" in
          let extend_type =
            let open Ext in
            match e with
            | UXTB -> "ExtendType_UXTB"
            | UXTH -> "ExtendType_UXTH"
            | UXTW -> "ExtendType_UXTW"
            | UXTX -> "ExtendType_UXTX"
            | SXTB -> "ExtendType_SXTB"
            | SXTH -> "ExtendType_SXTH"
            | SXTW -> "ExtendType_SXTW"
            | SXTX -> "ExtendType_SXTX"
          in
          let shift = match ko with None -> 0 | Some k -> k in
          Some
            ( base ^ fname,
              stmt
                [
                  "d" ^= reg rd;
                  "n" ^= reg rn;
                  "m" ^= reg rm;
                  "datasize" ^= liti datasize;
                  "extend_type" ^= var extend_type;
                  "shift" ^= liti shift;
                ] )
      | I_MOPL (sop, rd, rn, rm, ra) ->
          let fname =
            let open MOPLExt in
            match sop with
            | Signed, ADD -> "SMADDL_64WA_dp_3src.opn"
            | Signed, SUB -> "SMSUBL_64WA_dp_3src.opn"
            | Unsigned, ADD -> "UMADDL_64WA_dp_3src.opn"
            | Unsigned, SUB -> "UMSUBL_64WA_dp_3src.opn"
          in
          Some
            ( "integer/arithmetic/mul/widening/32-64/" ^ fname,
              stmt
                [ "d" ^= reg rd; "n" ^= reg rn; "m" ^= reg rm; "a" ^= reg ra; ]
            )
      | I_MOP (op,v,rd,rn,rm,ra) ->
           let fname =
            let open MOPExt in
            match op with
            | ADD -> "MADD_32A_dp_3src.opn"
            | SUB -> "MSUB_32A_dp_3src.opn"
          in
          Some
            ( "integer/arithmetic/mul/uniform/add-sub/" ^ fname,
              stmt
                ["destsize" ^= variant v;
                 "d" ^= reg rd; "n" ^= reg rn;
                 "m" ^= reg rm; "a" ^= reg ra; ])
      | I_OP3
          ( v,
            (( ADD | ADDS | SUB | SUBS | AND | ANDS | BIC | BICS | EOR | EON
             | ORN | ORR ) as op),
            rd,
            rn,
            OpExt.Reg (rm, s) ) ->
          let base =
            match op with
            | ADD | ADDS | SUB | SUBS ->
                "integer/arithmetic/add-sub/shiftedreg/"
            | AND | ANDS | BIC | BICS | EOR | EON | ORN | ORR ->
                "integer/logical/shiftedreg/"
            | _ -> assert false
          and fname =
            match op with
            | ADD -> "ADD_32_addsub_shift.opn"
            | ADDS -> "ADDS_32_addsub_shift.opn"
            | SUB -> "SUB_32_addsub_shift.opn"
            | SUBS -> "SUBS_32_addsub_shift.opn"
            | AND -> "AND_32_log_shift.opn"
            | ANDS -> "ANDS_32_log_shift.opn"
            | BIC -> "BIC_32_log_shift.opn"
            | BICS -> "BICS_32_log_shift.opn"
            | EOR -> "EOR_32_log_shift.opn"
            | EON -> "EON_32_log_shift.opn"
            | ORR -> "ORR_32_log_shift.opn"
            | ORN -> "ORN_32_log_shift.opn"
            | _ -> assert false
          in
          Some
            ( base ^ fname,
              stmt
                [
                  "d" ^= reg rd;
                  "n" ^= reg rn;
                  "m" ^= reg rm;
                  "datasize" ^= variant v;
                  "shift_type" ^= var (opext_decode_shift s);
                  "shift_amount" ^= liti (opext_shift_amount s);
                ] )
      | I_OP3 (v, ((ADD | ADDS | SUB | SUBS) as op), rd, rn, OpExt.Imm (k, s))
        ->
          let datasize = variant_raw v in
          let k = k lsl s in
          let fname =
            match op with
            | ADD -> "ADD_32_addsub_imm.opn"
            | ADDS -> "ADDS_32S_addsub_imm.opn"
            | SUB -> "SUB_32_addsub_imm.opn"
            | SUBS -> "SUBS_32S_addsub_imm.opn"
            | _ -> assert false
          in
          Some
            ( "integer/arithmetic/add-sub/immediate/" ^ fname,
              stmt
                [
                  "d" ^= reg rd;
                  "n" ^= reg rn;
                  "imm" ^= litbv datasize k;
                  "datasize" ^= liti datasize;
                ] )
      | I_OP3 (v, ((AND | ANDS | EOR | ORR) as op), rd, rn, OpExt.Imm (k, 0)) ->
          let datasize = variant_raw v in
          let fname =
            match op with
            | AND -> "AND_32_log_imm.opn"
            | ANDS -> "ANDS_32S_log_imm.opn"
            | EOR -> "EOR_32_log_imm.opn"
            | ORR -> "ORR_32_log_imm.opn"
            | _ -> assert false
          in
          Some
            ( "integer/logical/immediate/" ^ fname,
              stmt
                [
                  "d" ^= reg rd;
                  "n" ^= reg rn;
                  "imm" ^= litbv datasize k;
                  "datasize" ^= liti datasize;
                ] )
      | I_OP3
          (v,(ASR|LSL|LSR|ROR as op),
           rd,rn,OpExt.Reg (rm, s))
           when OpExt.is_no_shift s
        ->
         let shift_type,fname =
           match op with
           | ASR ->  "ShiftType_ASR","ASRV_32_dp_2src.opn"
           | LSL ->  "ShiftType_LSL","LSLV_32_dp_2src.opn"
           | LSR ->  "ShiftType_LSR","LSRV_32_dp_2src.opn"
           | ROR ->  "ShiftType_ROR","RORV_32_dp_2src.opn"
           | _ -> assert false in
         Some
           ("integer/shift/variable/" ^ fname,
             stmt [
               "d" ^= reg rd;
               "n" ^= reg rn;
               "m" ^= reg rm;
               "datasize" ^= variant v;
               "shift_type" ^= var shift_type;])
        | ( I_STR (v, rt, rn, MemExt.Reg (_vm, rm, e, s))
        | I_LDR (v, rt, rn, MemExt.Reg (_vm, rm, e, s)) ) as i ->
          let fname =
            match i with
            | I_STR _ -> "STR_32_ldst_regoff.opn"
            | I_LDR _ -> "LDR_32_ldst_regoff.opn"
            | _ -> assert false
          and extend_type = memext_decode_ext e in
          Some
            ( "memory/single/general/register/" ^ fname,
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "m" ^= reg rm;
                  "extend_type" ^= var extend_type;
                  "shift" ^= liti s;
                  "datasize" ^= variant v;
                  "regsize" ^= variant v;
                ] )
      | I_LDRSW (rt, rn, MemExt.Reg (_vm, rm, e, s)) ->
          let extend_type = memext_decode_ext e in
          Some
            ( "memory/single/general/register/LDRSW_64_ldst_regoff.opn",
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "m" ^= reg rm;
                  "extend_type" ^= var extend_type;
                  "shift" ^= liti s;
                ] )
      | I_STR (v, rt, rn, MemExt.Imm (k, idx))
      | I_LDR (v, rt, rn, MemExt.Imm (k, idx)) ->
          let memop, fname =
            match ii.A.inst with
            | I_STR _ -> ("MemOp_STORE", "STR_32_ldst_immpost.opn")
            | I_LDR _ -> ("MemOp_LOAD", "LDR_32_ldst_immpost.opn")
            | _ -> assert false
          in
          let wback, postindex =
            match idx with
            | Idx -> (false, false)
            | PreIdx -> (true, false)
            | PostIdx -> (true, true)
          in
          Some
            ( "memory/single/general/immediate/signed/post-idx/" ^ fname,
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "offset" ^= litbv 64 k;
                  "wback" ^= litb wback;
                  "postindex" ^= litb postindex;
                  "signed" ^= litb false;
                  "nontemporal" ^= litb false;
                  "memop" ^= var memop;
                  "tagchecked" ^= litb (wback || rn <> SP);
                  "datasize" ^= variant v;
                  "regsize" ^= variant v;
                  "rt_unknown" ^= litb false;
                  "wb_unknown" ^= litb false;
                ] )
      | I_LDRSW (rt, rn, MemExt.Imm (k, idx)) ->
          let wback, postindex =
            match idx with
            | Idx -> (false, false)
            | PreIdx -> (true, false)
            | PostIdx -> (true, true)
          in
          Some
            ( "memory/single/general/immediate/signed/post-idx/LDRSW_64_ldst_immpost.opn",
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "offset" ^= litbv 64 k;
                  "wback" ^= litb wback;
                  "postindex" ^= litb postindex;
                  "tagchecked" ^= litb (wback || rn <> SP);
                  "wb_unknown" ^= litb false;
                ] )
      | I_STLR (v, rt, rn) ->
          Some
            ( "memory/ordered/STLR_SL32_ldstord.opn",
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "wback" ^= litb false;
                  "rt_unknown" ^= litb false;
                  "tagchecked" ^= litb (rn <> SP);
                  "offset" ^= liti 0;
                  "datasize" ^= variant v;
                ] )
      | I_LDAR (v, AA, rt, rn) ->
          Some
            ( "memory/ordered/LDAR_LR32_ldstord.opn",
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "tagchecked" ^= litb (rn <> SP);
                  "regsize" ^= variant v;
                  "elsize" ^= variant v;
                ] )
      | I_LDAR (v, ((XX | AX) as a), rt, rn) ->
          let fname =
            match a with
            | XX -> "LDXR_LR32_ldstexclr.opn"
            | AX -> "LDAXR_LR32_ldstexclr.opn"
            | _ -> assert false
          in
          Some
            ( "memory/exclusive/single/" ^ fname,
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "tagchecked" ^= litb (rn <> SP);
                  "regsize" ^= variant v;
                  "elsize" ^= variant v;
                ] )
      | I_LDAR (v, AQ, rt, rn) ->
          Some
            ( "memory/ordered-rcpc/LDAPR_32L_memop.opn",
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "wback" ^= litb false;
                  "offset" ^= liti 0;
                  "wb_unknown" ^= litb false;
                  "tagchecked" ^= litb (rn <> SP);
                  "regsize" ^= variant v;
                  "elsize" ^= variant v;
                  "datasize" ^= variant v;
                ] )
      | I_STXR (v, t, rs, rt, rn) ->
          let fname =
            match t with
            | YY -> "STXR_SR32_ldstexclr.opn"
            | LY -> "STLXR_SR32_ldstexclr.opn"
          in
          Some
            ( "memory/exclusive/single/" ^ fname,
              stmt
                [
                  "n" ^= reg rn;
                  "t" ^= reg rt;
                  "s" ^= reg rs;
                  "elsize" ^= variant v;
                  "tagchecked" ^= litb (rn <> SP);
                  "rt_unknown" ^= litb false;
                  "rn_unknown" ^= litb false;
                ] )
      | I_FENCE ISB -> Some ("system/barriers/isb/ISB_BI_barriers.opn", stmt [])
      | I_FENCE (DMB (dom, btyp)) ->
          Some
            ( "system/barriers/dmb/DMB_BO_barriers.opn",
              stmt
                [
                  "domain" ^= var (barrier_domain dom);
                  "types" ^= var (barrier_typ btyp);
                ] )
      | I_FENCE (DSB (dom, btyp)) ->
          Some
            ( "system/barriers/dsb/DSB_BO_barriers.opn",
              stmt
                [
                  "nXS" ^= litb false;
                  "alias" ^= var "DSBAlias_DSB";
                  "domain" ^= var (barrier_domain dom);
                  "types" ^= var (barrier_typ btyp);
                ] )
      | I_UDF k when C.variant Variant.ASL_AArch64_UDF ->
          Some ("udf/UDF_only_perm_undef.opn", stmt [ "imm16" ^= litbv 16 k ])
      | i ->
          let () =
            if _dbg then
              Printf.eprintf
                "Unsupported now: %s\nFalling back on regular semantics.\n"
                (A.pp_instruction PPMode.Ascii i)
          in
          None

    let tr_cst tr =
      Constant.map tr Misc.identity
        (fun _ -> Warn.fatal "Cannot translate instruction")

    let aarch64_to_asl_bv = function
      | V.Var v -> ASLS.A.V.Var v
      | V.Val cst ->
          ASLS.A.V.Val (tr_cst ASLScalar.as_bv cst)

    let aarch64_to_asl = function
      | V.Var v -> ASLS.A.V.Var v
      | V.Val cst -> ASLS.A.V.Val (tr_cst Misc.identity cst)

    let asl_to_aarch64 = function
      | ASLS.A.V.Var v -> V.Var v
      | ASLS.A.V.Val cst -> V.Val (tr_cst Misc.identity cst)

    let is_experimental = TopConf.C.variant Variant.ASLExperimental
    let is_vmsa = TopConf.C.variant Variant.VMSA
    let is_cutoff = TopConf.C.variant Variant.CutOff

    let fake_test ii fname decode =
      let init = [] in
      let prog =
        let version =
          if TopConf.C.variant (Variant.ASLVersion `ASLv0) then `ASLv0
          else if TopConf.C.variant (Variant.ASLVersion `ASLv1) then `ASLv1
          else `Any
        in
        let () =
          if _dbg then
            Format.eprintf "Trying with ASL parser for version %a.@."
              Asllib.PP.pp_version version
        in
        let main =
          let execute =
            Filename.concat "asl-pseudocode/aarch64/instrs" fname
            |> TopConf.C.libfind
            |> ASLBase.build_ast_from_file ~ast_type:`Opn version
          in
          let open Asllib.AST in
          let open Asllib.ASTUtils in
          match execute with
          | [ ({ desc = D_Func ({ body = SB_ASL s; _ } as f); _ } as d) ] ->
              let s = stmt_from_list [ decode; s; return_0 ] in
              let s =
                if is_vmsa then  catch_silent_exit s else s in
              D_Func { f with body = SB_ASL s } |> add_pos_from_st d
          | _ -> assert false
        in
        let () =
          if _dbg then
            Format.eprintf "@[<v>Executing main:@ %a@]@." Asllib.PP.pp_t
              [ main ]
        in
        [ ((ii.A.proc, None, MiscParser.Main), [ ASLBase.Instruction [ main ] ]) ]
      in
      let t =
        {
          MiscParser.init;
          prog;
          info = [];
          filter = None;
          condition = ConstrGen.ExistsState (ConstrGen.And []);
          locations = [];
          extra_data = MiscParser.empty_extra;
        }
      in
      let name =
        Name.{ name = "ASL (fake)"; file = ""; texname = ""; doc = "" }
      in
      let test = ASLTH.build name t in
      let init =
        let global_loc name =
          ASLS.A.Location_reg
            (ii.A.proc, ASLBase.(ASLLocalId (Scope.Global true, name)))
        in
        let st =
          List.fold_left
            (fun st reg ->
              match A.look_reg reg ii.A.env.A.regs with
              | Some v ->
                  ASLS.A.state_add st
                    (ASLS.A.Location_reg (ii.A.proc, ASLBase.ArchReg reg))
                    (aarch64_to_asl v)
              | _ -> st)
            ASLS.A.state_empty ASLBase.gregs
        in
        let nzcv = AArch64Base.NZCV
        and _nzcv =
          global_loc (if is_experimental then "_NZCV" else "PSTATE") in
        let st =
          match A.look_reg nzcv ii.A.env.A.regs with
          | Some v ->
              let v = aarch64_to_asl_bv v in
              ASLS.A.state_add st _nzcv v
          | _ -> st
        in
        let regq = AArch64Base.ResAddr and resaddr = global_loc "RESADDR" in
        let v =
          match A.look_reg regq ii.A.env.A.regs with
          | Some v -> Some (aarch64_to_asl v)
          | None -> None
        in
        match v with Some v -> ASLS.A.state_add st resaddr v | None -> st
      in
      let test = { test with Test_herd.init_state = init } in
      let () =
        if _dbg then
          Printf.eprintf "Building fake test with initial state:\n\t%s\n"
            (ASLS.A.dump_state test.Test_herd.init_state)
      in
      test

    module Translator : sig
      val tr_execution :
        AArch64.inst_instance_id -> asl_exec -> (proc * branch) M.t
    end = struct
      module IMap = Map.Make (Int)

      let tr_v v = asl_to_aarch64 v

      let tr_loc ii loc =
        let nloc =
          match loc with
          | ASLS.A.Location_global x -> Some (A.Location_global (tr_v x))
          | ASLS.A.Location_reg (_proc, ASLBase.ArchReg reg) ->
              Some (A.Location_reg (ii.A.proc, reg))
          | ASLS.A.Location_reg (_proc, ASLBase.ASLLocalId _) -> None
        in
        let () =
          if _dbg then
            Printf.eprintf "tr_loc %s ->%s\n%!" (ASLS.A.pp_location loc)
              (match nloc with
              | None -> ""
              | Some loc -> " " ^ A.pp_location loc)
        in
        nloc

      let tr_op op acc v1 v2 = (M.VC.Binop (op, tr_v v1, tr_v v2), acc)

      let tr_arch_op1 op acc v =
        (M.VC.Unop (Op.ArchOp1 (AArch64Op.Extra op), tr_v v), acc)

      let tr_op1 =
        let open Op in
        function
        | ArchOp1 ASLOp.OA ->
          fun acc v ->
            (M.VC.Unop (Op.ArchOp1 (AArch64Op.OA), tr_v v), acc)
        | ArchOp1 op -> tr_arch_op1 op
        | op ->
            let new_op =
              match op with
              | Not -> Not
              | SetBit i -> SetBit i
              | UnSetBit i -> UnSetBit i
              | ReadBit i -> ReadBit i
              | LeftShift i -> LeftShift i
              | LogicalRightShift i -> LogicalRightShift i
              | ArithRightShift i -> ArithRightShift i
              | AddK i -> AddK i
              | AndK i -> AndK i
              | Inv -> Inv
              | Abs -> Abs
              | Mask sz -> Mask sz
              | Sxt sz -> Sxt sz
              | Rbit sz -> Rbit sz
              | RevBytes (csz, sz) -> RevBytes (csz, sz)
              | TagLoc -> TagLoc
              | CapaTagLoc -> CapaTagLoc
              | TagExtract -> TagExtract
              | LocExtract -> LocExtract
              | UnSetXBits (nbBits, from) -> UnSetXBits (nbBits, from)
              | CapaGetTag -> CapaGetTag
              | CheckSealed -> CheckSealed
              | CapaStrip -> CapaStrip
              | TLBLoc -> TLBLoc
              | PTELoc -> PTELoc
              | Offset -> Offset
              | IsVirtual -> IsVirtual
              | IsInstr -> IsInstr
              | Promote -> Promote
              | Demote -> Demote
              | ArchOp1 _ -> assert false
            in
            fun acc v -> (M.VC.Unop (new_op, tr_v v), acc)

      let tr_action is_bcc e ii act =
        let () =
          if  _dbg then
            Printf.eprintf "tr_action %s\n%!"
              (ASLS.Act.pp_action act) in
        match act with
        | ASLS.Act.Access (dir, loc, v, sz, (a, exp, acc)) -> (
            match tr_loc ii loc with
            | None -> None
            | Some loc ->
               Some (Act.Access (dir, loc, tr_v v, a, exp, sz, acc)))
        | ASLS.Act.Fault (_,loc,d,t) ->
            Option.bind
              (tr_loc ii loc)
              (fun loc ->
                 (Act.Fault (ii,Some loc,d,AArch64Annot.N,false,Some t,None))
                 |> Misc.some)
        | ASLS.Act.Barrier b -> Some (Act.Barrier b)
        | ASLS.Act.Branching txt ->
           let ct = if is_bcc e then Act.Bcc else Act.Pred in
           Some (Act.Commit (ct,txt))
        | ASLS.Act.NoAction ->
           (* As long as aarch64.cat ignores "NoAction" effects *)
           None
        | ASLS.Act.CutOff msg -> Some (Act.CutOff msg)

      let tr_expr acc = function
        | ASLVC.Atom a -> (M.VC.Atom (tr_v a), acc)
        | ASLVC.ReadInit _ -> assert false
        | ASLVC.Unop (op, v) -> tr_op1 op acc v
        | ASLVC.Binop (op, v1, v2) -> tr_op op acc v1 v2
        | ASLVC.Terop (op, a1, a2, a3) ->
            (M.VC.Terop (op, tr_v a1, tr_v a2, tr_v a3), acc)

      let tr_cnstrnt acc = function
        | ASLVC.Warn s -> M.VC.Warn s :: acc
        | ASLVC.Failed e -> M.VC.Failed e :: acc
        | ASLVC.Assign (la, ex) ->
            let expr, acc = tr_expr acc ex in
            M.VC.Assign (tr_v la, expr) :: acc

      let tr_cnstrnts cs = List.fold_left tr_cnstrnt [] cs

      let event_to_monad ii is_bcc is_data event =
        let { ASLE.action; ASLE.iiid; _ } = event in
        let () =
          if _dbg then
            Printf.eprintf "%s:%s%s" (ASLE.pp_eiid event)
              (ASLE.Act.pp_action action)
              (if is_data event then "(data)" else "")
        in
        match (iiid, tr_action is_bcc event ii action) with
        | ASLE.IdInit, _ | _, None ->
            let () = if _dbg then Printf.eprintf ", " in
            None
        | _, Some action' ->
            let () =
              if _dbg then Printf.eprintf "(=%s), " (Act.pp_action action')
            in
            let m =
              M.mk_singleton_es action' ii
              |> (if is_data event then M.as_data_port else Fun.id)
              |> M.force_once
            in
            Some (event, m)

      let rel_to_monad event_to_monad_map comb rel =
        let one_pair (e1, e2) =
          let () =
            if _dbg then
              Printf.eprintf "%s->%s, " (ASLE.pp_eiid e1) (ASLE.pp_eiid e2)
          in
          match
            ( EMap.find_opt e1 event_to_monad_map,
              EMap.find_opt e2 event_to_monad_map )
          with
          | Some m1, Some m2 -> comb m1 (fun () -> m2)
          | _ -> M.unitT ()
        in
        let monads = Seq.map one_pair (ASLE.EventRel.to_seq rel) in
        Seq.fold_left ( ||| ) (return ()) monads


      let tr_execution ii (conc, cs, set_pp, vbpp) =
        let get_cat_show get x =
          match StringMap.find_opt x set_pp with
          | Some e -> get e
          | None -> get ESet.empty in
        let () = if _dbg then Printf.eprintf "Translating event structure:\n" in
        let () =
          if _dbg then (
            Printf.eprintf "\t-all events:\n";
            ESet.iter
              (fun e ->
                Printf.eprintf "\t\t- %s:%s\n" (ASLE.pp_eiid e)
                  (ASLE.Act.pp_action e.ASLE.action))
              conc.ASLS.str.ASLE.events)
        in
        let events = get_cat_show ESet.to_seq "AArch64" in
        let is_data =
          let data_set =
            get_cat_show Misc.identity "AArch64_DATA" in
          fun e -> ESet.mem e data_set in
        let is_bcc =
          let bcc = get_cat_show Misc.identity "AArch64_BCC" in
          fun e -> ASLE.EventSet.mem e bcc in
        let () = if _dbg then Printf.eprintf "\t- events: " in
        let event_list = List.of_seq events in
        let event_to_monad_map =
          Seq.filter_map (event_to_monad ii is_bcc is_data) events
          |> EMap.of_seq
        in
        let events_m =
          let folder _e1 m1 acc = m1 ||| acc in
          EMap.fold folder event_to_monad_map (return ())
        in
        let () = if _dbg then Printf.eprintf "\n" in
        let translate_maybe_rel comb name =
          match List.assoc_opt name vbpp with
          | Some rel ->
              let () = if _dbg then Printf.eprintf "\t- %s: " name in
              let res = rel_to_monad event_to_monad_map comb rel in
              let () = if _dbg then Printf.eprintf "\n" in
              res
          | None -> return ()
        in
        let iico_data = translate_maybe_rel M.( >>= ) aarch64_iico_data in
        let iico_ctrl = translate_maybe_rel M.( >>*= ) aarch64_iico_ctrl in
        let iico_order = translate_maybe_rel M.bind_order aarch64_iico_order in
        let branch =
          let one_event bds event =
            match event.ASLE.action with
            | ASLS.Act.Access
              (Dir.W, ASLS.A.Location_reg (_, ASLBase.ArchReg reg), v, _, _)
              ->
               let v = tr_v v in
               let () =
                 if _dbg then
                   Printf.eprintf "Recording %s <- %s\n%!"
                     (AArch64Base.pp_reg reg) (A.V.pp_v v) in
               (reg,v)::bds
            |  ASLS.Act.Access (Dir.W, loc , v, _, _)
               ->
                if _dbg then
                  Printf.eprintf
                    "Not recorded in B.Next: %s <- %s\n"
                    (ASLS.A.pp_location loc) (ASLS.A.V.pp_v v) ;
                bds
            | _ -> bds in
          let bds = List.fold_left one_event [] event_list in
          let finals = get_cat_show  Misc.identity "AArch64Finals" in
          let pc =
            let n_pc = (* Count writes to PC *)
              List.fold_left
                (fun c (r,_) ->
                  match r with
                  | AArch64Base.PC -> c+1
                  | _ -> c)
                0 bds in
            (* Branching instructions all generate one, initial,
             * PC assignement and a second PC assignement
             * that gives the branch target. This applies even
             * for  non-taken conditional branches where
             * the second assignment is to the next instruction.
             * This second write event is the final write to PC.
             * Non-branching instructions neither read nor
             * write the PC, cf. case [None] below.
             *)
            if n_pc <= 1 then None
            else
              ESet.fold
                (fun e r ->
                  match e.ASLE.action with
                  | ASLS.Act.Access
                    (Dir.W,
                     ASLS.A.Location_reg
                       (_,
                        ASLBase.ArchReg AArch64Base.PC), v, _, _) ->
                     Some (tr_v v)
                  | _ -> r)
                finals None in
          match Misc.seq_opt A.V.as_int pc with
          | Some v -> B.Jump (B.Addr v,bds)
          | None ->
              let is_fault =  List.exists ASLE.is_fault event_list in
              if is_fault then B.Fault bds else B.Next bds
        in
        let () =
          if _dbg then
            match branch with
            | B.Next bds ->
                let pp =
                  List.map
                    (fun (r, v) ->
                      Printf.sprintf "(%s,%s)" (AArch64Base.pp_reg r)
                        (AArch64.V.pp_v v))
                    bds
                in
                let pp = String.concat "; " pp in
                Printf.eprintf "Next [%s]\n%!" pp
            | _ -> ()
        in
        let constraints =
          let () =
            if _dbg then
              Printf.eprintf "\t- constraints:\n%s\n" (ASLVC.pp_cnstrnts cs)
          in
          M.restrict (tr_cnstrnts cs)
        in
        let () = if _dbg then Printf.eprintf "\n" in
        let* () =
          events_m ||| iico_data ||| iico_ctrl
          ||| iico_order ||| constraints
        in
        M.addT (A.next_po_index ii.A.program_order_index) (return branch)
    end

    let check_event_structure model =
      let module MemConfig = struct
        include ASLConf.C

        let model = model
        let bell_model_info = None
        let debug = ASLConf.C.debug.Debug_herd.barrier
        let debug_files = ASLConf.C.debug.Debug_herd.files
        let showsome = true
        let skipchecks = StringSet.empty
        let strictskip = true
        let through = Model.ThroughAll
        let cycles = StringSet.empty
        let dirty = TopConf.dirty
      end in
      let module ASL64M = MemCat.Make (MemConfig) (ASLS) in
      ASL64M.check_event_structure

    let build_model_from_file fname =
      let module P = ParseModel.Make (struct
        include LexUtils.Default

        let libfind = ASLConf.libfind
      end) in
      let fname,m = P.find_parse fname in
      Model.Generic (fname,m)

    let is_strict = C.variant Variant.Strict
    let is_warn = C.variant Variant.Warn && not is_strict

    let check_strict test ii =
      if is_strict then
        Warn.fatal "No ASL implemention for instruction %s"
          (A.dump_instruction ii.A.inst);
      if is_warn then
        Warn.warn_always "No ASL implemention for instruction %s"
          (A.dump_instruction ii.A.inst);
      AArch64Mixed.build_semantics test ii

    let asl_build_semantics test ii =
      let flitmus = test.Test_herd.name.Name.file in
      let () =
        if _dbg then
          Printf.eprintf "\n\nExecuting %s by proc %s\n%!"
            (A.pp_instruction PPMode.Ascii ii.A.inst)
            (Proc.pp ii.A.proc)
      in
      match decode_inst ii with
      | None -> check_strict test ii
      | Some _ when AArch64.is_mixed -> check_strict test ii
      | Some (fname, args) -> (
          let test = fake_test ii fname args in
          let model = build_model_from_file "asl.cat" in
          let check_event_structure = check_event_structure model in
          let { MC.event_structures = rfms; _ }, test =
            MC.glommed_event_structures test
          in
          let () =
            if _dbg then
              Printf.eprintf "Got rfms back: %d of them.\n%!" (List.length rfms)
          in
          let monads,_ =
            let solve_regs (_i, cs, es) =
              let () =
                if  _dbg then begin
                  Printf.eprintf "** Events **\n" ;
                  ASLE.EventSet.iter
                    (fun e ->
                       Printf.eprintf "  %a\n"
                         ASLE.debug_event e)
                    es.ASLE.events
                end in
              MC.solve_regs test es cs in

            let check_rfm li (es, rfm, cs) =
              let po = MU.po_iico es in
              let pos =
                let mem_evts = ASLE.mem_of es.ASLE.events in
                ASLE.EventRel.of_pred mem_evts mem_evts (fun e1 e2 ->
                    ASLE.same_location e1 e2 && ASLE.EventRel.mem (e1, e2) po)
              in
              let partial_po =
                ASLE.EventTransRel.to_implicitely_transitive_rel
                  es.ASLE.partial_po
              in

              let conc =
                {
                  ASLS.conc_zero with
                  ASLS.str = es;
                  ASLS.rfmap = rfm;
                  ASLS.po;
                  ASLS.partial_po;
                  ASLS.pos;
                }
              in
              let kfail li =
                let () =
                  if _dbg then prerr_endline "ASL cat, fail" in
                li in
              let ksuccess conc _fs (out_sets, out_show) _flags li =
                let () =
                  if _dbg then  prerr_endline "ASL cat, success" in
                let c =
                  conc, cs, Lazy.force out_sets, Lazy.force out_show in
                Translator.tr_execution ii c::li
              in
              check_event_structure test conc kfail ksuccess li
            in
            let check (li,seen)  (_,_,cs as c) =
              let msg =
                if is_cutoff then
                  (* Keep all executions, included pruned ones. *)
                  None
                else ASLS.find_cutoff cs.ASLS.E.events in
              let seen  =
                match msg with
                | Some msg ->
                    if not (StringSet.mem msg seen) then begin
                      Warn.warn_always
                        "%a: %s, some legal outcomes may be missing"
                        Pos.pp_pos0 flitmus
                        msg;
                      StringSet.add msg seen
                    end else seen
                | None -> seen in
              if Misc.is_some msg then li,seen
              else
                match solve_regs c with
                | None -> li,seen
                | Some c -> check_rfm li c,seen in
            List.fold_left check ([],StringSet.empty) rfms
          in
          let () =
            if _dbg then
              Printf.eprintf "Got %d complete executions.\n%!"
                (List.length monads)
          in
          let () =
            if _dbg then
              Printf.eprintf "End of ASL execution for %s.\n\n%!"
                (A.pp_instruction PPMode.Ascii ii.A.inst)
          in
          match monads with
          | [] -> Warn.fatal "No possible ASL execution."
          | h :: t -> List.fold_left M.altT h t)

    let build_semantics test ii =
      let open AArch64Base in
      match ii.A.inst with
      | I_OP3 (V64,LSR,_,_,OpExt.Imm (12,0)) (* Specific -> get TLBI key *)
      | I_OP3 (V64,SUBS,ZR,_,
               OpExt.(Imm (0,0)|Reg(_,LSL 0))) (* Register or zero comparison *)
      | I_DC _|I_IC _ | I_TLBI _ ->
          AArch64Mixed.build_semantics test ii
      | _ -> asl_build_semantics test ii

    let spurious_setaf v = AArch64Mixed.spurious_setaf v
  end
end
