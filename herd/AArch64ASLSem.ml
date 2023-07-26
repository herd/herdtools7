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
(* Authors:                                                                 *)
(* Hadrien Renaud, University College London, UK.                           *)
(****************************************************************************)

let aarch64_iico_ctrl = "aarch64_iico_ctrl"
let aarch64_iico_data = "aarch64_iico_data"
let aarch64_iico_order = "aarch64_iico_order"

let return_0 =
  let open Asllib.AST in
  let open Asllib.ASTUtils in
  S_Return (Some (expr_of_int 0)) |> add_dummy_pos

module Make (TopConf : AArch64Sig.Config) (V : Value.AArch64) :
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

    module ASLS = ASLSem.Make (ASLConf)
    module ASLE = ASLS.E
    module EMap = ASLE.EventMap
    module ESet = ASLE.EventSet
    module ASLVC = ASLS.M.VC
    module ASLTH = Test_herd.Make (ASLS.A)

    module MCConf = struct
      include ASLConf

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

    let decode_inst ii =
      let open Asllib.AST in
      let with_pos desc = Asllib.ASTUtils.add_dummy_pos desc in
      let ( ^= ) x e =
        S_Decl (LDK_Let, LDI_Var (x, None), Some e) |> with_pos
      in
      let lit v = E_Literal v |> with_pos in
      let liti i = lit (V_Int (Z.of_int i)) in
      let litb b = lit (V_Bool b) in
      let litbv v i = lit (V_BitVector (Asllib.Bitvector.of_int_sized v i)) in
      let var x = E_Var x |> with_pos in
      let variant_raw v = AArch64Base.tr_variant v |> MachSize.nbits in
      let variant v = variant_raw v |> liti in
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
      let logical_op = function
        | AND | ANDS | BIC | BICS -> var "LogicalOp_AND"
        | ORR | ORN -> var "LogicalOp_ORR"
        | EOR -> var "LogicalOp_EOR"
        | _ -> assert false
      in
      let invert = function
        | AND | ANDS | ORR | EOR -> litb false
        | BIC | BICS | ORN -> litb true
        | _ -> assert false
      in
      let setflags = function
        | ANDS | BICS | ADDS | SUBS -> litb true
        | AND | BIC | EOR | ORR | ORN | ADD | SUB | LSL | ASR | LSR ->
            litb false
      in
      match ii.A.inst with
      | I_NOP ->
          Some ("system/hints.opn", stmt [ "op" ^= var "SystemHintOp_NOP" ])
      | I_SWP (v, RMW_P, r1, r2, r3) ->
          Some
            ( "memory/atomicops/swp.opn",
              stmt
                [
                  "s" ^= reg r1;
                  "t" ^= reg r2;
                  "n" ^= reg r3;
                  "datasize" ^= variant v;
                  "regsize" ^= liti 64;
                  "acquire" ^= litb false;
                  "release" ^= litb false;
                  "tagchecked" ^= litb true;
                ] )
      | I_CAS (v, RMW_P, rs, rt, rn) ->
          Some
            ( "memory/atomicops/cas/single.opn",
              stmt
                [
                  "s" ^= reg rs;
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "datasize" ^= variant v;
                  "regsize" ^= liti 64;
                  "acquire" ^= litb false;
                  "release" ^= litb false;
                  "tagchecked" ^= litb true;
                ] )
      | I_CSEL (v, rd, rn, rm, c, opsel) ->
          let else_inv =
            match opsel with Cpy | Inc -> false | Inv | Neg -> true
          in
          let else_inc =
            match opsel with Inc | Neg -> true | Cpy | Inv -> false
          in
          Some
            ( "integer/conditional/select.opn",
              stmt
                [
                  "d" ^= reg rd;
                  "n" ^= reg rn;
                  "m" ^= reg rm;
                  "datasize" ^= variant v;
                  "condition" ^= cond c;
                  "else_inv" ^= litb else_inv;
                  "else_inc" ^= litb else_inc;
                ] )
      | I_MOV (v, rd, RV (v', (Ireg _ as rm))) when v = v' ->
          Some
            ( "integer/logical/shiftedreg.opn",
              stmt
                [
                  "n" ^= liti 31;
                  "m" ^= reg rm;
                  "d" ^= reg rd;
                  "datasize" ^= variant v;
                  "shift_type" ^= var "ShiftType_LSR";
                  "shift_amount" ^= liti 0;
                  "invert" ^= litb false;
                  "op" ^= var "LogicalOp_ORR";
                  "setflags" ^= litb false;
                ] )
      | I_MOV (v, rd, RV (v', SP)) when v = v' ->
          let datasize = variant_raw v in
          Some
            ( "integer/arithmetic/add-sub/immediate.opn",
              stmt
                [
                  "d" ^= reg rd;
                  "n" ^= liti 31;
                  "imm" ^= litbv datasize 0;
                  "datasize" ^= liti datasize;
                  "sub_op" ^= litb false;
                  "setflags" ^= litb false;
                ] )
      | I_MOV (v, rd, K k) ->
          let datasize = variant_raw v in
          Some
            ( "integer/logical/immediate.opn",
              stmt
                [
                  "d" ^= reg rd;
                  "n" ^= liti 31;
                  "imm" ^= litbv datasize k;
                  "datasize" ^= liti datasize;
                  "op" ^= logical_op EOR;
                  "setflags" ^= setflags EOR;
                ] )
      | I_OP3 (v, op, rd, rn, RV (v', rm), S_NOEXT) when v = v' -> (
          match op with
          | AND | ANDS | BIC | BICS | EOR | ORN | ORR ->
              Some
                ( "integer/logical/shiftedreg.opn",
                  stmt
                    [
                      "d" ^= reg rd;
                      "n" ^= reg rn;
                      "m" ^= reg rm;
                      "datasize" ^= variant v;
                      "shift_type" ^= var "ShiftType_LSR";
                      "shift_amount" ^= liti 0;
                      "invert" ^= invert op;
                      "op" ^= logical_op op;
                      "setflags" ^= setflags op;
                    ] )
          | ADD | ADDS | SUB | SUBS | ASR | LSL | LSR -> None)
      | I_OP3 (v, op, rd, rn, K k, S_NOEXT) -> (
          let datasize = variant_raw v in
          match op with
          | AND | ANDS | BIC | BICS | EOR | ORN | ORR ->
              (* BIC, BICS, ORN do not make sense here but .. *)
              Some
                ( "integer/logical/immediate.opn",
                  stmt
                    [
                      "d" ^= reg rd;
                      "n" ^= reg rn;
                      "imm" ^= litbv datasize k;
                      "datasize" ^= liti datasize;
                      "op" ^= logical_op op;
                      "setflags" ^= setflags op;
                    ] )
          | ADD | ADDS | SUB | SUBS ->
              let subop = match op with SUB | SUBS -> true | _ -> false in
              Some
                ( "integer/arithmetic/add-sub/immediate.opn",
                  stmt
                    [
                      "d" ^= reg rd;
                      "n" ^= reg rn;
                      "imm" ^= litbv datasize k;
                      "datasize" ^= liti datasize;
                      "sub_op" ^= litb subop;
                      "setflags" ^= setflags op;
                    ] )
          | ASR | LSL | LSR -> None)
      | I_STR (v, rt, rn, RV (v', rm), barrel_shift)
      | I_LDR (v, rt, rn, RV (v', rm), barrel_shift) ->
          let memop =
            match ii.A.inst with
            | I_STR _ -> "MemOp_STORE"
            | I_LDR _ -> "MemOp_LOAD"
            | _ -> assert false
          in
          let extend_type =
            match barrel_shift with
            | S_NOEXT -> "ExtendType_UXTX"
            | S_SXTW -> "ExtendType_SXTW"
            | S_UXTW -> "ExtendType_UXTW"
            | S_LSL _ -> "ExtendType_UXTX"
            | _ ->
                Warn.fatal "Unsupported barrel shif† for LDR: %s."
                  (AArch64Base.pp_barrel_shift "" barrel_shift string_of_int)
          in
          let shift =
            match barrel_shift with
            | S_LSL k -> k
            | S_NOEXT -> 0
            | _ -> AArch64Base.tr_variant v |> MachSize.nbytes
          in
          Some
            ( "memory/single/general/register.opn",
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "m" ^= reg rm;
                  "wback" ^= litb false;
                  "postindex" ^= litb false;
                  "extend_type" ^= var extend_type;
                  "shift" ^= liti shift;
                  "signed" ^= litb false;
                  "nontemporal" ^= litb false;
                  "privileged" ^= litb false;
                  "memop" ^= var memop;
                  "tagchecked" ^= litb true;
                  "datasize" ^= variant v;
                  "regsize" ^= variant v';
                  "rt_unknown" ^= litb false;
                  "wb_unknown" ^= litb false;
                ] )
      | I_STR (v, rt, rn, K k, barrel_shift)
      | I_LDR (v, rt, rn, K k, barrel_shift) ->
          let memop =
            match ii.A.inst with
            | I_STR _ -> "MemOp_STORE"
            | I_LDR _ -> "MemOp_LOAD"
            | _ -> assert false
          in
          let offset =
            match barrel_shift with
            | S_LSL n | S_MSL n -> k lsl n
            | S_LSR n -> k lsr n
            | S_ASR n -> k asr n
            | S_NOEXT | S_SXTW | S_UXTW -> k
          in
          Some
            ( "memory/single/general/immediate/signed/post-idx.opn",
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "offset" ^= litbv 64 offset;
                  "wback" ^= litb false;
                  "postindex" ^= litb false;
                  "signed" ^= litb false;
                  "nontemporal" ^= litb false;
                  "privileged" ^= litb false;
                  "memop" ^= var memop;
                  "tagchecked" ^= litb true;
                  "datasize" ^= variant v;
                  "regsize" ^= variant v;
                  "rt_unknown" ^= litb false;
                  "wb_unknown" ^= litb false;
                ] )
      | i ->
          let () =
            if _dbg then
              Printf.eprintf
                "Unsupported now: %s\nFalling back on regular semantics.\n"
                (A.pp_instruction PPMode.Ascii i)
          in
          None

    let symb_values_tbl : (string, V.v) Hashtbl.t = Hashtbl.create 17
    let symb_values_tag = "AArch64-HashedValue"

    let fake_test ii fname decode =
      let proc = 0 in
      let init =
        let loc r = MiscParser.Location_reg (proc, AArch64Base.pp_reg r) in
        let set r c =
          let c' =
            let open Constant in
            match c with
            | Concrete s -> Concrete (V.Cst.Scalar.pp false s)
            | Symbolic s -> Symbolic s
            | _ ->
                let name = V.Cst.pp_v c and tag = Some symb_values_tag in
                let () = Hashtbl.add symb_values_tbl name (V.Val c) in
                Symbolic (Virtual { offset = 0; cap = 0L; tag; name })
          in
          (loc r, (TestType.TyDef, c'))
        in
        let init_gregs =
          let one_reg r =
            match A.look_reg r ii.A.env.A.regs with
            | Some (V.Val c) -> Some (set r c)
            | Some (V.Var _) | None -> None
          in
          List.filter_map one_reg ASLBase.gregs
        in
        let nzcv = AArch64Base.NZCV in
        match A.look_reg nzcv ii.A.env.A.regs with
        | Some (V.Val c) -> set nzcv c :: init_gregs
        | Some (V.Var _) -> init_gregs
        | None ->
            (loc nzcv, (TestType.TyDef, Constant.Concrete "0")) :: init_gregs
      in
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
              D_Func { f with body = SB_ASL s } |> add_pos_from_st d
          | _ -> assert false
        in
        let () =
          if _dbg then
            Format.eprintf "@[<v>Executing main:@ %a@]@." Asllib.PP.pp_t
              [ main ]
        in
        [ ((0, None, MiscParser.Main), [ ASLBase.Instruction [ main ] ]) ]
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

      let csym_tbl = ref IMap.empty
      let atom v = M.VC.Atom v

      let declare e acc =
        let name = V.fresh_var () in
        (name, M.VC.Assign (name, e) :: acc)

      let tr_v = function
        | ASLValue.V.Var s | ASLValue.V.Val (Constant.Frozen s) -> (
            match IMap.find_opt s !csym_tbl with
            | Some v -> v
            | None ->
                let v = V.fresh_var () in
                csym_tbl := IMap.add s v !csym_tbl;
                v)
        | ASLValue.V.Val (Constant.Concrete i) ->
            V.Val
              (Constant.Concrete (V.Cst.Scalar.of_int64 (ASLScalar.to_int64 i)))
        | ASLValue.V.Val (Constant.Symbolic symb) -> (
            match symb with
            | Constant.(Virtual { tag = Some tag; name; _ })
              when tag == symb_values_tag ->
                Hashtbl.find symb_values_tbl name
            | _ -> V.Val (Constant.Symbolic symb))
        | v ->
            Warn.fatal "AArch64.ASL does not know how to translate: %s"
              (ASLValue.V.pp_v v)

      let tr_loc ii = function
        | ASLS.A.Location_global x -> Some (A.Location_global (tr_v x))
        | ASLS.A.Location_reg (_proc, ASLBase.ArchReg reg) ->
            Some (A.Location_reg (ii.A.proc, reg))
        | ASLS.A.Location_reg (_proc, ASLBase.ASLLocalId _) -> None

      let mask_of_positions =
        let mask_one acc i =
          let open Int64 in
          shift_left 1L i |> lognot |> logor acc
        in
        List.fold_left mask_one (-1L)

      let group_by_continuous =
        let rec aux prec start length acc = function
          | [] -> (start, length) :: acc
          | h :: t ->
              if prec = h + 1 then aux h start (length + 1) acc t
              else aux h h 1 ((start, length) :: acc) t
        in
        function [] -> [] | h :: t -> aux h h 1 [] t

      let extract_bits_to dst_pos src src_pos n acc =
        if n >= 64 then Warn.fatal "Can't handle values with more than 64 bits."
        else
          let shifted_v, acc =
            let shift = src_pos - dst_pos in
            if shift = 0 then (src, acc)
            else
              let dir =
                if shift > 0 then Op.LogicalRightShift shift
                else Op.LeftShift ~-shift
              in
              declare (M.VC.Unop (dir, src)) acc
          in
          let mask = ((1 lsl n) - 1) lsl (dst_pos - n) in
          (M.VC.Unop (Op.AndK (string_of_int mask), shifted_v), acc)

      let tr_arch_op arch_op acc (v1 : ASLValue.V.v) (v2 : ASLValue.V.v) =
        match arch_op with
        | ASLValue.SetIndex _ | ASLValue.SetField _ ->
            Warn.fatal "Cannot translate vector operations to AArch64."
        | ASLValue.Concat -> (
            match (v1, v2) with
            | _, ASLValue.V.Val (Constant.Concrete (ASLScalar.S_BitVector bv2))
              -> (
                match Asllib.Bitvector.length bv2 with
                | 0 -> (atom (tr_v v1), acc)
                | n ->
                    let shifted, acc =
                      declare (M.VC.Unop (Op.LeftShift n, tr_v v1)) acc
                    in
                    (M.VC.Binop (Op.Or, shifted, tr_v v2), acc))
            | ASLValue.V.Val (Constant.Concrete (ASLScalar.S_BitVector bv1)), _
              when Asllib.Bitvector.is_zeros bv1 ->
                (atom (tr_v v2), acc)
            | _, ASLValue.V.Var _ ->
                Warn.fatal
                  "Not yet implemented: concatenating variables: %s and %s."
                  (ASLValue.V.pp_v v1) (ASLValue.V.pp_v v2)
            | _ ->
                Warn.fatal "Cannot translate concatenation of %s and %s."
                  (ASLValue.V.pp_v v1) (ASLValue.V.pp_v v2))
        | ASLValue.BVSliceSet positions -> (
            let dst = tr_v v1 and src = tr_v v2 in
            match group_by_continuous positions with
            | [] -> (atom dst, acc)
            | [ (63, 64) ] -> (atom src, acc)
            | (x, n) :: t ->
                let folder (prec, acc, i) (x, n) =
                  let w, acc = extract_bits_to x src i n acc in
                  let nw, acc = declare w acc in
                  let nprec, acc = declare prec acc in
                  (M.VC.Binop (Op.Or, nw, nprec), acc, i + n)
                in
                let first, acc = extract_bits_to x src 0 n acc in
                let w, acc, _ = List.fold_left folder (first, acc, n) t in
                let nw, acc = declare w acc in
                let mask =
                  mask_of_positions positions |> Int64.to_string |> V.stringToV
                in
                let masked_dst, acc =
                  declare (M.VC.Binop (Op.And, mask, dst)) acc
                in
                (M.VC.Binop (Op.Or, nw, masked_dst), acc))

      let tr_op =
        let open Op in
        function
        | ArchOp archop -> tr_arch_op archop
        | op ->
            let op =
              match op with
              | Add -> Add
              | Sub -> Sub
              | Mul -> Mul
              | Div -> Div
              | And -> And
              | Or -> Or
              | Xor -> Xor
              | Nor -> Nor
              | AndNot2 -> AndNot2
              | ASR -> ASR
              | CapaAdd -> CapaAdd
              | Alignd -> Alignd
              | Alignu -> Alignu
              | Build -> Build
              | ClrPerm -> ClrPerm
              | CpyType -> CpyType
              | CSeal -> CSeal
              | Cthi -> Cthi
              | Seal -> Seal
              | SetValue -> SetValue
              | CapaSub -> CapaSub
              | CapaSubs -> CapaSubs
              | CapaSetTag -> CapaSetTag
              | Unseal -> Unseal
              | ShiftLeft -> ShiftLeft
              | ShiftRight -> ShiftRight
              | Lsr -> Lsr
              | Lt -> Lt
              | Gt -> Gt
              | Eq -> Eq
              | Ne -> Ne
              | Le -> Le
              | Ge -> Ge
              | Max -> Max
              | Min -> Min
              | SetTag -> SetTag
              | SquashMutable -> SquashMutable
              | CheckPerms s -> CheckPerms s
              | ToInteger -> ToInteger
              | ArchOp _ -> assert false
            in
            fun acc v1 v2 -> (M.VC.Binop (op, tr_v v1, tr_v v2), acc)

      let tr_arch_op1 op acc v =
        let v = tr_v v in
        match op with
        | ASLValue.ToIntS -> (atom v, acc)
        | ASLValue.ToIntU -> (atom v, acc)
        | ASLValue.ToBool -> (M.VC.Binop (Op.Ne, V.zero, v), acc)
        | ASLValue.ToBV -> (atom v, acc)
        | ASLValue.BVSlice positions -> (
            let folder (prec, acc, i) (x, n) =
              let w, acc = extract_bits_to i v x n acc in
              let nw, acc = declare w acc in
              let nprec, acc = declare prec acc in
              (M.VC.Binop (Op.Or, nw, nprec), acc, i + n)
            in
            match group_by_continuous positions with
            | [] -> (atom V.zero, acc)
            | [ (63, 64) ] -> (atom v, acc)
            | [ (31, 32) ] -> (M.VC.Unop (Op.Mask MachSize.Word, v), acc)
            | [ (15, 16) ] -> (M.VC.Unop (Op.Mask MachSize.Short, v), acc)
            | [ (7, 8) ] -> (M.VC.Unop (Op.Mask MachSize.Byte, v), acc)
            | (x, n) :: t ->
                let first, acc =
                  if n = 1 then (M.VC.Unop (Op.ReadBit x, v), acc)
                  else extract_bits_to 0 v x n acc
                in
                let w, acc, _ = List.fold_left folder (first, acc, n) t in
                (w, acc))
        | ASLValue.BoolNot -> (M.VC.Unop (Op.Not, v), acc)
        | ASLValue.GetIndex _ | ASLValue.GetField _ ->
            Warn.fatal "Cannot translate vector operations to AArch64."

      let tr_op1 =
        let open Op in
        function
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
              | Mask sz -> Mask sz
              | Sxt sz -> Sxt sz
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
              | ArchOp1 _ -> assert false
            in
            fun acc v -> (M.VC.Unop (new_op, tr_v v), acc)

      let tr_action ii =
        let an = AArch64.N in
        let exp = AArch64.Exp in
        function
        | ASLS.Act.Access (dir, loc, v, sz) -> (
            match tr_loc ii loc with
            | None -> None
            | Some loc ->
                let ac = Act.access_of_location_std loc in
                Some (Act.Access (dir, loc, tr_v v, an, exp, sz, ac)))
        | ASLS.Act.NoAction -> Some Act.NoAction
        | ASLS.Act.TooFar msg -> Some (Act.TooFar msg)

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

      let tr_cnstrnts cs =
        let prepare (symb_assign, acc) = function
          | ASLVC.Assign (ASLValue.V.Var i, e) -> (IMap.add i e symb_assign, acc)
          | cnstrnt -> (symb_assign, tr_cnstrnt acc cnstrnt)
        in
        let symb_assigns, acc = List.fold_left prepare (IMap.empty, []) cs in
        let tr_one acc i =
          match IMap.find_opt i symb_assigns with
          | Some e ->
              let e', acc = tr_expr acc e in
              M.VC.Assign (tr_v (ASLValue.V.Var i), e') :: acc
          | None -> acc
        in
        let map_diff_key map1 map2 =
          let folder key _val acc =
            if IMap.mem key map2 then acc else key :: acc
          in
          IMap.fold folder map1 []
        in
        let rec loop to_do acc1 =
          let csym_tbl_1 = !csym_tbl in
          let acc2 = List.fold_left tr_one [] to_do in
          let to_do = map_diff_key !csym_tbl csym_tbl_1
          and acc = List.rev_append acc1 acc2 in
          match to_do with [] -> acc | _ -> loop to_do acc
        in
        let to_do = map_diff_key !csym_tbl IMap.empty in
        loop to_do acc

      let event_to_monad ii is_data event =
        let { ASLE.action; ASLE.iiid; _ } = event in
        let () =
          if _dbg then
            Printf.eprintf "%s:%s%s" (ASLE.pp_eiid event)
              (ASLE.Act.pp_action action)
              (if is_data event then "(data)" else "")
        in
        match (iiid, tr_action ii action) with
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
        let events =
          match StringMap.find_opt "AArch64" set_pp with
          | None -> Seq.empty
          | Some s -> ESet.to_seq s
        in
        let is_data =
          let data_set =
            match StringMap.find_opt "AArch64_DATA" set_pp with
            | None -> ESet.empty
            | Some s -> s
          in
          fun e -> ESet.mem e data_set
        in
        let () = if _dbg then Printf.eprintf "\t- events: " in
        let event_list = List.of_seq events in
        let event_to_monad_map =
          List.to_seq event_list
          |> Seq.filter_map (event_to_monad ii is_data)
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
          let one_event acc event =
            match (acc, event.ASLE.action) with
            | ( B.Next li,
                ASLS.Act.Access
                  (Dir.W, ASLS.A.Location_reg (_, ASLBase.ArchReg reg), v, _) )
              ->
                B.Next ((reg, tr_v v) :: li)
            | _ -> acc
          in
          List.fold_left one_event (B.Next []) event_list
        in
        let constraints =
          let () =
            if _dbg then
              Printf.eprintf "\t- constraints: %s\n" (ASLVC.pp_cnstrnts cs)
          in
          M.restrict (tr_cnstrnts cs)
        in
        let () = if _dbg then Printf.eprintf "\n" in
        let* () =
          events_m ||| iico_data ||| iico_ctrl ||| iico_order ||| constraints
        in
        M.addT (A.next_po_index ii.A.program_order_index) (return branch)
    end

    let check_event_structure model =
      let module MemConfig = struct
        include ASLConf

        let model = model
        let bell_model_info = None
        let debug = ASLConf.debug.Debug_herd.barrier
        let debug_files = ASLConf.debug.Debug_herd.files
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
      Model.Generic (P.parse fname)

    let build_semantics test ii =
      let () =
        if _dbg then
          Printf.eprintf "\n\nExecuting %s\n"
            (A.pp_instruction PPMode.Ascii ii.A.inst)
      in
      match decode_inst ii with
      | None -> AArch64Mixed.build_semantics test ii
      | Some _ when AArch64.is_mixed -> AArch64Mixed.build_semantics test ii
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
          let rfms_with_regs =
            let solve_regs (_i, cs, es) = MC.solve_regs test es cs in
            List.filter_map solve_regs rfms
          in
          let () =
            if _dbg then
              Printf.eprintf "With regs solved, still %d rfms.\n%!"
                (List.length rfms_with_regs)
          in
          let conc_and_pp =
            let check_rfm li (es, rfm, cs) =
              let po = MU.po_iico es in
              let pos =
                let mem_evts = ASLE.mem_of es.ASLE.events in
                ASLE.EventRel.of_pred mem_evts mem_evts (fun e1 e2 ->
                    ASLE.same_location e1 e2 && ASLE.EventRel.mem (e1, e2) po)
              in
              let conc =
                {
                  ASLS.conc_zero with
                  ASLS.str = es;
                  ASLS.rfmap = rfm;
                  ASLS.po;
                  ASLS.pos;
                }
              in
              let kfail li = li in
              let ksuccess conc _fs (out_sets, out_show) _flags li =
                (conc, cs, Lazy.force out_sets, Lazy.force out_show) :: li
              in
              check_event_structure test conc kfail ksuccess li
            in
            List.fold_left check_rfm [] rfms_with_regs
          in
          let () =
            if _dbg then
              Printf.eprintf "Got %d complete executions.\n%!"
                (List.length conc_and_pp)
          in
          let monads = List.map (Translator.tr_execution ii) conc_and_pp in
          let () =
            if _dbg then
              Printf.eprintf "End of ASL execution for %s.\n\n%!"
                (A.pp_instruction PPMode.Ascii ii.A.inst)
          in
          match monads with
          | [] -> Warn.fatal "No possible ASL execution."
          | h :: t -> List.fold_left M.altT h t)

    let spurious_setaf _ = assert false
  end
end
