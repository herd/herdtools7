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

module Make (TopConf : AArch64Sig.Config) (V : Value.AArch64) = struct
  module AArch64S = AArch64Sem.Make (TopConf) (V)
  include AArch64S

  let ( ||| ) = M.( ||| )
  let ( >>! ) = M.( >>! )
  let ( let* ) = M.( >>= )
  let return = M.unitT
  let _dbg = TopConf.C.debug.Debug_herd.monad

  module Mixed (SZ : ByteSize.S) = struct
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
    end

    module ASLS = ASLSem.Make (ASLConf)
    module ASLV = ASLS.V
    module ASLA = ASLS.A
    module ASLAct = ASLS.Act
    module ASLE = ASLS.E
    module EMap = ASLE.EventMap
    module ESet = ASLE.EventSet
    module ERel = ASLE.EventRel
    module ASLVC = ASLS.M.VC
    module ASLTH = Test_herd.Make (ASLA)
    module IMap = Map.Make (Int)

    module MCConf = struct
      include ASLConf

      let byte = SZ.byte
      let cache_type = TopConf.cache_type
      let dirty = TopConf.dirty
      let initwrites = false
    end

    module MC = Mem.Make (MCConf) (ASLS)
    module MU = MemUtils.Make (ASLS)

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
        let cache_type = TopConf.cache_type
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
      let ( ^= ) x e = S_Assign (LE_Var x |> with_pos, e) |> with_pos in
      let lit i = E_Literal (V_Int i) |> with_pos in
      let litb b = E_Literal (V_Bool b) |> with_pos in
      let var x = E_Var x |> with_pos in
      let reg r = ASLBase.arch_reg_to_int r |> lit in
      let variant v = AArch64Base.tr_variant v |> MachSize.nbits |> lit in
      let cond c = tr_cond c |> lit in
      let stmt = Asllib.ASTUtils.stmt_from_list in
      let open AArch64Base in
      match ii.A.inst with
      | I_NOP ->
          Some
            ( "system/hints.opn",
              stmt [ "op" ^= var "SystemHintOp_NOP" ] )
      | I_SWP (v, RMW_P, Ireg r1, Ireg r2, Ireg r3) ->
          Some
            ( "memory/atomicops/swp.opn",
              stmt
                [
                  "s" ^= reg r1;
                  "t" ^= reg r2;
                  "n" ^= reg r3;
                  "datasize" ^= variant v;
                  "regsize" ^= lit 64;
                  "acquire" ^= litb false;
                  "release" ^= litb false;
                  "tagchecked" ^= litb true;
                ] )
      | I_CAS (v, RMW_P, Ireg rs, Ireg rt, Ireg rn) ->
          Some
            ( "memory/atomicops/cas/single.opn",
              stmt
                [
                  "s" ^= reg rs;
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "datasize" ^= variant v;
                  "regsize" ^= lit 64;
                  "acquire" ^= litb false;
                  "release" ^= litb false;
                  "tagchecked" ^= litb true;
                ] )
      | I_CSEL (v, Ireg rd, Ireg rn, Ireg rm, c, Cpy) ->
          Some
            ( "integer/conditional/select.opn",
              stmt
                [
                  "d" ^= reg rd;
                  "n" ^= reg rn;
                  "m" ^= reg rm;
                  "datasize" ^= variant v;
                  "condition" ^= cond c;
                  "else_inv" ^= litb false;
                  "else_inc" ^= litb false;
                ] )
      | I_MOV (v, Ireg rt, RV (v', Ireg rs)) when v = v' ->
          Some
            ( "integer/logical/shiftedreg.opn",
              stmt
                [
                  "n" ^= lit 31;
                  "m" ^= reg rs;
                  "d" ^= reg rt;
                  "datasize" ^= variant v;
                  "shift_type" ^= var "ShiftType_LSR";
                  "shift_amount" ^= lit 0;
                  "invert" ^= litb false;
                  "op" ^= var "LogicalOp_ORR";
                  "setflags" ^= litb false;
                ] )
      | I_LDR (v, Ireg rt, Ireg rn, K 0, S_NOEXT) ->
          Some
            ( "memory/single/general/register.opn",
              stmt
                [
                  "t" ^= reg rt;
                  "n" ^= reg rn;
                  "m" ^= lit 31 (* i.e. Zero *);
                  "wback" ^= litb false;
                  "postindex" ^= litb false;
                  "extend_type" ^= var "ExtendType_UXTX";
                  (* ie LSL . *)
                  "shift" ^= lit 0;
                  "signed" ^= litb false;
                  "nontemporal" ^= litb false;
                  "privileged" ^= litb false;
                  "memop" ^= var "MemOp_LOAD";
                  "tagchecked" ^= litb true;
                  "datasize" ^= variant v;
                  "regsize" ^= lit 64;
                ] )
      | I_STR (v, Ireg rt, Ireg rn, K 0, S_NOEXT) ->
          Some
            ( "memory/single/general/register.opn",
              stmt
                [
                  "wback" ^= litb false;
                  "postindex" ^= litb false;
                  "scale" ^= lit 0b11;
                  "extend_type" ^= var "ExtendType_UXTX";
                  "shift" ^= lit 0;
                  "n" ^= reg rn;
                  "t" ^= reg rt;
                  "m" ^= lit 31 (* i.e. Zero *);
                  "nontemporal" ^= litb false;
                  "privileged" ^= litb false;
                  "memop" ^= var "MemOp_STORE";
                  "signed" ^= litb false;
                  "regsize" ^= lit 64;
                  "datasize" ^= variant v;
                  "tagchecked" ^= litb true;
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

    let fake_test ii fname decode =
      let proc = 0 in
      let init =
        let one_reg r =
          let loc = MiscParser.Location_reg (proc, AArch64Base.pp_reg r) in
          match A.look_reg r ii.A.env.A.regs with
          | None -> None
          | Some (V.Var _csym) -> None
          | Some (V.Val (Constant.Concrete c)) ->
              Some
                ( loc,
                  (TestType.TyDef, Constant.Concrete (V.Cst.Scalar.pp false c))
                )
          | Some (V.Val (Constant.Symbolic s)) ->
              Some (loc, (TestType.TyDef, Constant.Symbolic s))
          | _ -> None
        in
        List.filter_map one_reg ASLBase.arch_regs
      in
      let prog =
        let version =
          if C.variant (Variant.ASLVersion `ASLv0) then `ASLv0
          else if C.variant (Variant.ASLVersion `ASLv1) then `ASLv1
          else `Any
        in
        let () =
          if _dbg then
            Format.eprintf "Trying with ASL parser for version %a.@."
              Asllib.PP.pp_version version
        in
        let build ?ast_type version fname =
          Filename.concat "asl-pseudocode" fname
          |> ASLConf.libfind
          |> ASLBase.build_ast_from_file ?ast_type version
        in
        let main =
          let fname = Filename.concat "aarch64/instrs" fname in
          let execute = build ~ast_type:`Opn version fname in
          let open Asllib.AST in
          match execute with
          | [ D_Func f ] ->
              let body = Asllib.ASTUtils.s_then decode f.body in
              D_Func { f with body }
          | _ -> assert false
        in
        let patches = build `ASLv1 "patches.asl"
        and custom_implems = build `ASLv1 "implementations.asl"
        and shared = build `ASLv0 "shared_pseudocode.asl" in
        let ast =
          Asllib.ASTUtils.patch
            ~patches:(List.rev_append custom_implems patches)
            ~src:shared
        in
        [ ((0, None, MiscParser.Main), [ ASLBase.Instruction (main :: ast) ]) ]
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
            (ASLA.dump_state test.Test_herd.init_state)
      in
      test

    let csym_tbl = ref IMap.empty

    let tr_v = function
      | ASLV.Var s -> (
          match IMap.find_opt s !csym_tbl with
          | Some v -> v
          | None ->
              let v = V.fresh_var () in
              csym_tbl := IMap.add s v !csym_tbl;
              v)
      | ASLV.Val (Constant.Concrete i) ->
          V.Val
            (Constant.Concrete (V.Cst.Scalar.of_int64 (ASLScalar.to_int64 i)))
      | ASLV.Val (Constant.Symbolic symb) -> V.Val (Constant.Symbolic symb)
      | v ->
          Warn.fatal "AArch64.ASL does not know how to translate: %s"
            (ASLV.pp_v v)

    let tr_loc ii = function
      | ASLA.Location_global x -> Some (A.Location_global (tr_v x))
      | ASLA.Location_reg (_proc, ASLBase.ArchReg reg) ->
          Some (A.Location_reg (ii.A.proc, reg))
      | _ -> None

    let tr_action ii =
      let an = AArch64.N in
      let exp = AArch64.Exp in
      function
      | ASLAct.Access (dir, loc, v, sz) -> (
          match tr_loc ii loc with
          | None -> None
          | Some loc ->
              let ac = Act.access_of_location_std loc in
              Some (Act.Access (dir, loc, tr_v v, an, exp, sz, ac)))
      | ASLAct.NoAction -> Some Act.NoAction
      | ASLAct.TooFar msg -> Some (Act.TooFar msg)

    let tr_op =
      let open Op in
      function
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

    let tr_arch_op1 =
      let open ASLValue.ASLArchOp in
      let atom v = M.VC.Atom v in
      let declare e acc =
        let name = V.fresh_var () in
        (name, M.VC.Assign (name, e) :: acc)
      in
      fun op acc v ->
        let v = tr_v v in
        match op with
        | ToInt -> (atom v, acc)
        | ToBool -> (M.VC.Binop (Op.Ne, V.zero, v), acc)
        | BVSlice positions -> (
            let extract_bit_to dst_pos src_pos acc =
              let bit = M.VC.Unop (Op.ReadBit src_pos, v) in
              if dst_pos = 0 then (bit, acc)
              else
                let nbit, acc = declare bit acc in
                (M.VC.Unop (Op.LeftShift dst_pos, nbit), acc)
            in
            let folder (prec, acc, i) pos =
              let w, acc = extract_bit_to i pos acc in
              let nw, acc = declare w acc in
              let nprec, acc = declare prec acc in
              (M.VC.Binop (Op.Or, nw, nprec), acc, i + 1)
            in
            match positions with
            | [] -> (V.zero |> atom, acc)
            | [ x ] -> extract_bit_to 0 x acc
            | h :: t ->
                let first, acc = extract_bit_to 0 h acc in
                let w, acc, _ = List.fold_left folder (first, acc, 1) t in
                (w, acc))
        | _ ->
            Warn.fatal "Not yet implemented: translation of vector operations."

    let tr_op1 =
      let open Op in
      function
      | ArchOp1 op -> tr_arch_op1 op
      | op ->
          let new_op =
            match op with
            | ArchOp1 _ -> assert false
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
          in
          fun acc v -> (M.VC.Unop (new_op, tr_v v), acc)

    let tr_expr acc = function
      | ASLVC.Atom a -> (M.VC.Atom (tr_v a), acc)
      | ASLVC.ReadInit _ -> assert false
      | ASLVC.Unop (op, v) -> tr_op1 op acc v
      | ASLVC.Binop (op, a1, a2) ->
          (M.VC.Binop (tr_op op, tr_v a1, tr_v a2), acc)
      | ASLVC.Terop (op, a1, a2, a3) ->
          (M.VC.Terop (op, tr_v a1, tr_v a2, tr_v a3), acc)

    let tr_cnstrnt acc = function
      | ASLVC.Warn s -> M.VC.Warn s :: acc
      | ASLVC.Failed e -> M.VC.Failed e :: acc
      | ASLVC.Assign (la, ex) ->
          let expr, acc = tr_expr acc ex in
          M.VC.Assign (tr_v la, expr) :: acc

    let tr_cnstrnts cs = List.fold_left tr_cnstrnt [] cs

    let event_to_monad ii event =
      let { ASLE.action; ASLE.iiid; _ } = event in
      let () =
        if _dbg then
          Printf.eprintf "%s:%s" (ASLE.pp_eiid event) (ASLAct.pp_action action)
      in
      match (iiid, tr_action ii action) with
      | ASLE.IdInit, _ | _, None ->
          let () = if _dbg then Printf.eprintf ", " in
          None
      | _, Some action' ->
          let () =
            if _dbg then Printf.eprintf "(=%s), " (Act.pp_action action')
          in
          Some (event, M.force_once (M.mk_singleton_es action' ii))

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
      let monads = Seq.map one_pair (ERel.to_seq rel) in
      Seq.fold_left ( ||| ) (return ()) monads

    let tr_execution ii (_conc, cs, set_pp, vbpp) =
      let () = if _dbg then Printf.eprintf "Translating event structure:\n" in
      let constraints =
        let () =
          if _dbg then
            Printf.eprintf "\t- constraints: %s\n" (ASLVC.pp_cnstrnts cs)
        in
        M.restrict (tr_cnstrnts cs)
      in
      let events =
        match StringMap.find_opt "AArch64" set_pp with
        | None -> Seq.empty
        | Some s -> ESet.to_seq s
      in
      let () = if _dbg then Printf.eprintf "\t- events: " in
      let event_list = List.of_seq events in
      let event_to_monad_map =
        List.to_seq event_list
        |> Seq.filter_map (event_to_monad ii)
        |> EMap.of_seq
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
      let iico_order = translate_maybe_rel M.( >>*= ) aarch64_iico_order in
      let () = if _dbg then Printf.eprintf "\n" in
      let branch =
        let one_event acc event =
          match (acc, event.ASLE.action) with
          | ( B.Next li,
              ASLAct.Access
                (Dir.W, ASLA.Location_reg (_, ASLBase.ArchReg reg), v, _) ) ->
              B.Next ((reg, tr_v v) :: li)
          | _ -> acc
        in
        List.fold_left one_event (B.Next []) event_list
      in
      let* () = iico_data ||| iico_ctrl ||| iico_order ||| constraints in
      M.addT (A.next_po_index ii.A.program_order_index) (return branch)

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
          let monads = List.map (tr_execution ii) conc_and_pp in
          match monads with
          | [] -> Warn.fatal "No possible ASL execution."
          | h :: t -> List.fold_left M.altT h t)

    let spurious_setaf _ = assert false
  end
end
