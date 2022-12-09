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

    module ASLValue = Int64Value.Make (ASLBase.Instr)
    module ASLS = ASLSem.Make (ASLConf) (ASLValue)
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
      let dirty = TopConf.dirty
    end

    module MC = Mem.Make (MCConf) (ASLS)

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

    let tr_cond =
      let open AArch64Base in
      function NE -> 0 | EQ -> 1 | GE -> 2 | GT -> 3 | LE -> 4 | LT -> 5

    let pseudocode_fname = Filename.concat "asl-pseudocode"

    let decode_inst ii =
      let r2i = ASLBase.arch_reg_to_int in
      let open AArch64Base in
      match ii.A.inst with
      | I_NOP -> Some (pseudocode_fname "nop.asl", [])
      | I_OP3 (_ty, ADD, rd, rn, RV (_, rm), _os) ->
          Some
            ( "asl-pseudocode/add.asl",
              [ ("d", r2i rd); ("n", r2i rn); ("m", r2i rm) ] )
      | I_SWP (_v, _rmw, r1, r2, r3) ->
          Some
            ( pseudocode_fname "swp.asl",
              [ ("s", r2i r1); ("t", r2i r2); ("n", r2i r3) ] )
      | I_CAS (_v, _rmw, rs, rt, rn) ->
          Some
            ( pseudocode_fname "cas.asl",
              [ ("s", r2i rs); ("t", r2i rt); ("n", r2i rn) ] )
      | I_CSEL (_, rd, rn, rm, c, _) ->
          Some
            ( pseudocode_fname "csel.asl",
              [
                ("d", r2i rd); ("n", r2i rn); ("m", r2i rm); ("cond", tr_cond c);
              ] )
      | I_MOV (_, rt, RV (_, rs)) ->
          Some (pseudocode_fname "mov.asl", [ ("s", r2i rs); ("t", r2i rt) ])
      | i ->
          let () =
            if _dbg then
              Printf.eprintf
                "Unsupported now: %s\nFalling back on regular semantics.\n"
                (A.pp_instruction PPMode.Ascii i)
          in
          None

    let fake_test ii fname args =
      let proc = 0 in
      let init_args =
        let scope = ("main", 0) in
        let one_arg (x, i) =
          let r = ASLBase.ASLLocalId (scope, x) in
          let loc = MiscParser.Location_reg (proc, ASLBase.pp_reg r) in
          (loc, (TestType.TyDef, ParsedConstant.intToV i))
        in
        List.map one_arg args
      in
      let init_env =
        let one_reg (r, _s) =
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
        List.filter_map one_reg AArch64Base.xregs
      in
      let init = init_args @ init_env in
      let fname = ASLConf.libfind fname in
      let prog =
        let ast = ASLBase.build_ast_from_file fname in
        [ ((0, None, MiscParser.Main), [ ASLBase.Instruction ast ]) ]
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
      | ASLValue.Var s -> (
          match IMap.find_opt s !csym_tbl with
          | Some v -> v
          | None ->
              let v = V.fresh_var () in
              csym_tbl := IMap.add s v !csym_tbl;
              v)
      | ASLValue.Val (Constant.Concrete i) ->
          V.intToV (ASLValue.Cst.Scalar.to_int i)
      | ASLValue.Val (Constant.Symbolic symb) -> V.Val (Constant.Symbolic symb)
      | v ->
          Warn.fatal "AArch64.ASL does not know how to translate: %s"
            (ASLValue.pp_v v)

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

    let tr_op1 =
      let open Op in
      function
      | ArchOp1 _ -> assert false
      | Not -> Not
      | SetBit i -> SetBit i
      | UnSetBit i -> UnSetBit i
      | ReadBit i -> ReadBit i
      | LeftShift i -> LeftShift i
      | LogicalRightShift i -> LogicalRightShift i
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

    let tr_expr = function
      | ASLVC.Atom a -> M.VC.Atom (tr_v a)
      | ASLVC.ReadInit _ -> assert false
      | ASLVC.Unop (op, v) -> M.VC.Unop (tr_op1 op, tr_v v)
      | ASLVC.Binop (op, a1, a2) -> M.VC.Binop (op, tr_v a1, tr_v a2)
      | ASLVC.Terop (op, a1, a2, a3) ->
          M.VC.Terop (op, tr_v a1, tr_v a2, tr_v a3)

    let tr_cnstrnt = function
      | ASLVC.Warn s -> M.VC.Warn s
      | ASLVC.Failed e -> M.VC.Failed e
      | ASLVC.Assign (la, ex) -> M.VC.Assign (tr_v la, tr_expr ex)

    let tr_cnstrnts = List.map tr_cnstrnt

    let event_to_monad ii event =
      let { ASLE.action; _ } = event in
      let () =
        if _dbg then
          Printf.eprintf "%s:%s" (ASLE.pp_eiid event) (ASLAct.pp_action action)
      in
      match tr_action ii action with
      | Some action' ->
          let () =
            if _dbg then Printf.eprintf "(=%s), " (Act.pp_action action')
          in
          Some (event, M.force_once (M.mk_singleton_es action' ii))
      | None -> None

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
        let cs =
          match cs with
          | Some cs -> cs
          | None -> Warn.fatal "Execution error in AArch64/ASL translation"
        in
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
      | Some (fname, args) -> (
          let test = fake_test ii fname args in
          let model = build_model_from_file "asl.cat" in
          let check_event_structure = check_event_structure model in
          let ( { MC.event_structures = rfms; MC.overwritable_labels = owls },
                test ) =
            MC.glommed_event_structures test
          in
          let kfail li = li in
          let ksuccess (conc : ASLS.concrete) _fsc
              ((set_pp, vbpp) : ASLS.set_pp Lazy.t * ASLS.rel_pp Lazy.t) _flags
              (cs, li) =
            (None, (conc, cs, Lazy.force set_pp, Lazy.force vbpp) :: li)
          in
          let call_model conc cs (_, li) =
            check_event_structure test conc kfail ksuccess (Some cs, li)
          in
          let _, conc_and_pp =
            List.fold_left
              (fun res (_i, cs, es) ->
                MC.calculate_rf_with_cnstrnts test owls es cs call_model res)
              (None, []) rfms
          in
          let monads = List.map (tr_execution ii) conc_and_pp in
          match monads with
          | [] -> Warn.fatal "No possible ASL execution."
          | h :: t -> List.fold_left M.altT h t)

    let spurious_setaf _ = assert false
  end
end
