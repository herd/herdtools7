(******************************************************************************)
(*                           the diy toolsuite                                *)
(*                                                                            *)
(* Jade Alglave, University College London, UK.                               *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(*                                                                            *)
(* Copyright 2015-present Institut National de Recherche en Informatique et   *)
(* en Automatique and the authors. All rights reserved.                       *)
(*                                                                            *)
(* This software is governed by the CeCILL-B license under French law and     *)
(* abiding by the rules of distribution of free software. You can use,        *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B   *)
(* license as circulated by CEA, CNRS and INRIA at the following URL          *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.              *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                            *)
(* Jade Alglave, Arm Ltd and UCL, UK.                                       *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

(*
   A quick note on monads:
   -----------------------

   We use three main connecters here:
    - the classic data binder ( [>>=] )
    - a control binder ( [>>*=] or assimilate)
    - a sequencing operator ( [M.para_bind_output_right] )
   And some specialized others:
    - the parallel operator ( [>>|] )
    - a choice operation

   Monad type has:
   - input: EventSet
   - output: EventSet
   - data_input: EventSet
   - ctrl_output: EventSet
   - iico_data: EventRel
   - iico_ctrl: EventRel

   Description of the main data binders:

   - _data_ binders:
     iico_data U= 1.output X 2.data_input
     input = 1.input (or 2.input if 1 is empty)
        same with data_input
     output = 2.ouput (or 1.output if 2.output is None)
        same-ish with ctrl_output

   - _seq_ binder (called [para_bind_output_right]):
     input = 1.input U 2.input
        same with data_input
     output = 2.output
     ctrl_output = 1.ctrl_output U 2.ctrl_output

   - _ctrl_ binder (called [bind_ctrl_seq_data]):
     iico_ctrl U= 1.ctrl_output X 2.input
     input = 1.input (or 2.input if 1 is empty)
        same with data_input
     output = 2.output (or 1.output if 2.output is None)
        same-ish with ctrl_output
 *)

module AST = Asllib.AST
module ASTUtils = Asllib.ASTUtils
open ASLBase

module type Config = sig
  module C : Sem.Config
  val dirty : DirtyBit.t option
  val libfind : string -> string
end

module Make (Conf : Config) = struct
  module V = ASLValue.V

  let variant = Conf.C.variant

  module ConfLoc = struct
    include SemExtra.ConfigToArchConfig (Conf.C)

    let default_to_symb = variant Variant.ASL
  end

  module ASL64AH = struct
    include ASLExtra.Make (ASLBase) (ConfLoc) (V)
    include ASLBase

    let opt_env = true
  end

  module Act = ASLAction.Make (Conf.C.PC) (ASL64AH)
  include SemExtra.Make (Conf.C) (ASL64AH) (Act)

  module TypeCheck = Asllib.Typing.Annotate (struct
    let check =
      let open Asllib.Typing in
      if variant (Variant.ASLType `Warn) then Warn
      else if variant (Variant.ASLType `TypeCheck) then TypeCheckNoWarn
      else Silence

    let output_format = Asllib.Error.HumanReadable
    let print_typed = false
    let use_field_getter_extension = false
    let fine_grained_side_effects = false
    let use_conflicting_side_effects_extension = false
    let override_mode = Asllib.Typing.Permissive
  end)

  module ASLInterpreterConfig = struct
    let unroll =
      match Conf.C.unroll with None -> Opts.unroll_default `ASL | Some u -> u

    let recursive_unroll = function
      | "AArch64_S1Translate" when variant (Variant.ASL_AArch64) -> Some 1
      | _ -> Some (Opts.unroll_default `ASL)

    let error_handling_time = Asllib.Error.Dynamic
    let empty_branching_effects_optimization = false
    let log_nondet_choice = Conf.C.debug.Debug_herd.asl_symb
    let display_call_stack_on_error = Conf.C.debug.Debug_herd.asl_stack
    let track_symbolic_path = true
    let bit_clear_optimisation = true

    module Instr = Asllib.Instrumentation.SemanticsNoInstr
  end

  let is_vmsa = variant Variant.VMSA

  let build ?ast_type fname =
    Filename.concat "asl-pseudocode" fname
    |> Conf.libfind
    |> ASLBase.build_ast_from_file ?ast_type `ASLv1

  let built_shared_pseudocode = lazy (build "shared_pseudocode.asl")
  let barriers = []
  let isync = None
  let atomic_pair_allowed _ _ = true
  module Mixed (SZ : ByteSize.S) : sig
    val build_semantics : test -> A.inst_instance_id -> (proc * branch) M.t
    val spurious_setaf : A.V.v -> unit M.t
  end = struct
    module Mixed = M.Mixed (SZ)

    let ( let* ) = M.asl_data
    let ( and* ) = M.( >>| )
    let return = M.unitT
    let ( >>= ) = M.asl_data

    let aneutral = AArch64Annot.N
    let aatomic = AArch64Annot.X
    and aexp = AArch64Explicit.Exp
    and areg = Access.REG
    and avir = Access.VIR
    and apte = Access.PTE
    let areg_std = (aneutral,aexp,Access.REG)

    (**************************************************************************)
    (* ASL-PO handling                                                        *)
    (**************************************************************************)

    let incr (poi : A.program_order_index ref) : A.program_order_index =
      let i = !poi in
      let () = poi := A.next_po_index i in
      i

    let use_ii_with_poi ii poi =
      let program_order_index = incr poi in
      { ii with A.program_order_index }

    (**************************************************************************)
    (* Values handling                                                        *)
    (**************************************************************************)

    (*
     * Non-resolved values are "frozen" into constants.
     * Useful for storing them into vector of constants.
     * Notice: such "constants" are usable only when
     * extracted from vectors.
     * See `unfreeze` below.
     *)

    let as_constant = function
      | V.Val c -> c
      | V.Var id -> Constant.Frozen id

    let freeze_constant = function
      | V.Val c -> c
      | V.Var id ->  Constant.Frozen id

    let v_unknown_of_type ~eval_expr_sef:(_: Asllib.AST.expr -> V.v M.t) _t =
      return (V.fresh_var ())

    let v_of_literal =
      let open AST in
      let open ASLScalar in
      let concrete v = Constant.Concrete v in
      let tr = function
        | L_Int i -> S_Int i |> concrete
        | L_Bool b -> S_Bool b |> concrete
        | L_BitVector bv -> S_BitVector bv |> concrete
        | L_Real _f ->
            Printf.eprintf "real: %s\n%!" (Q.to_string _f);
            Warn.fatal "Cannot use reals yet."
        | L_String s -> S_String s |> concrete
        | L_Label s -> S_Label s |> concrete
      in
      fun v -> V.Val (tr v)

    let v_to_z = function
      | V.Val (Constant.Concrete (ASLScalar.S_Int i)) -> Some i
      | _ -> None

    let v_to_label = function
      | V.Val (Constant.Concrete (ASLScalar.S_Label l)) -> l
      | v -> Warn.fatal "Cannot make a label out of value %s" (V.pp_v v)

    let v_as_int = function
      | V.Val (Constant.Concrete i) -> V.Cst.Scalar.to_int i
      | v -> Warn.fatal "Cannot concretise symbolic value %s as an int" (V.pp_v v)

    let cst_as_int cst = v_as_int (V.Val cst)
    let cst_as_label cst = v_to_label (V.Val cst)

    let v_as_record = function
      | V.Val (Constant.ConcreteRecord map) -> map
      | v ->
          Warn.fatal "Cannot concretise symbolic value %s as a record"
            (V.pp_v v)

    let cst_as_bool = function
      | Constant.Concrete (ASLScalar.S_Bool b) -> b
      | c ->
          Warn.fatal "Cannot concretise symbolic value %s as a boolean"
            (V.pp_v (V.Val c))

    let datasize_to_machsize v =
      match v_as_int v with
      | 8 -> MachSize.Byte
      | 16 -> MachSize.Short
      | 32 -> MachSize.Word
      | 64 -> MachSize.Quad
      | 128 -> MachSize.S128
      | _ ->
          Warn.fatal
            "Cannot access a register or memory with size %s" (V.pp_v v)

    let access_field v f map =
      try StringMap.find f map
      with Not_found -> Warn.fatal "Record %s has no %s field" (V.pp_v v) f

    let access_bool_field v f map = access_field v f map |> cst_as_bool
    let access_integer_field v f map = access_field v f map |> cst_as_int

    type caller = Write|Read|Fault

    let accdesc_to_annot caller accdesc =
      let open AArch64Annot in
      let map = v_as_record accdesc in
      let is_release = access_bool_field accdesc "relsc" map
      and is_acquiresc = access_bool_field accdesc "acqsc" map
      and is_acquirepc = access_bool_field accdesc "acqpc" map
      and is_atomic = access_bool_field accdesc "atomicop" map
      and is_exclusive = access_bool_field accdesc "exclusive" map in
      let is_read =
        match caller with
        | Write -> false
        | Read -> true
        | Fault ->
            (* Read has priority, as for native VMSA *)
            access_bool_field accdesc "read" map
      in
      let is_ax x n = if is_atomic || is_exclusive then x else n in
      let is_noret =
        if not is_atomic then false
        else
          let rs = access_integer_field accdesc "Rs" map
          and rt = access_integer_field accdesc "Rt" map
          and rs2 = access_integer_field accdesc "Rs2" map
          and rt2 = access_integer_field accdesc "Rt2" map
          and modop = access_field accdesc "modop" map |> cst_as_label in
          let () =
            if false then
              Printf.eprintf
                "Got accdesc with modop=%S, Rs=%d, Rt=%d, Rs2=%d, Rt2=%d.\n%!"
                modop rs rt rs2 rt2
          in
          let destreg, destreg2 =
            match modop with
            | "MemAtomicOp_ADD" | "MemAtomicOp_BIC" | "MemAtomicOp_EOR"
            | "MemAtomicOp_ORR" | "MemAtomicOp_SMAX" | "MemAtomicOp_SMIN"
            | "MemAtomicOp_UMAX" | "MemAtomicOp_UMIN" | "MemAtomicOp_SWP" ->
                (rt, rt2)
            | "MemAtomicOp_CAS" -> (rs, rs2)
            | _ -> assert false
          in
          match (destreg, destreg2) with
          | 31, 31 | 31, -1 | -1, 31 -> true
          | _ -> false
      in
      let an =
        if (not is_read) && is_release then is_ax XL L
        else if is_noret then NoRet
        else if is_read && is_acquiresc then is_ax XA A
        else if is_read && is_acquirepc then is_ax XQ Q
        else is_ax X N
      in
      let () =
        if false && an <> N then
          Printf.eprintf "ASL -> AArch64 Memory annotation %s\n%!"
            (AArch64Annot.pp an)
      in
      an

    let access_to_access acc =
      let open Access in
      match v_to_label acc with
      | "REG" -> REG
      | "VIR" -> VIR
      | "PHY" -> PHY
      | "PTE" -> PTE
      | "TLB" -> TLB
      | "TAG" -> TAG
      | "PHY_PTE" -> PHY_PTE
      | s -> Warn.fatal "Bad access code from ASL: %s" s

    let to_bv sz = M.op1 (Op.ArchOp1 (ASLOp.ToBV (MachSize.nbits sz)))
    let to_int_unsigned = M.op1 (Op.ArchOp1 ASLOp.ToIntU)
    let to_int_signed = M.op1 (Op.ArchOp1 ASLOp.ToIntS)
    let to_aarch64_val = M.op1 (Op.ArchOp1 ASLOp.ToAArch64)
    and from_aarch64_val = M.op1 (Op.ArchOp1 ASLOp.FromAArch64)

    (**************************************************************************)
    (* Special monad interactions                                              *)
    (**************************************************************************)

    let create_barrier b ii = M.mk_singleton_es (Act.Barrier b) ii

    let write_loc sz loc v a e acc ii =
      let mk_action loc' = Act.Access (Dir.W, loc', v, sz, (a, e, acc)) in
      M.write_loc mk_action loc ii

    let read_loc sz loc a e acc ii =
      let mk_action loc' v' = Act.Access (Dir.R, loc', v', sz, (a, e, acc)) in
      M.read_loc Port.No mk_action loc ii >>= to_bv sz

    (**************************************************************************)
    (* ASL-Backend implementation                                             *)
    (**************************************************************************)

    let commit (ii, poi) msg =
      M.mk_singleton_es (Act.Branching msg) (use_ii_with_poi ii poi)

    let choice (m1 : V.v M.t) (m2 : 'b M.t) (m3 : 'b M.t) : 'b M.t =
      M.asl_data m1 @@ function
      | V.Val (Constant.Concrete (ASLScalar.S_Bool b)) -> if b then m2 else m3
      | b -> M.choiceT b m2 m3

    let logor v1 v2 =
      match (v1, v2) with
      | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)), v
        when Asllib.Bitvector.is_zeros bv ->
          return v
      | v, V.Val (Constant.Concrete (ASLScalar.S_BitVector bv))
        when Asllib.Bitvector.is_zeros bv ->
          return v
      | _ -> M.op Op.Or v1 v2

    let boolop herdop shortcut v1 v2 =
      match (v1, v2) with
      | V.Val (Constant.Concrete (ASLScalar.S_Bool b)), v
      | v, V.Val (Constant.Concrete (ASLScalar.S_Bool b)) ->
          return @@ shortcut b v
      | _ -> M.op herdop v1 v2

    let concat v1 v2 =
      match (v1, v2) with
      | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)), v
        when Asllib.Bitvector.length bv = 0 ->
          return v
      | v, V.Val (Constant.Concrete (ASLScalar.S_BitVector bv))
        when Asllib.Bitvector.length bv = 0 ->
          return v
      | _ -> M.op (Op.ArchOp ASLOp.Concat) v1 v2

    let is_valid_trailing_bits z =
      let open Z in
      fits_int z && match to_int z with 1 | 2 | 4 | 8 -> true | _ -> false

    let binop_mod v1 v2 =
      match (v1, v2) with
      | ( (V.Val Constant.(Symbolic _) | V.Var _),
          V.Val (Constant.Concrete (ASLScalar.S_Int z)) )
        when is_valid_trailing_bits z ->
          return V.zero
      | _ -> M.op Op.Rem v1 v2

    let binop =
      let open AST in
      let v_true = V.Val (Constant.Concrete (ASLScalar.S_Bool true))
      and v_false = V.Val (Constant.Concrete (ASLScalar.S_Bool false)) in
      function
      | `AND -> M.op Op.And
      | `BAND -> boolop Op.And (fun b v -> if b then v else v_false)
      | `BEQ -> M.op Op.Eq
      | `BOR -> boolop Op.Or (fun b v -> if b then v_true else v)
      | `DIV -> M.op Op.Div
      | `MOD -> binop_mod
      | `DIVRM -> M.op (Op.ArchOp ASLOp.Divrm)
      | `XOR -> M.op Op.Xor
      | `EQ -> M.op Op.Eq
      | `GT -> M.op Op.Gt
      | `GE -> M.op Op.Ge
      | `LT -> M.op Op.Lt
      | `LE -> M.op Op.Le
      | `SUB -> M.op Op.Sub
      | `MUL -> M.op Op.Mul
      | `NE -> M.op Op.Ne
      | `OR -> logor
      | `ADD -> M.op Op.Add
      | `SHL -> M.op Op.ShiftLeft
      | `SHR -> M.op Op.ShiftRight
      | `BV_CONCAT -> concat
      | `BIC -> M.op Op.AndNot2
      | `STR_CONCAT -> M.op (Op.ArchOp ASLOp.StringConcat)
      | `POW -> M.op (Op.ArchOp ASLOp.Pow)
      | (`IMPL | `RDIV) as op ->
          Warn.fatal "ASL operation %s not yet implement in ASLSem."
            (Asllib.PP.binop_to_string op)

    let unop op =
      let open AST in
      match op with
      | BNOT -> M.op1 (Op.ArchOp1 ASLOp.BoolNot)
      | NEG -> M.op Op.Sub V.zero
      | NOT -> M.op1 Op.Inv

    let ternary = function
      | V.Val (Constant.Concrete (ASLScalar.S_Bool true)) -> fun m1 _ -> m1 ()
      | V.Val (Constant.Concrete (ASLScalar.S_Bool false)) -> fun _ m2 -> m2 ()
      | V.Val (Constant.Concrete _) as v ->
          Warn.fatal "ASL Type error: got %s for a ternary." (V.pp_v v)
      | v ->
          fun m1 m2 ->
            let* v1 = m1 () and* v2 = m2 () in
            M.op3 Op.If v v1 v2

    let reg_of_scope_id x scope =
      match (x, scope) with
      | "RESADDR", Scope.Global false -> ArchReg AArch64Base.ResAddr
      | "_SP_EL0", Scope.Global false -> ArchReg AArch64Base.SP
      | "PSTATE.N", Scope.Global false -> ArchReg AArch64Base.(PState PSTATE.N)
      | "PSTATE.Z", Scope.Global false -> ArchReg AArch64Base.(PState PSTATE.Z)
      | "PSTATE.C", Scope.Global false -> ArchReg AArch64Base.(PState PSTATE.C)
      | "PSTATE.V", Scope.Global false -> ArchReg AArch64Base.(PState PSTATE.V)
      | "_PC", Scope.Global false -> ArchReg AArch64Base.PC
      | _ -> ASLLocalId (scope, x)

    let loc_of_scoped_id ii x scope =
      A.Location_reg (ii.A.proc, reg_of_scope_id x scope)

    (* AArch64 registers hold integers, not bitvectors *)
    let is_aarch64_reg = function
      | A.Location_reg (_, ASLBase.ArchReg _) -> true
      | _ -> false

    let tr_regval = M.op1 (Op.ArchOp1 ASLOp.ToIntU)

    let mk_std_access (ii,poi) dir loc v =
      let action = Act.Access (dir, loc, v, MachSize.Quad, areg_std) in
      M.mk_singleton_es action (use_ii_with_poi ii poi)

    let on_access_identifier dir (ii,_ as ii_poi) x scope v =
      let loc = loc_of_scoped_id ii x scope in
      let m = mk_std_access ii_poi dir loc in
      if is_aarch64_reg loc then tr_regval v >>= m
      else m v

    let on_write_identifier = on_access_identifier Dir.W
    and on_read_identifier = on_access_identifier Dir.R

    let create_vector li =
      let li = List.map freeze_constant li in
      return (V.Val (Constant.ConcreteVector li))

    let create_record li =
      let record =
        List.to_seq li
        |> Seq.map (fun (x, v) -> (x, as_constant v))
        |> StringMap.of_seq
      in
      return (V.Val (Constant.ConcreteRecord record))

    let create_exception = create_record
    let freeze = function V.Val c -> V.Val c | V.Var i -> V.Val (V.freeze i)

    let unfreeze = function
      | V.Val (Constant.Frozen i) -> return (V.Var i)
      | v -> return v

    let get_index i v = M.op1 (Op.ArchOp1 (ASLOp.GetIndex i)) v >>= unfreeze
    let set_index i v vec = M.op (Op.ArchOp (ASLOp.SetIndex i)) vec (freeze v)

    let get_field name v =
      M.op1 (Op.ArchOp1 (ASLOp.GetField name)) v >>= unfreeze

    let set_field name v record =
      M.op (Op.ArchOp (ASLOp.SetField name)) record (freeze v)

    let read_from_bitvector ~loc:_ positions bvs =
      let positions = Asllib.ASTUtils.slices_to_positions v_as_int positions in
      let arch_op1 = ASLOp.BVSlice positions in
      M.op1 (Op.ArchOp1 arch_op1) bvs

    let write_to_bitvector positions w v =
      let positions = Asllib.ASTUtils.slices_to_positions v_as_int positions in
      M.op (Op.ArchOp (ASLOp.BVSliceSet positions)) v w

    let concat_bitvectors bvs =
      let bvs =
        let filter = function
          | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)) ->
              Asllib.Bitvector.length bv > 0
          | _ -> true
        in
        List.filter filter bvs
      in
      match bvs with
      | [] -> V.Val (Constant.Concrete ASLScalar.empty) |> return
      | [ x ] -> return x
      | h :: t ->
          let folder acc v =
            let* acc = acc in
            M.op (Op.ArchOp ASLOp.Concat) acc v
          in
          List.fold_left folder (return h) t

    let bitvector_length v = M.op1 (Op.ArchOp1 ASLOp.BVLength) v

    (**************************************************************************)
    (* Primitives and helpers                                                 *)
    (**************************************************************************)

    type primitive_t = V.v M.t list -> V.v M.t list -> V.v M.t list M.t

    let vbool b = V.Val (Constant.Concrete (ASLScalar.S_Bool b))

    (*
     * Add equation, the effect will be silent
     * discard of execution candidate if a
     * contradiction appears.
     *)
    let check_prop _ii ~prop = prop >>= M.assign (vbool true)

    let check_eq _ii ~n:_ ~d1 ~d2 =
      let* d1 = d1 and* d2 = d2 in
      M.assign d1 d2

    (*
     * Primitives that generate fence events.
     * Notice that ASL fence events take
     * an AArch64 barrier as argument.
     *)

    let cutoffT (ii,poi) msg v = M.cutoffT msg (use_ii_with_poi ii poi) v

    let primitive_isb (ii, poi) () =
      create_barrier AArch64Base.ISB (use_ii_with_poi ii poi)

    let dom_of_label =
      let open AArch64Base in
      function
      | "MBReqDomain_Nonshareable" -> NSH
      | "MBReqDomain_InnerShareable" -> ISH
      | "MBReqDomain_OuterShareable" -> OSH
      | "MBReqDomain_FullSystem" -> SY
      | _ -> assert false

    and btyp_of_label =
      let open AArch64Base in
      function
      | "MBReqTypes_Reads" -> LD
      | "MBReqTypes_Writes" -> ST
      | "MBReqTypes_All" -> FULL
      | _ -> assert false

    let primitive_db constr (ii, poi) ~d:dom_m ~t:btyp_m =
      let* dom_v = dom_m and* btyp_v = btyp_m in
      let dom = dom_v |> v_to_label |> dom_of_label
      and btyp = btyp_v |> v_to_label |> btyp_of_label in
      create_barrier (constr (dom, btyp)) (use_ii_with_poi ii poi)

    let primitive_dmb = primitive_db (fun (d, t) -> AArch64Base.DMB (d, t))
    and primitive_dsb = primitive_db (fun (d, t) -> AArch64Base.DSB (d, t))

    (*
     * Primitives for read and write events.
     *)

    let virtual_to_loc_reg =
      let tgprs = Array.of_list AArch64Base.gprs in
      fun rv ii ->
        let i = v_as_int rv in
        if i >= Array.length tgprs || i < 0 then
          Warn.fatal "Invalid register number: %d" i
        else
          let arch_reg = AArch64Base.Ireg tgprs.(i) in
          A.Location_reg (ii.A.proc, ASLBase.ArchReg arch_reg)

    let read_register (ii, poi) ~reg:r_m =
      let* rval = r_m in
      let loc = virtual_to_loc_reg rval ii in
      read_loc MachSize.Quad loc aneutral aexp areg (use_ii_with_poi ii poi)
      >>= from_aarch64_val

    let write_register (ii, poi) ~reg:r_m ~data:v_m =
      let* v = v_m >>= to_aarch64_val and* r = r_m in
      let loc = virtual_to_loc_reg r ii in
      write_loc MachSize.Quad loc v aneutral aexp areg (use_ii_with_poi ii poi)

    let do_read_memory (ii, poi) addr_m datasize_m an aexp acc =
      let* addr = M.as_addr_port addr_m and* datasize = datasize_m in
      let sz = datasize_to_machsize datasize in
      read_loc sz (A.Location_global addr) an aexp acc (use_ii_with_poi ii poi)

    let read_memory ii ~n ~addr =
      do_read_memory ii addr n aneutral aexp avir

    let read_pte ii ~n ~addr =
      (* We do all the operations with 64 bits, even if the argument passed is different. *)
      let* _ = n in
      do_read_memory ii addr (M.unitT (V.intToV 64))
        aneutral (AArch64Explicit.(NExp Other)) apte

    let read_memory_gen ii ~n ~addr ~accdesc ~access =
      let* accdesc = accdesc and* access = access in
      do_read_memory ii addr n (accdesc_to_annot Read accdesc)
        aexp (access_to_access access)

    let do_write_memory (ii, poi) addr_m datasize_m value_m an aexp acc =
      let* addr = M.as_addr_port addr_m
      and* datasize = datasize_m
      and* value = value_m >>= to_aarch64_val in
      let sz = datasize_to_machsize datasize in
      write_loc sz (A.Location_global addr) value an aexp acc
        (use_ii_with_poi ii poi)

    (**************)
    (* PTE update *)
    (**************)

(*
 * To implement PTE update, we rely on special primitives,
 * because those accesses have special explicit annotations.
 * Those special annotations serve to build the AF and DB
 * sets that appear in the aarch64.cat model source text.
 *)

    let as_bool b_m =
      let* b =  b_m in
      let (>>=) = Option.bind in
      V.as_scalar b >>= ASLScalar.as_bool |>
      fun b -> M.unitT (Misc.as_some b)

    let pte_nexp_nat proc is_write =
      let open DirtyBit in
      let open AArch64Explicit in
      let d =
        match Conf.dirty with
        | None -> soft
        | Some d -> d in
      NExp
        (if is_write then
           if d.hd proc then AFDB
           else if d.ha proc then AF
           else Other
         else if d.ha proc then AF
         else Other)

    (* Always quad size, whatever parameter _N is *)
    let size_m_64 =  M.unitT (V.intToV 64)

    (* Second pte read, used for flag update *)
    let read_pte_again (iinst,_ as ii) ~n:_ ~addr ~is_write =
      let* is_write = as_bool is_write in
      let nexp_nat = pte_nexp_nat iinst.A.proc is_write in
      do_read_memory ii addr size_m_64
        aatomic nexp_nat apte

    (* Pte write for flag update *)
    let write_pte (iinst,_ as ii) ~n:_ ~addr ~data ~is_write =
      let* is_write = as_bool is_write in
      let nexp_nat = pte_nexp_nat iinst.A.proc is_write in
      do_write_memory ii addr size_m_64 data aatomic nexp_nat apte

    (*********************)
    (* End of PTE update *)
    (*********************)

    let write_memory ii ~n ~addr ~data =
      do_write_memory ii addr n data aneutral aexp avir

    let write_memory_gen ii ~n ~addr ~data ~accdesc ~access =
      let* accdesc = accdesc and* access = access in
      do_write_memory ii addr n data (accdesc_to_annot Write accdesc) aexp
        (access_to_access access)

    let data_abort_fault (ii,_) ~addr ~write ~statuscode ~accdesc =
      (*
       * Notice: write specifies that writing yields a fault.
       * The executed instruction may be a store or a RMO.
       *)
      let* loc = addr
      and* write = write
      and* statuscode = statuscode
      and* accessdesc = accdesc in
      let d =
        match Option.bind (V.as_scalar write) ASLScalar.as_bool with
        | Some true -> Dir.W
        | Some false -> Dir.R
        | None ->
            Warn.fatal "data_abort boolean expected, found %s"
              (V.pp true write)
      and ft =
        let open FaultType.AArch64 in
        match  Option.bind (V.as_scalar statuscode) ASLScalar.as_label with
        | Some "Fault_AccessFlag" -> MMU AccessFlag
        | Some "Fault_Translation" -> MMU Translation
        | Some "Fault_Permission" -> MMU Permission
(* NB: this fault should not occur, meaning that the current execution
   will be discarderd later. *)
        | Some "Fault_Exclusive" -> MMU Exclusive
        | _ ->
          Warn.warn_always
            "data_abort, fault expected, found %s\n"
            (V.pp_v statuscode) ;
          assert false
      and loc = A.Location_global loc in
      let a = accdesc_to_annot Fault accessdesc in
      M.mk_singleton_es (Act.Fault (ii,loc,d,a,ft)) ii

    let get_ha_or_hd get (ii,_) () =
      let dirty =
        match Conf.dirty with
        | None -> DirtyBit.soft
        | Some f -> f in
      let h = get dirty ii.A.proc in
      V.Val (Constant.Concrete (ASLScalar.bv_of_bool h)) |> M.unitT

    let get_ha = get_ha_or_hd (fun d p -> d.DirtyBit.ha p || d.DirtyBit.hd p)
    and get_hd = get_ha_or_hd (fun d -> d.DirtyBit.hd)

    (**************************************************************************)
    (* ASL environment                                                        *)
    (**************************************************************************)

    module RawPrimitives = struct
      type v = V.v
      type 'a m = 'a M.t
      type ii = A.inst_instance_id * A.program_order_index ref

      let return = return
      let bind = ( >>= )

      let arity_error s ~expected ~actual =
        Warn.fatal "Arity error for function %s: expected %d and received %d."
          s expected actual

      let u_int _ii ~n:_ ~x = x >>= to_int_unsigned
      let s_int _ii ~n:_ ~x = x >>= to_int_signed
      let is_virtual _ii ~addr = addr >>= M.op1 Op.IsVirtual
      let compute_pte_primitive _ii ~addr = addr >>= M.op1 Op.PTELoc
      and get_oa_primitive _ii ~n:_ ~addr = addr >>= M.op1 (Op.ArchOp1 ASLOp.OA)
      and offset_primitive _ii ~addr = addr >>= M.op1 Op.Offset

      let primitive_isb = primitive_isb
      let primitive_dmb = primitive_dmb
      let primitive_dsb = primitive_dsb
      let read_register = read_register
      let write_register = write_register
      let read_memory = read_memory
      let read_memory_gen = read_memory_gen
      let write_memory = write_memory
      let write_memory_gen = write_memory_gen
      let read_pte_primitive = read_pte
      let data_abort_primitive = data_abort_fault
      let get_ha_primitive = get_ha
      let get_hd_primitive = get_hd
      let read_pte_again_primitive = read_pte_again
      let write_pte_primitive = write_pte
      let check_prop = check_prop
      let check_eq = check_eq
    end

    module Primitives = ASLSemPrimitives.Make(RawPrimitives)

    let primitives =
      let signatures =
        Asllib.Builder.from_string `ASLv1 ~filename:"primitives.asl"
          ~ast_string:ASLSemPrimitivesSig.primitives_signatures
      in
      Asllib.ASTUtils.plug_primitives signatures Primitives.primitives

    let build_shared_pseudocode () =
      let open AST in
      let open ASTUtils in
      let ( @! ) = List.rev_append in
      let patches =
        build "patches.asl" @!
        build (if is_vmsa then "patches-vmsa.asl" else "patches-std.asl")
      and custom_implems =
        build "implementations.asl" @!
        build (if is_vmsa then "physmem-vmsa.asl" else "physmem-std.asl")
      and shared =
        build "system_registers.asl"
        @! build "features.asl"
        @! Lazy.force built_shared_pseudocode
      in
      let ast = patch ~patches:(custom_implems @! patches) ~src:shared in
      Asllib.Builder.with_stdlib ast
      |> Asllib.Builder.with_primitives primitives
      |> TypeCheck.type_check_ast

    let typed_shared_pseudocode : unit -> AST.t * Asllib.StaticEnv.global =
      let if_asl_aarch64 = Lazy.from_fun build_shared_pseudocode
      and otherwise =
        lazy
          (Lazy.force Asllib.Builder.stdlib
          |> Asllib.Builder.with_primitives primitives
          |> TypeCheck.type_check_ast)
      in
      fun () ->
        Lazy.force
        @@ if variant Variant.ASL_AArch64 then if_asl_aarch64 else otherwise

    (**************************************************************************)
    (* Execution                                                              *)
    (**************************************************************************)

    let build_semantics t ii =
      let ii_env = (ii, ref ii.A.program_order_index) in
      let module ASLBackend = struct
        module Scope = Scope

        type value = V.v
        type value_range = value * value
        type 'a m = 'a M.t
        type primitive = primitive_t

        let debug_value = V.pp_v
        let is_undetermined = function V.Var _ -> true | V.Val _ -> false
        let v_of_int = V.intToV
        let v_of_literal = v_of_literal
        let v_to_z = v_to_z
        let v_to_label = v_to_label
        let bind_data = M.asl_data
        let bind_seq = M.asl_seq
        let bind_ctrl = M.asl_ctrl
        let prod_par = M.( >>| )
        let appl_data m f = m >>= fun v -> return (f v)
        let debugT = M.debugT
        let commit = commit ii_env
        let choice = choice
        let delay m k = M.delay_kont "ASL" m k
        let return = M.unitT
        let cutoffT msg v = cutoffT ii_env msg v
        let on_write_identifier = on_write_identifier ii_env
        let on_read_identifier = on_read_identifier ii_env
        let binop = binop
        let unop = unop
        let ternary = ternary
        let create_vector = create_vector
        let create_record = create_record
        let create_exception = create_exception
        let get_index = get_index
        let set_index = set_index
        let get_field = get_field
        let set_field = set_field
        let read_from_bitvector = read_from_bitvector
        let write_to_bitvector = write_to_bitvector
        let concat_bitvectors = concat_bitvectors
        let bitvector_length = bitvector_length
        let v_unknown_of_type = v_unknown_of_type
        let primitives = List.map (fun (func, f) -> (func, f ii_env)) primitives
        let fail = M.failT
      end in
      let module ASLInterpreter =
        Asllib.Interpreter.Make (ASLBackend) (ASLInterpreterConfig)
      in
      let ast, tenv =
        let shared_ast, shared_tenv = typed_shared_pseudocode () in
        let main, tenv = TypeCheck.type_check_ast_in_env shared_tenv ii.A.inst in
        (List.rev_append main shared_ast, tenv)
      in
      let () =
        if false then Format.eprintf "Completed AST: %a.@." Asllib.PP.pp_t ast
      in
      let env = [ ("_ProcessorID", V.intToV ii.A.proc) ] in
      let env =
        A.state_fold
          (fun loc v env ->
            match loc with
            | A.Location_reg (_, ASLLocalId (Scope.Global _, name)) ->
                (name, v) :: env
            | _ -> env)
          t.Test_herd.init_state env
      in
      let exec () =
        let main_name = TypeCheck.find_main tenv in
        ASLInterpreter.run_typed_env env tenv main_name ast
      in
      let* i =
        match Asllib.Error.intercept exec () with
        | Ok m -> m
        | Error err -> Asllib.Error.error_to_string err |> Warn.fatal "%s"
      in
      assert (V.equal i V.zero);
      M.addT !(snd ii_env) B.nextT

    let spurious_setaf _ = assert false
  end
end
