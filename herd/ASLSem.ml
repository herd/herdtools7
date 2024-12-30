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

  let is_experimental = variant Variant.ASLExperimental

  module TypeCheck = Asllib.Typing.Annotate (struct
    let check =
      let open Asllib.Typing in
      if variant (Variant.ASLType `Warn) then Warn
      else if variant (Variant.ASLType `TypeCheck) then TypeCheckNoWarn
      else Silence

    let output_format = Asllib.Error.HumanReadable
    let print_typed = false
    let use_field_getter_extension = is_experimental
  end)

  module ASLInterpreterConfig = struct
    let unroll =
      match Conf.C.unroll with None -> Opts.unroll_default `ASL | Some u -> u

    let error_handling_time = Asllib.Error.Dynamic
    let log_nondet_choice = false

    module Instr = Asllib.Instrumentation.SemanticsNoInstr
  end

  let is_kvm = variant Variant.VMSA
  let barriers = []
  let isync = None
  let atomic_pair_allowed _ _ = true
  let aneutral = AArch64Annot.N
  and aexp = AArch64Explicit.Exp
  and aifetch = AArch64Explicit.(NExp IFetch)
  and areg = Access.REG
  and avir = Access.VIR
  and apte = Access.PTE
  and aphy = Access.PHY
  let areg_std = (aneutral,aexp,Access.REG)

  module Mixed (SZ : ByteSize.S) : sig
    val build_semantics : test -> A.inst_instance_id -> (proc * branch) M.t
    val spurious_setaf : A.V.v -> unit M.t
  end = struct
    module Mixed = M.Mixed (SZ)

    let ( let* ) = M.asl_data
    let ( let*| ) = M.asl_seq
    let ( and* ) = M.( >>| )
    let return = M.unitT
    let ( >>= ) = M.asl_data
    let ( >>! ) = M.( >>! )

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
        | L_String _f -> Warn.fatal "Cannot strings in herd yet."
        | L_Label (_, i) -> S_Int (Z.of_int i) |> concrete
      in
      fun v -> V.Val (tr v)

    let v_to_int = function
      | V.Val (Constant.Concrete (ASLScalar.S_Int i)) -> Some (Z.to_int i)
      | _ -> None

    let v_as_int = function
      | V.Val (Constant.Concrete i) -> V.Cst.Scalar.to_int i
      | v -> Warn.fatal "Cannot concretise symbolic value %s as an int" (V.pp_v v)

    let v_as_record = function
      | V.Val (Constant.ConcreteRecord map) -> map
      | v ->
          Warn.fatal "Cannot concretise symbolic value %s as a record"
            (V.pp_v v)

    let v_as_bool = function
      | Constant.Concrete (ASLScalar.S_Bool b) -> b
      | c ->
          Warn.fatal "Cannot concretise symbolic value %s as a boolean"
            (V.pp_v (V.Val c))

    let datasize_to_machsize v =
      match v_as_int v with
      | 32 -> MachSize.Word
      | 64 -> MachSize.Quad
      | 128 -> MachSize.S128
      | _ ->
          Warn.fatal
            "Cannot access a register or memory with size %s" (V.pp_v v)

    let access_bool_field v f map =
      try StringMap.find f map |> v_as_bool
      with Not_found -> Warn.fatal "Record %s has no %s field" (V.pp_v v) f

    let accdesc_to_annot is_read accdesc =
      let open AArch64Annot in
      let map = v_as_record accdesc in
      let is_release = access_bool_field accdesc "relsc" map
      and is_acquiresc = access_bool_field accdesc "acqsc" map
      and is_acquirepc = access_bool_field accdesc "acqpc" map
      and is_atomic = access_bool_field accdesc "atomicop" map
      and is_exclusive = access_bool_field accdesc "exclusive" map in
      let is_ax x n = if is_atomic || is_exclusive then x else n in
      let an =
        if (not is_read) && is_release then is_ax XL L
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
      match v_as_int acc with
      | 0 -> REG
      | 1 -> VIR
      | 2 -> PHY
      | 3 -> PTE
      | 4 -> TLB
      | 5 -> TAG
      | 6 -> PHY_PTE
      | i -> Warn.fatal "Bad access code from ASL: %d" i

(* Why so ?
    let wrap_op1_symb_as_var op1 = function
      | V.Val (Constant.Symbolic _) as v ->
          let v' = V.fresh_var () in
          M.restrict M.VC.[ Assign (v', Unop (op1, v)) ] >>! v'
      | v -> M.op1 op1 v
*)

(* Can be more efficient, because symbols get transmited to ASL,
 * resulting in more semantics time evaluation and, hopefull,
 * less unresolved conditional statements.
 *)

    let wrap_op1_symb_as_var op1 = M.op1 op1

    let to_bv sz =
      wrap_op1_symb_as_var
        (Op.ArchOp1 (ASLOp.ToBV (MachSize.nbits sz)))
    let to_int_unsigned = wrap_op1_symb_as_var (Op.ArchOp1 ASLOp.ToIntU)
    let to_int_signed = wrap_op1_symb_as_var (Op.ArchOp1 ASLOp.ToIntS)
    let to_aarch64_val =
      wrap_op1_symb_as_var (Op.ArchOp1 ASLOp.ToAArch64)
    and from_aarch64_val =
      wrap_op1_symb_as_var (Op.ArchOp1 ASLOp.FromAArch64)

    (**************************************************************************)
    (* Special monad interactions                                              *)
    (**************************************************************************)

    let create_barrier b ii = M.mk_singleton_es (Act.Barrier b) ii >>! []

    let resize_from_quad = function
      | MachSize.Quad -> return
      | sz -> (
          function
          | V.Val (Constant.Symbolic _) as v -> return v
          | v -> M.op1 (Op.Mask sz) v)

    let write_loc sz loc v a e acc ii =
      let* resized_v = resize_from_quad sz v in
      let mk_action loc' =
        Act.Access (Dir.W, loc', resized_v, sz, (a, e, acc)) in
      M.write_loc mk_action loc ii

    let read_loc sz loc a e acc ii =
      let mk_action loc' v' =
        Act.Access (Dir.R, loc', v', sz, (a, e, acc)) in
      let* v = M.read_loc false mk_action loc ii in
      resize_from_quad sz v >>= to_bv sz

    (**************************************************************************)
    (* ASL-Backend implementation                                             *)
    (**************************************************************************)

    let commit (ii,poi) msg =
      M.mk_singleton_es (Act.Branching msg) (use_ii_with_poi ii poi)

    let choice (m1 : V.v M.t) (m2 : 'b M.t) (m3 : 'b M.t) : 'b M.t =
      M.asl_data m1 @@ function
        | V.Val (Constant.Concrete (ASLScalar.S_Bool b)) -> if b then m2 else m3
        | V.Var _ as b ->
            M.asl_data (to_int_signed b) (fun v -> M.choiceT v m2 m3)
        | V.Val c ->
            Warn.fatal "Cannot concretise symbolic value %s as a boolean"
              (V.pp_v (V.Val c))

    let binop =
      let open AST in
      let to_bool op v1 v2 = op v1 v2 >>= M.op1 (Op.ArchOp1 ASLOp.ToBool) in
      let or_ v1 v2 =
        match (v1, v2) with
        | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)), v
          when Asllib.Bitvector.is_zeros bv ->
            return v
        | v, V.Val (Constant.Concrete (ASLScalar.S_BitVector bv))
          when Asllib.Bitvector.is_zeros bv ->
            return v
        | _ -> M.op Op.Or v1 v2
      in
      function
      | AND -> M.op Op.And
      | BAND -> M.op Op.And
      | BEQ -> M.op Op.Eq |> to_bool
      | BOR -> M.op Op.Or
      | DIV -> M.op Op.Div
      | MOD -> M.op Op.Rem
      | DIVRM -> M.op (Op.ArchOp ASLOp.Divrm)
      | EOR -> M.op Op.Xor
      | EQ_OP -> M.op Op.Eq |> to_bool
      | GT -> M.op Op.Gt |> to_bool
      | GEQ -> M.op Op.Ge |> to_bool
      | LT -> M.op Op.Lt |> to_bool
      | LEQ -> M.op Op.Le |> to_bool
      | MINUS -> M.op Op.Sub
      | MUL -> M.op Op.Mul
      | NEQ -> M.op Op.Ne |> to_bool
      | OR -> or_
      | PLUS -> M.op Op.Add
      | SHL -> M.op Op.ShiftLeft
      | SHR -> M.op Op.ShiftRight
      | BV_CONCAT -> M.op (Op.ArchOp ASLOp.Concat)
      | (POW | IMPL | RDIV) as op ->
          Warn.fatal "ASL operation %s not yet implement in ASLSem."
            (Asllib.PP.binop_to_string op)

    let unop op =
      let open AST in
      match op with
      | BNOT -> wrap_op1_symb_as_var (Op.ArchOp1 ASLOp.BoolNot)
      | NEG -> M.op Op.Sub V.zero
      | NOT -> wrap_op1_symb_as_var Op.Inv

    let ternary = function
      | V.Val (Constant.Concrete (ASLScalar.S_Bool true)) -> fun m1 _ -> m1 ()
      | V.Val (Constant.Concrete (ASLScalar.S_Bool false)) -> fun _ m2 -> m2 ()
      | v ->
          fun m1 m2 ->
            let* v1 = m1 () and* v2 = m2 () and* v = to_int_signed v in
            M.op3 Op.If v v1 v2

    (*
     * Any access to `PSTATE` (experimental `_NZCV`)
     * emits an access to NZCV.
     * Notice that the value is casted into an integer.
     *)

    let is_nzcv =
      if is_experimental then
        fun x scope ->
          match (x, scope) with
          | "_NZCV", Scope.Global false -> true
          | _ -> false
      else
        fun x scope ->
          match (x, scope) with
          | "PSTATE", Scope.Global false -> true
          | _ -> false

    let loc_of_scoped_id ii x scope =
      if is_nzcv x scope then
        A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.NZCV)
      else A.Location_reg (ii.A.proc, ASLBase.ASLLocalId (scope, x))

    let mk_std_access (ii,poi) dir loc v =
      let action = Act.Access (dir, loc, v, MachSize.Quad, areg_std) in
      M.mk_singleton_es action (use_ii_with_poi ii poi)

    let on_access_identifier dir (ii,_ as ii_poi) x scope v =
      let loc = loc_of_scoped_id ii x scope in
      let m v =  mk_std_access ii_poi dir loc v in
      if is_nzcv x scope then M.op1 (Op.ArchOp1 ASLOp.ToIntU) v >>= m
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

    let read_from_bitvector positions bvs =
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

    let checkprop _ m_prop =
      let* vprop = m_prop in
      M.assign (vbool true) vprop >>! []

    (*
     * Split the current execution candidate into two:
     * one candidate, has variable [v] value to be TRUE,
     * while the value is FALSE for the other.
     *)

    let somebool _ _ =
      let v = V.fresh_var () in
      let mbool b =
        (* The underlying choice operator operates
         * by adding equations ToInt(v) := 1
         * and ToInt(v) := 0, which our naive solver
         * does not resolve as v=TRUE and v=FALSE,
         * Thereby leaving the equation unsolved.
         * To correct this, we add
         * the direct equations on "v":
         * v := TRUE and v := FALSE in the
         * positive and negative branches of choice.
         *)
        let* () = M.assign v (vbool b) in
        M.unitT (vbool b) in
      (* Using "choice" and not returning "v" directly performs the split *)
      choice (M.unitT v) (mbool true) (mbool false)

    (*
     * Primitives that generate fence events.
     * Notice that ASL fence events take
     * an AArch64 barrier as argument.
     *)

    let cutoffT (ii,poi) msg v = M.cutoffT msg (use_ii_with_poi ii poi) v

    let primitive_isb (ii, poi) () =
      create_barrier AArch64Base.ISB (use_ii_with_poi ii poi)

    let dom_of =
      let open AArch64Base in
      function 0 -> NSH | 1 -> ISH | 2 -> OSH | 3 -> SY | _ -> assert false

    and btyp_of =
      let open AArch64Base in
      function 0 -> LD | 1 -> ST | 2 -> FULL | _ -> assert false

    let primitive_db constr (ii, poi) dom_m btyp_m =
      let* dom = dom_m and* btyp = btyp_m in
      let dom = v_as_int dom and btyp = v_as_int btyp in
      let dom = dom_of dom and btyp = btyp_of btyp in
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

    let read_register (ii, poi) r_m =
      let* rval = r_m in
      let loc = virtual_to_loc_reg rval ii in
      read_loc MachSize.Quad loc aneutral aexp areg (use_ii_with_poi ii poi)
      >>= from_aarch64_val

    let write_register (ii, poi) r_m v_m =
      let* v = v_m >>= to_aarch64_val and* r = r_m in
      let loc = virtual_to_loc_reg r ii in
      write_loc MachSize.Quad loc v aneutral aexp areg (use_ii_with_poi ii poi) >>! []

    let loc_pc ii = A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.PC)

    let read_pc (ii,poi) () =
      read_loc MachSize.Quad (loc_pc ii) aneutral aexp areg
        (use_ii_with_poi ii poi)

    let write_pc (ii,poi) v_m =
      let* v = v_m >>= to_aarch64_val in
      write_loc MachSize.Quad (loc_pc ii)
        v aneutral aexp areg (use_ii_with_poi ii poi) >>! []

    let on_access_resaddr dir (ii,_ as ii_poi) v_m =
      let* v = v_m >>= to_aarch64_val in
      let loc =
        A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.ResAddr) in
      mk_std_access ii_poi dir loc v >>! []

    let on_read_resaddr = on_access_resaddr Dir.R
    and on_write_resaddr = on_access_resaddr Dir.W

    let do_read_memory (ii, poi) addr_m datasize_m an aexp acc =
      let* addr = addr_m and* datasize = datasize_m in
      let sz = datasize_to_machsize datasize in
      read_loc sz (A.Location_global addr) an aexp acc (use_ii_with_poi ii poi)

    let read_memory ii datasize_m addr_m =
      do_read_memory ii addr_m datasize_m aneutral aexp avir

    let read_pte ii addr_m =
      let* addr = addr_m in
      do_read_memory ii (M.unitT addr)  (M.unitT (V.intToV 64))
        aneutral (AArch64Explicit.(NExp Other)) apte

    let read_memory_gen ii datasize_m addr_m accdesc_m access_m =
      let* accdesc = accdesc_m and* access = access_m in
      do_read_memory ii addr_m datasize_m (accdesc_to_annot true accdesc)
        aexp (access_to_access access)

    let do_write_memory (ii, poi) addr_m datasize_m value_m an aexp acc =
      let value_m = M.as_data_port value_m in
      let* addr = addr_m and* datasize = datasize_m and* value = value_m in
      let sz = datasize_to_machsize datasize in
      write_loc sz (A.Location_global addr) value an aexp acc
        (use_ii_with_poi ii poi)
      >>! []

    let write_pte (iinst,_ as ii) addr_m val_m write_m =
      let* is_write = write_m in
      let is_write =
        let (>>=) = Option.bind in
        V.as_scalar is_write >>= ASLScalar.as_bool |> Misc.as_some in
      let open AArch64Explicit in
      let open DirtyBit in
      let d =
        match Conf.dirty with
        | None -> soft
        | Some d -> d in
      let nexp_nat =
        if is_write then
          if d.hd iinst.A.proc then AFDB
          else if d.ha iinst.A.proc then AF
          else Other
        else if d.ha iinst.A.proc then AF
        else Other in
      do_write_memory ii addr_m (M.unitT (V.intToV 64)) val_m
        aneutral (NExp nexp_nat) apte

    let write_memory ii datasize_m addr_m value_m =
      do_write_memory ii addr_m datasize_m value_m aneutral aexp avir

    let write_memory_gen ii datasize_m addr_m value_m accdesc_m access_m =
      let* accdesc = accdesc_m and* access = access_m in
      do_write_memory ii addr_m datasize_m value_m
        (accdesc_to_annot false accdesc)  aexp (access_to_access access)

    let loc_sp ii = A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.SP)

    let read_sp (ii, poi) () =
      read_loc MachSize.Quad (loc_sp ii) aneutral aexp areg
        (use_ii_with_poi ii poi)

    let write_sp (ii, poi) v_m =
      let* v = v_m >>= to_aarch64_val in
      write_loc MachSize.Quad (loc_sp ii) v aneutral aexp areg
        (use_ii_with_poi ii poi)
      >>! []

    let uint _ bv_m = bv_m >>= to_int_unsigned
    let sint _ bv_m = bv_m >>= to_int_signed
    let processor_id (ii, _poi) () = return (V.intToV ii.A.proc)
    let is_virtual _ addr_m =
      addr_m
      >>= M.op1 Op.IsVirtual
      >>=  M.op1 (Op.ArchOp1 ASLOp.ToBool)

    let can_predict_from _ v_m w_m =
      let diff_case = v_m in
      let eq_case = M.altT v_m w_m in
      let*| v = v_m and* w = w_m in
      let*| c = M.op Op.Eq v w in
      M.choiceT c eq_case diff_case

    let compute_pte _ addr = addr >>= M.op1 Op.PTELoc
    and get_oa _ pte = pte >>= M.op1 (Op.ArchOp1 ASLOp.OA)
    and get_offset _ ma = ma >>= M.op1 Op.Offset

    let data_abort_fault (ii,_) addr write statuscode =
      let* loc = addr
      and* write = write
      and* statuscode = statuscode  in
      let d =
        match Option.bind (V.as_scalar write) ASLScalar.as_bool with
        | Some true -> Dir.W
        | Some false -> Dir.R
        | None ->
            Warn.fatal "data_abort boolean expected, found %s"
              (V.pp true write)
      and ft =
        let open FaultType.AArch64 in
        match  Option.bind (V.as_scalar statuscode) ASLScalar.as_int with
        | Some 1 -> MMU AccessFlag
        | Some 6 -> MMU Translation
        | Some 5 -> MMU Permission
        | _ -> assert false
      and loc = A.Location_global loc in
      M.mk_singleton_es (Act.Fault (ii,loc,d,ft)) ii >>! []

    let get_ha_or_hd get (ii,_) () =
      let dirty =
        match Conf.dirty with
        | None -> DirtyBit.soft
        | Some f -> f in
      let h = get dirty ii.A.proc in
      V.Val (Constant.Concrete (ASLScalar.bv_of_bool h)) |> M.unitT

    let get_ha  = get_ha_or_hd  (fun d p -> d.DirtyBit.ha p || d.DirtyBit.hd p)
    and get_hd  = get_ha_or_hd  (fun d -> d.DirtyBit.hd)

    (**************************************************************************)
    (* ASL environment                                                        *)
    (**************************************************************************)

    (* Helpers *)
    let build_primitive ?(args = []) ?returns ?(parameters = []) ~side_effecting
        name f : AST.func * (_ -> primitive_t) =
      let open AST in
      let subprogram_type =
        match returns with None -> ST_Procedure | _ -> ST_Function
      and body = SB_Primitive side_effecting
      and recurse_limit = None
      and return_type = returns in
      ( {
          name;
          args;
          body;
          return_type;
          parameters;
          subprogram_type;
          recurse_limit;
          builtin = true;
        }
        [@warning "-40-42"],
        f )

    (*
     * Functions that build primitives from underlying OCaml functions.
     *
     * The function [pX] is building primitives with arity X
     * and no return value.
     * The function [pXr] is building primitives with arity X
     * and a return value.
     * All those conveniently ignore the parameters,
     * which can thus be of any type in any number.
     *
     * A few primitive builder pass parameters to the
     * underlying OCaml function: for instance,
     * `p1a3` accepts one parameter, three arguments
     * and returns no value.
     *)

    (** Build a primitive with arity 0 and no return value. *)
    let p0 name ?parameters ?(side_effecting = false) f =
      let f ii_env _ = function
        | [] -> f ii_env ()
        | _ :: _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?parameters ~side_effecting name f

    (** Build a primitive with arity 0 and a return value. *)
    let p0r name ~returns ?(side_effecting = false) f =
      let f ii_env _ = function
        | [] -> return [ f ii_env () ]
        | _ :: _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?returns:(Some returns) ~side_effecting name f

    (** Build a primitive with arity 1 and no return value. *)
    let p1 name arg ?parameters ?(side_effecting = false) f =
      let f ii_env _ = function
        | [ v ] -> f ii_env v
        | [] | _ :: _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ~args:[ arg ] ~side_effecting ?parameters name f

    (** Build a primitive with arity 1 and a return value. *)
    let p1r name arg ~returns ?(side_effecting = false) ?parameters f =
      let f ii_env _ = function
        | [ v ] -> return [ f ii_env v ]
        | [] | _ :: _ :: _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?returns:(Some returns) ~args:[ arg ] ~side_effecting
        ?parameters name f

    (** Build a primitive with arity 2 and no return value. *)
    let p2 name arg1 arg2 ?parameters ?(side_effecting = false) f =
      let f ii_env _ = function
        | [ v1; v2 ] -> f ii_env v1 v2
        | _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ~args:[ arg1; arg2 ] ~side_effecting ?parameters name f

    (** Build a primitive with arity 2 and a return value. *)
    let p2r name arg1 arg2 ~returns ?(side_effecting = false) ?parameters f =
      let f ii_env _ = function
        | [ v1; v2 ] -> return [ f ii_env v1 v2 ]
        | _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?returns:(Some returns) ~args:[ arg1; arg2 ]
        ~side_effecting ?parameters name f

    (** Build a primitive with arity 3 and no return value. *)
    let p3 name arg1 arg2 arg3 ?(side_effecting = false) ?parameters f =
      let f ii_env _ = function
        | [ v1; v2; v3; ] -> f ii_env v1 v2 v3
        | _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ~args:[ arg1; arg2; arg3; ] ?parameters
        ~side_effecting name f

    (** Build various primitives with 1 parameter. *)
    let p1a1r name param1 arg1 ?(side_effecting = false) ~returns f =
      let f ii_env params args =
        match (params, args) with
        | [ v1 ], [ v2 ] -> return [ f ii_env v1 v2 ]
        | _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?returns:(Some returns)
        ~args:[ arg1; ] ~parameters:[param1;]
        ~side_effecting name f

    let p1a2 name param1 arg1 arg2 ?(side_effecting = false) f =
      let f ii_env params args =
        match params,args with
        | [v1],[v2; v3; ] -> f ii_env v1 v2 v3
        | _ -> Warn.fatal "Arity error for function %s." name in
      build_primitive
        ~args:[ arg1; arg2; ] ~parameters:[param1;]
        ~side_effecting name f

    let p1a3r name param1 arg1 arg2 arg3 ~returns
        ?(side_effecting = false)  f =
      let f ii_env params args =
        match params,args with
        | [ v1; ], [ v2; v3; v4; ] -> return [ f ii_env v1 v2 v3 v4; ]
        | _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?returns:(Some returns)
        ~args:[ arg1; arg2; arg3; ] ~parameters:[param1;]
        ~side_effecting name f

    let p1a4 name param1 arg1 arg2 arg3 arg4
        ?(side_effecting = false) f =
      let f ii_env params args =
        match params,args with
        | [v1],[v2; v3; v4; v5; ] -> f ii_env v1 v2 v3 v4 v5
        | _ -> Warn.fatal "Arity error for function %s." name in
      build_primitive
        ~args:[ arg1; arg2; arg3; arg4;]
        ~parameters:[param1;]
        ~side_effecting name f

    (* Primitives *)
    let extra_funcs =
      let open AST in
      let with_pos e = Asllib.ASTUtils.add_dummy_annotation ~version:V0 e in
      let integer = Asllib.ASTUtils.integer in
      let int_ctnt e1 e2 =
        T_Int (WellConstrained [ Constraint_Range (e1, e2) ]) |> with_pos
      in
      let boolean = Asllib.ASTUtils.boolean in
      let reg = integer in
      let var x = E_Var x |> with_pos in
      let lit x = E_Literal (L_Int (Z.of_int x)) |> with_pos in
      let bv x = T_Bits (x, []) |> with_pos in
      let bv_var x = bv @@ var x in
      let bv_lit x = bv @@ lit x in
      let bv_64 = bv_lit 64 in
      let ia_msb = 8 in
      let binop = Asllib.ASTUtils.binop in
      let minus_one e = binop MINUS e (lit 1) in
      let pow_2 = binop POW (lit 2) in
      let t_named x = T_Named x |> with_pos in
      let side_effecting = true in
      let uint_returns = int_ctnt (lit 0) (minus_one (pow_2 (var "N")))
      and sint_returns =
        let big_pow = pow_2 (minus_one (var "N")) in
        int_ctnt (E_Unop (NEG, big_pow) |> with_pos) (minus_one big_pow)
      in
      [
        (* Fences *)
        p0 "primitive_isb" ~side_effecting primitive_isb;
        p2 "primitive_dmb" ~side_effecting ("d", integer) ("t", integer)
          primitive_dmb;
        p2 "primitive_dsb" ~side_effecting ("d", integer) ("t", integer)
          primitive_dsb;
        (* Registers *)
        p1r "read_register" ~side_effecting
          ("reg", reg) ~returns:bv_64 read_register;
        p2 "write_register" ~side_effecting
          ("data", bv_64) ("reg", reg) write_register;
        p0r "read_pc" ~side_effecting ~returns:bv_64 read_pc;
        p1 "write_pc" ~side_effecting ("data", bv_64) write_pc;
        p1 "on_read_resaddr" ~side_effecting
          ("data", bv_64) on_read_resaddr;
        p1 "on_write_resaddr" ~side_effecting
          ("data", bv_64) on_write_resaddr;
        p0r "SP_EL0" ~side_effecting ~returns:bv_64 read_sp;
        p1 "SP_EL0" ~side_effecting ("data", bv_64) write_sp;
        (* Memory *)
        p1a1r "read_memory"  ("N", None) ("addr", bv_64)
          ~returns:(bv_var "N")
          ~side_effecting read_memory;
        p1a3r "read_memory_gen" ("N",None)
          ("addr", bv_64)
          ("accdesc", t_named "AccessDescriptor")
          ("access", t_named "EventAccess")
          ~returns:(bv_var "N")
          ~side_effecting read_memory_gen;
        p1a2 "write_memory" ("N", None)
          ("addr", bv_64)
          ("data", bv_var "N")
          ~side_effecting write_memory;
        p1a4 "write_memory_gen" ("N",None)
          ("addr", bv_64)
          ("data", bv_var "N")
          ("accdesc", t_named "AccessDescriptor")
          ("access", t_named "EventAccess")
          ~side_effecting
          write_memory_gen;
(* VMSA *)
        p1r ~side_effecting "ComputePtePrimitive"
          ("addr", bv_64) ~returns:bv_64 compute_pte;
        p1r ~side_effecting "ReadPtePrimitive"
          ("addr", bv_64) ~returns:bv_64 read_pte;
        p1r ~side_effecting "GetOAPrimitive"
          ("addr", bv_64) ~returns:(bv_lit (64-ia_msb)) get_oa;
        p1r ~side_effecting "OffsetPrimitive"
          ("addr", bv_64) ~returns:(bv_lit ia_msb) get_offset;
        p3 ~side_effecting "DataAbortPrimitive"
          ("addr",bv_64) ("write",boolean) ("statuscode",integer)
          data_abort_fault;
        p0r "GetHaPrimitive" ~returns:(bv_lit 1) get_ha;
        p0r "GetHdPrimitive" ~returns:(bv_lit 1) get_hd;
        p3 "WritePtePrimitive"
          ("addr", bv_64)  ("data",bv_64) ("is_write",boolean) write_pte;
(* Translations *)
         p1r "UInt"
          ~parameters:[ ("N", None) ]
          ("x", bv_var "N")
          ~returns:uint_returns uint;
        p1r "SInt"
          ~parameters:[ ("N", None) ]
          ("x", bv_var "N")
          ~returns:sint_returns sint;
(* Misc *)
        p0r "ProcessorID" ~side_effecting ~returns:integer processor_id;
        p1r "IsVirtual" ~side_effecting
          ("addr",bv_64)  ~returns:boolean is_virtual;
        p2r "CanPredictFrom" ~side_effecting
          ~parameters:[ ("N", None) ]
          ("predicted", bv_var "N")
          ("from", bv_var "N")
          ~returns:(bv_var "N") can_predict_from;
        p0r ~side_effecting "SomeBoolean" ~returns:boolean somebool;
        p1 ~side_effecting "CheckProp" ("prop", boolean) checkprop;
      ]

    let make_extra_funcs ii_env =
      List.map
        (fun (func, make_primitive) -> (func, make_primitive ii_env))
        extra_funcs

    let build_shared_pseudocode () =
      let open AST in
      let open ASTUtils in
      let is_primitive =
        let set =
          List.fold_left
            (fun [@warning "-42"] acc ({ name; body = _; _ }, _) ->
               ISet.add name acc)
            ISet.empty extra_funcs
        in
        fun name -> ISet.mem name set
      in
      let build ?ast_type version fname =
        Filename.concat "asl-pseudocode" fname
        |> Conf.libfind
        |> ASLBase.build_ast_from_file ?ast_type version
      in
      let patches =
        let patches = build `ASLv1 "patches.asl" in
        let patches =
          if is_experimental then
            (* Replace default "PSTATE" definition by experimental ones. *)
            let pstate = build `ASLv1 "pstate-exp.asl" in
            List.fold_right
              (fun d k ->
                 match identifier_of_decl d with
                 | "PSTATE" -> pstate @ k
                 | _ -> d :: k)
            patches []
          else patches in
        if is_kvm then
          (* Adapt for VMSA:
           * 1. Use default address translation.
           * 2. Override some functions (see file patches-kvm.asl)
          *)
          let patches_kvm = build `ASLv1 "patches-vmsa.asl" in
          List.fold_right
            (fun d k ->
               match ASTUtils.identifier_of_decl d  with
               | "AArch64_TranslateAddress" -> k
               | _ -> d::k)
            patches patches_kvm
        else patches
      and custom_implems =
        let physmem = (* Final memory read and write *)
          let name =
            if is_kvm then "physmem-vmsa.asl"
            else "physmem-std.asl" in
          build `ASLv1 name in
        let impls =
          build `ASLv1 "implementations.asl"
          @ build `ASLv0 "implementations0.asl" in
        let impls =  impls @ physmem in
        if is_kvm then
          let impls_kvm =  build `ASLv1 "implementations-vmsa.asl" in
          impls @ impls_kvm
        else impls
      and shared = build `ASLv0 "shared_pseudocode.asl" in
      let shared =
        (*
         * Remove from shared pseudocode the functions declared in stdlib because:
         * 1. it avoids name clashes at type-checking time;
         * 2. when debugging, we know what function is called;
         * 3. stdlib functions usually out-perform their shared-pseudocode
         *    counterparts when executed in herd.
         *)
        let filter d =
          let open AST in
          match[@warning "-42"] d.desc with
          | D_Func { name; body = _; _ } ->
              let should_remove =
                Asllib.Builder.is_stdlib_name name || is_primitive name
              in
              let () =
                if false && should_remove then
                  Printf.eprintf "Subprogram %s removed from shared\n%!" name
              in
              not should_remove
          | _ -> true
        in
        List.filter filter shared
      in
      let ( @! ) = List.rev_append in
      let ast = patch ~patches:(custom_implems @! patches) ~src:shared in
      ast |> Asllib.Builder.with_stdlib
      |> Asllib.Builder.with_primitives extra_funcs
      |> TypeCheck.type_check_ast

    let typed_shared_pseudocode : unit -> AST.t * Asllib.StaticEnv.global =
      let if_asl_aarch64 = Lazy.from_fun build_shared_pseudocode
      and otherwise =
        lazy
          (Lazy.force Asllib.Builder.stdlib
          |> Asllib.Builder.with_primitives extra_funcs
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
        let v_to_int = v_to_int
        let bind_data = M.asl_data
        let bind_seq = M.asl_seq
        let bind_ctrl = M.asl_ctrl
        let prod_par = M.( >>| )
        let appl_data m f = m >>= fun v -> return (f v)
        let debugT = M.debugT
        let commit = commit ii_env
        let choice = choice
        let choice_debug (pp: unit -> unit)
              (m1 : V.v M.t) (m2 : 'b M.t) (m3 : 'b M.t) : 'b M.t =
          M.asl_data
            m1
            (function
             | V.Val (Constant.Concrete (ASLScalar.S_Bool b)) ->
                if b then m2 else m3
             | b ->
                let () = pp () in
                M.asl_data
                  (to_int_signed b)
                  (fun v -> M.choiceT v m2 m3))
        let delay m k = M.delay_kont "ASL" m k
        let failT e v = M.failT e v
        let return = M.unitT
        let warnT = M.warnT
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
        let primitives = make_extra_funcs ii_env
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
      let env =
        A.state_fold
          (fun loc v env ->
            match loc with
            | A.Location_reg (_, ASLLocalId (Scope.Global _, name)) ->
                (name, v) :: env
            | _ -> env)
          t.Test_herd.init_state []
      in
      let exec () = ASLInterpreter.run_typed_env env tenv ast in
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
