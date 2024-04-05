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

module type Config = sig
  include Sem.Config

  val libfind : string -> string
end

module Make (C : Config) = struct
  module V = ASLValue.V

  module ConfLoc = struct
    include SemExtra.ConfigToArchConfig (C)

    let default_to_symb = C.variant Variant.ASL
  end

  module ASL64AH = struct
    include GenericArch_herd.Make (ASLBase) (ConfLoc) (V)
    include ASLBase

    let opt_env = true
  end

  module Act = ASLAction.Make (ASL64AH)
  include SemExtra.Make (C) (ASL64AH) (Act)

  let is_experimental = C.variant Variant.ASLExperimental
  let barriers = []
  let isync = None
  let atomic_pair_allowed _ _ = true
  let aneutral = AArch64Annot.N

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

    let as_constant = function
      | V.Val c -> c
      | V.Var _ as v ->
          Warn.fatal "Cannot convert value %s into constant" (V.pp_v v)

    let v_unknown_of_type _t = V.fresh_var ()

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
      in
      fun v -> V.Val (tr v)

    let v_to_int = function
      | V.Val (Constant.Concrete (ASLScalar.S_Int i)) -> Some (Z.to_int i)
      | _ -> None

    let v_as_int = function
      | V.Val (Constant.Concrete i) -> V.Cst.Scalar.to_int i
      | v -> Warn.fatal "Cannot concretise symbolic value: %s" (V.pp_v v)

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
      | _ -> Warn.fatal "Cannot access a register with size %s" (V.pp_v v)

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

    let wrap_op1_symb_as_var op1 = function
      | V.Val (Constant.Symbolic _) as v ->
          let v' = V.fresh_var () in
          M.restrict M.VC.[ Assign (v', Unop (op1, v)) ] >>! v'
      | v -> M.op1 op1 v

    let to_bv = wrap_op1_symb_as_var (Op.ArchOp1 ASLOp.ToBV)
    let to_int_unsigned = wrap_op1_symb_as_var (Op.ArchOp1 ASLOp.ToIntU)
    let to_int_signed = wrap_op1_symb_as_var (Op.ArchOp1 ASLOp.ToIntS)

    (**************************************************************************)
    (* Special monad interations                                              *)
    (**************************************************************************)

    let create_barrier b ii = M.mk_singleton_es (Act.Barrier b) ii >>! []

    let resize_from_quad = function
      | MachSize.Quad -> return
      | sz -> (
          function
          | V.Val (Constant.Symbolic _) as v -> return v
          | v -> M.op1 (Op.Mask sz) v)

    let write_loc sz loc v a ii =
      let* resized_v = resize_from_quad sz v in
      let mk_action loc' = Act.Access (Dir.W, loc', resized_v, sz, a) in
      M.write_loc mk_action loc ii

    let read_loc sz loc a ii =
      let mk_action loc' v' = Act.Access (Dir.R, loc', v', sz, a) in
      let* v = M.read_loc false mk_action loc ii in
      resize_from_quad sz v >>= to_bv

    (**************************************************************************)
    (* ASL-Backend implementation                                             *)
    (**************************************************************************)

    let commit (ii,poi) msg =
      M.mk_singleton_es (Act.Branching msg) (use_ii_with_poi ii poi)

    let choice (m1 : V.v M.t) (m2 : 'b M.t) (m3 : 'b M.t) : 'b M.t =
      M.asl_data
        m1
        (function
         | V.Val (Constant.Concrete (ASLScalar.S_Bool b)) ->
            if b then m2 else m3
         | b -> M.asl_data (to_int_signed b) (fun v -> M.choiceT v m2 m3))

    let binop =
      let open AST in
      let to_bool op v1 v2 = op v1 v2 >>= M.op1 (Op.ArchOp1 ASLOp.ToBool) in
      let to_bv op v1 v2 = op v1 v2 >>= to_bv in
      let or_ v1 v2 =
        match (v1, v2) with
        | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)), v
          when Asllib.Bitvector.is_zeros bv ->
            return v
        | v, V.Val (Constant.Concrete (ASLScalar.S_BitVector bv))
          when Asllib.Bitvector.is_zeros bv ->
            return v
        | _ -> to_bv (M.op Op.Or) v1 v2
      in
      function
      | AND -> M.op Op.And |> to_bv
      | BAND -> M.op Op.And
      | BEQ -> M.op Op.Eq |> to_bool
      | BOR -> M.op Op.Or
      | DIV -> M.op Op.Div
      | MOD -> M.op Op.Rem
      | DIVRM -> M.op (Op.ArchOp ASLOp.Divrm)
      | EOR -> M.op Op.Xor |> to_bv
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
          | "_NZCV", AST.Scope_Global false -> true
          | _ -> false
      else
        fun x scope ->
          match (x, scope) with
          | "PSTATE", AST.Scope_Global false -> true
          | _ -> false

    let is_resaddr x scope =
      match (x, scope) with "RESADDR", AST.Scope_Global false -> true | _ -> false

    let loc_of_scoped_id ii x scope =
      if is_nzcv x scope then
        A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.NZCV)
      else if is_resaddr x scope then
        A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.ResAddr)
      else A.Location_reg (ii.A.proc, ASLBase.ASLLocalId (scope, x))

    let on_access_identifier dir (ii, poi) x scope v =
      let loc = loc_of_scoped_id ii x scope in
      let m v =
        let action = Act.Access (dir, loc, v, MachSize.Quad, aneutral) in
        M.mk_singleton_es action (use_ii_with_poi ii poi)
      in
      if is_nzcv x scope then M.op1 (Op.ArchOp1 ASLOp.ToIntU) v >>= m else m v

    let on_write_identifier = on_access_identifier Dir.W
    and on_read_identifier = on_access_identifier Dir.R

    let create_vector li =
      let li = List.map as_constant li in
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

    type primitive_t = V.v M.t list -> V.v M.t list M.t

    let vbool b = V.Val (Constant.Concrete (ASLScalar.S_Bool b))

    (*
     * Add equation, the effect will be silent
     * discard of executon candidate if a
     * contradiction appears.
     *)

    let checkprop m_prop =
      let* vprop = m_prop in
      M.assign (vbool true) vprop >>! []

    (*
     * Split the current execution candidate into two:
     * one candidate, has variable [v] value to be TRUE,
     * while the value is FALSE for the other.
     *)

    let somebool _ =
      let v = V.fresh_var () in
      let mbool b = M.unitT (vbool b) in
      choice (M.unitT v) (mbool true) (mbool false)

    (*
     * Primitives that generate fence events.
     * Notice that ASL fence events take
     * an AArch64 barrier as argument.
     *)

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
     * Prinitives for read and write events.
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
      read_loc MachSize.Quad loc aneutral (use_ii_with_poi ii poi)

    let write_register (ii, poi) r_m v_m =
      let* v = v_m >>= to_int_signed and* r = r_m in
      let loc = virtual_to_loc_reg r ii in
      write_loc MachSize.Quad loc v aneutral (use_ii_with_poi ii poi) >>! []

    let loc_pc ii = A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.PC)

    let read_pc (ii,poi) () =
      read_loc MachSize.Quad (loc_pc ii) aneutral (use_ii_with_poi ii poi)

    let write_pc (ii,poi) v_m =
      let* v = v_m >>= to_int_unsigned in
      write_loc MachSize.Quad (loc_pc ii)
        v aneutral (use_ii_with_poi ii poi) >>! []

    let do_read_memory (ii, poi) addr_m datasize_m an =
      let* addr = addr_m and* datasize = datasize_m in
      let sz = datasize_to_machsize datasize in
      read_loc sz (A.Location_global addr) an (use_ii_with_poi ii poi)

    let read_memory ii addr_m datasize_m =
      do_read_memory ii addr_m datasize_m aneutral

    let read_memory_gen ii addr_m datasize_m accdesc_m =
      let* accdesc = accdesc_m in
      do_read_memory ii addr_m datasize_m (accdesc_to_annot true accdesc)

    let do_write_memory (ii, poi) addr_m datasize_m value_m an =
      let value_m = M.as_data_port value_m in
      let* addr = addr_m and* datasize = datasize_m and* value = value_m in
      let sz = datasize_to_machsize datasize in
      write_loc sz (A.Location_global addr) value an (use_ii_with_poi ii poi)
      >>! []

    let write_memory ii addr_m datasize_m value_m =
      do_write_memory ii addr_m datasize_m value_m AArch64Annot.N

    let write_memory_gen ii addr_m datasize_m value_m accdesc_m =
      let* accdesc = accdesc_m in
      do_write_memory ii addr_m datasize_m value_m
        (accdesc_to_annot false accdesc)

    let loc_sp ii = A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.SP)

    let read_sp (ii, poi) () =
      read_loc MachSize.Quad (loc_sp ii) aneutral (use_ii_with_poi ii poi)

    let write_sp (ii, poi) v_m =
      let* v = v_m >>= to_int_signed in
      write_loc MachSize.Quad (loc_sp ii) v aneutral (use_ii_with_poi ii poi)
      >>! []

    let uint bv_m = bv_m >>= to_int_unsigned
    let sint bv_m = bv_m >>= to_int_signed
    let processor_id (ii, _poi) () = return (V.intToV ii.A.proc)

    let can_predict_from v_m w_m =
      let diff_case = v_m in
      let eq_case = M.altT v_m w_m in
      let*| v = v_m and* w = w_m in
      let*| c = M.op Op.Eq v w in
      M.choiceT c eq_case diff_case

    (**************************************************************************)
    (* ASL environment                                                        *)
    (**************************************************************************)

    (* Helpers *)
    let build_primitive ?(args = []) ?returns ?(parameters = []) name f :
        AST.func * primitive_t =
      let open AST in
      let subprogram_type =
        match returns with None -> ST_Procedure | _ -> ST_Function
      and body = SB_Primitive
      and return_type = returns in
      ( { name; args; body; return_type; parameters; subprogram_type }
        [@warning "-40-42"],
        f )

    (* The function [pX] is building primitives with arity X and no return value *)
    (* The function [pXr] is building primitives with arity X and a return value *)

    (** Build a primitive with arity 0 and no return value. *)
    let p0 name ?parameters f =
      let f = function
        | [] -> f ()
        | _ :: _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?parameters name f

    (** Build a primitive with arity 0 and a return value. *)
    let p0r name ~returns f =
      let f = function
        | [] -> return [ f () ]
        | _ :: _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?returns:(Some returns) name f

    (** Build a primitive with arity 1 and no return value. *)
    let p1 name arg ?parameters f =
      let f = function
        | [ v ] -> f v
        | [] | _ :: _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ~args:[ arg ] ?parameters name f

    (** Build a primitive with arity 1 and a return value. *)
    let p1r name arg ~returns ?parameters f =
      let f = function
        | [ v ] -> return [ f v ]
        | [] | _ :: _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?returns:(Some returns) ~args:[ arg ] ?parameters name f

    (** Build a primitive with arity 2 and no return value. *)
    let p2 name arg1 arg2 ?parameters f =
      let f = function
        | [ v1; v2 ] -> f v1 v2
        | _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ~args:[ arg1; arg2 ] ?parameters name f

    (** Build a primitive with arity 2 and a return value. *)
    let p2r name arg1 arg2 ~returns ?parameters f =
      let f = function
        | [ v1; v2 ] -> return [ f v1 v2 ]
        | _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?returns:(Some returns) ~args:[ arg1; arg2 ] ?parameters
        name f

    (** Build a primitive with arity 3 and no return value. *)
    let p3 name arg1 arg2 arg3 ?parameters f =
      let f = function
        | [ v1; v2; v3 ] -> f v1 v2 v3
        | _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ~args:[ arg1; arg2; arg3 ] ?parameters name f

    (** Build a primitive with arity 3 and a return value. *)
    let p3r name arg1 arg2 arg3 ~returns ?parameters f =
      let f = function
        | [ v1; v2; v3 ] -> return [ f v1 v2 v3 ]
        | _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ?returns:(Some returns) ~args:[ arg1; arg2; arg3 ]
        ?parameters name f

    (** Build a primitive with arity 4 and no return value. *)
    let p4 name arg1 arg2 arg3 arg4 ?parameters f =
      let f = function
        | [ v1; v2; v3; v4 ] -> f v1 v2 v3 v4
        | _ -> Warn.fatal "Arity error for function %s." name
      in
      build_primitive ~args:[ arg1; arg2; arg3; arg4 ] ?parameters name f

    (* Primitives *)
    let extra_funcs ii_env =
      let open AST in
      let with_pos = Asllib.ASTUtils.add_dummy_pos in
      let integer = Asllib.ASTUtils.integer in
      let boolean = Asllib.ASTUtils.boolean in
      let reg = integer in
      let var x = E_Var x |> with_pos in
      let lit x = E_Literal (L_Int (Z.of_int x)) |> with_pos in
      let bv x = T_Bits (x, []) |> with_pos in
      let bv_var x = bv @@ var x in
      let bv_lit x = bv @@ lit x in
      let bv_64 = bv_lit 64 in
      let t_named x = T_Named x |> with_pos in
      [
(* Fences *)
        p0 "primitive_isb" (primitive_isb ii_env);
        p2 "primitive_dmb" ("d", integer) ("t", integer) (primitive_dmb ii_env);
        p2 "primitive_dsb" ("d", integer) ("t", integer) (primitive_dsb ii_env);
(* Registers *)
        p1r "read_register" ("reg", reg) ~returns:bv_64 (read_register ii_env);
        p2 "write_register" ("data", bv_64) ("reg", reg) (write_register ii_env);
        p0r "read_pc" ~returns:bv_64 (read_pc ii_env);
        p1 "write_pc" ("data", bv_64) (write_pc ii_env);
        p0r "SP_EL0" ~returns:bv_64 (read_sp ii_env);
        p1 "SP_EL0" ("data", bv_64) (write_sp ii_env);
(* Memory *)
        p2r "read_memory" ("addr", bv_64) ("size", integer) ~returns:bv_64
          (read_memory ii_env);
        p3r "read_memory_gen" ("addr", bv_64) ("size", integer)
          ("accdesc", t_named "AccessDescriptor")
          ~returns:bv_64 (read_memory_gen ii_env);
        p3 "write_memory" ("addr", bv_64) ("size", integer)
          ("data", bv_var "size")
          (write_memory ii_env);
        p4 "write_memory_gen" ("addr", bv_64) ("size", integer)
          ("data", bv_var "size")
          ("accdesc", t_named "AccessDescriptor")
          (write_memory_gen ii_env);
(* Translations *)
         p1r "UInt"
          ~parameters:[ ("N", None) ]
          ("x", bv_var "N")
          ~returns:integer uint;
        p1r "SInt"
          ~parameters:[ ("N", None) ]
          ("x", bv_var "N")
          ~returns:integer sint;
(* Misc *)
        p0r "ProcessorID" ~returns:integer (processor_id ii_env);
        p2r "CanPredictFrom"
          ~parameters:[ ("N", None) ]
          ("predicted", bv_var "N")
          ("from", bv_var "N")
          ~returns:(bv_var "N") can_predict_from;
        p0r "SomeBoolean" ~returns:boolean somebool;
        p1 "CheckProp" ("prop", boolean) checkprop;
      ]

    (**************************************************************************)
    (* Execution                                                              *)
    (**************************************************************************)

    let build_semantics t ii =
      let ii_env = (ii, ref ii.A.program_order_index) in
      let module ASLBackend = struct
        type value = V.v
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
        let delay m k = M.delay_kont "ASL" m k
        let return = M.unitT
        let warnT = M.warnT
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
        let primitives = extra_funcs ii_env
      end in
      let module Config = struct
        let type_checking_strictness : Asllib.Typing.strictness =
          if C.variant (Variant.ASLType `Warn) then `Warn
          else if C.variant (Variant.ASLType `TypeCheck) then `TypeCheck
          else `Silence

        let unroll =
          match C.unroll with None -> Opts.unroll_default `ASL | Some u -> u

        module Instr = Asllib.Instrumentation.SemanticsNoInstr
      end in
      let module ASLInterpreter = Asllib.Interpreter.Make (ASLBackend) (Config)
      in
      let ast =
        if not (C.variant Variant.ASL_AArch64) then ii.A.inst
        else
          let build ?ast_type version fname =
            Filename.concat "asl-pseudocode" fname
            |> C.libfind
            |> ASLBase.build_ast_from_file ?ast_type version
          in
          let patches =
            let patches = build `ASLv1 "patches.asl" in
            if is_experimental then
              (* Replace default "PSTATE" definition by experimental ones. *)
              let pstate =  build `ASLv1 "pstate-exp.asl" in
              List.fold_right
                (fun d k ->
                   match ASTUtils.identifier_of_decl d with
                   | "PSTATE" -> pstate @ k
                   | _ -> d::k)
                patches []
            else patches
          and custom_implems = build `ASLv1 "implementations.asl"
          and shared = build `ASLv0 "shared_pseudocode.asl" in
          let is_primitive =
            let open AST in
            let open ASTUtils in
            let set =
              List.fold_left
                (fun [@warning "-42"] acc ({ name; body = _; _ }, _) ->
                  ISet.add name acc)
                ISet.empty ASLBackend.primitives
            in
            fun name -> ISet.mem name set
          in
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
                      Printf.eprintf "Subprogram %s removed from shared\n%!"
                        name
                  in
                  not should_remove
              | _ -> true
            in
            List.filter filter shared
          in
          let ( @! ) = List.rev_append in
          let shared =
            Asllib.ASTUtils.patch
              ~patches:(custom_implems @! patches)
              ~src:shared
          in
          ii.A.inst @! shared
      in
      let () =
        if false then Format.eprintf "Completed AST: %a.@." Asllib.PP.pp_t ast
      in
      let env =
        A.state_fold
          (fun loc v env ->
            let open ASLBase in
            match loc with
            | A.Location_reg (_, ASLLocalId (AST.Scope_Global _, name)) ->
                (name, v) :: env
            | _ -> env)
          t.Test_herd.init_state []
      in
      let exec () = ASLInterpreter.run_env env ast in
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
