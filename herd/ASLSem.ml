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

module AST = Asllib.AST

module Make (C : Sem.Config) = struct
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

  let barriers = []
  let isync = None
  let atomic_pair_allowed _ _ = true

  module Mixed (SZ : ByteSize.S) = struct
    module Mixed = M.Mixed (SZ)

    let ( let* ) = M.( >>= )
    let ( and* ) = M.( >>| )
    let return = M.unitT
    let ( >>= ) = M.( >>= )

    let big_op op default =
      let folder m1 m2 =
        let* v1 = m1 and* v2 = m2 in
        op v1 v2
      in
      function [] -> default | h :: t -> List.fold_left folder h t

    let big_or = big_op (M.op Op.Or) (return V.one)

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

    let v_to_parsed_v = function
      | V.Val (Constant.Concrete s) -> ASLScalar.to_native_value s
      | v ->
          Warn.fatal "Cannot convert value %s into a native value." (V.pp_v v)

    let v_of_parsed_v =
      let open AST in
      let open ASLScalar in
      let concrete v = Constant.Concrete v in
      let vector li = Constant.ConcreteVector li in
      let rec tr = function
        | V_Int i -> S_Int (Int64.of_int i) |> concrete
        | V_Bool b -> S_Bool b |> concrete
        | V_BitVector bv -> S_BitVector bv |> concrete
        | V_Tuple li -> List.map tr li |> vector
        | V_Record li -> List.map (fun (_, v) -> tr v) li |> vector
        | V_Exception li -> List.map (fun (_, v) -> tr v) li |> vector
        | V_Real _f -> Warn.fatal "Cannot use reals yet."
      in
      fun v -> V.Val (tr v)

    let v_to_int = function
      | V.Val (Constant.Concrete (ASLScalar.S_Int i)) -> Some (Int64.to_int i)
      | _ -> None

    let v_as_int = function
      | V.Val (Constant.Concrete i) -> V.Cst.Scalar.to_int i
      | v -> Warn.fatal "Cannot concretise symbolic value: %s" (V.pp_v v)

    let v_to_bv = function
      | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)) -> bv
      | v -> Warn.fatal "Cannot construct a bitvector out of %s." (V.pp_v v)

    let datasize_to_machsize v =
      match v_as_int v with
      | 32 -> MachSize.Word
      | 64 -> MachSize.Quad
      | 128 -> MachSize.S128
      | _ -> Warn.fatal "Cannot access a register with size %s" (V.pp_v v)

    (**************************************************************************)
    (* Special monad interations                                              *)
    (**************************************************************************)

    let resize_from_quad = function
      | MachSize.Quad -> return
      | sz -> (
          function
          | V.Val (Constant.Symbolic _) as v -> return v
          | v -> M.op1 (Op.Mask sz) v)

    let write_loc sz loc v ii =
      let* resized_v = resize_from_quad sz v in
      let mk_action loc' = Act.Access (Dir.W, loc', resized_v, sz) in
      M.write_loc mk_action loc ii

    let read_loc sz is_data loc ii =
      let mk_action loc' v' = Act.Access (Dir.R, loc', v', sz) in
      let* v = M.read_loc is_data mk_action loc ii in
      resize_from_quad sz v

    let loc_of_scoped_id ii x scope =
      A.Location_reg (ii.A.proc, ASLBase.ASLLocalId (scope, x))

    let loc_of_arch_reg ii r = A.Location_reg (ii.A.proc, ASLBase.ArchReg r)

    let read_arch_reg sz is_data r ii =
      read_loc sz is_data (loc_of_arch_reg ii r) ii

    let write_arch_reg sz r v ii = write_loc sz (loc_of_arch_reg ii r) v ii

    (**************************************************************************)
    (* ASL-Backend implementation                                             *)
    (**************************************************************************)

    let choice m1 m2 m3 =
      M.( >>*= ) m1 (function
        | V.Val (Constant.Concrete (ASLScalar.S_Bool b)) -> if b then m2 else m3
        | b ->
            let* v = M.op1 (Op.ArchOp1 ASLValue.ASLArchOp.ToInt) b in
            M.choiceT v m2 m3)

    let binop op =
      let open AST in
      let to_bool op v1 v2 =
        let* v = op v1 v2 in
        M.op1 (Op.ArchOp1 ASLValue.ASLArchOp.ToBool) v
      in
      match op with
      | AND -> M.op Op.And
      | BAND -> M.op Op.And
      | BEQ -> M.op Op.Eq |> to_bool
      | BOR -> M.op Op.Or
      | DIV -> M.op Op.Div
      | EOR -> M.op Op.Xor
      | EQ_OP -> M.op Op.Eq |> to_bool
      | GT -> M.op Op.Gt |> to_bool
      | GEQ -> M.op Op.Ge |> to_bool
      | LT -> M.op Op.Lt |> to_bool
      | LEQ -> M.op Op.Le |> to_bool
      | MINUS -> M.op Op.Sub
      | MUL -> M.op Op.Mul
      | NEQ -> M.op Op.Ne |> to_bool
      | OR -> M.op Op.Or
      | PLUS -> M.op Op.Add
      | SHL -> M.op Op.ShiftLeft
      | SHR -> M.op Op.ShiftRight
      | IMPL | MOD | RDIV -> Warn.fatal "Not yet implemented operation."

    let unop op =
      let open AST in
      match op with
      | BNOT -> (
          function
          | V.Val (Constant.Concrete (ASLScalar.S_Bool b)) ->
              V.Val (Constant.Concrete (ASLScalar.S_Bool (not b))) |> return
          | v -> M.op1 Op.Not v >>= M.op1 (Op.ArchOp1 ASLValue.ASLArchOp.ToBool)
          )
      | NEG -> M.op Op.Sub V.zero
      | NOT -> M.op1 Op.Inv

    let on_write_identifier (ii, poi) x scope v =
      let loc = loc_of_scoped_id ii x scope in
      let action = Act.Access (Dir.W, loc, v, MachSize.Quad) in
      M.mk_singleton_es action (use_ii_with_poi ii poi)

    let on_read_identifier (ii, poi) x scope v =
      let loc = loc_of_scoped_id ii x scope in
      let action = Act.Access (Dir.R, loc, v, MachSize.Quad) in
      M.mk_singleton_es action (use_ii_with_poi ii poi)

    let create_vector _ty li =
      let li = List.map as_constant li in
      return (V.Val (Constant.ConcreteVector li))

    let get_i i = function
      | V.Val (Constant.ConcreteVector li) as v -> (
          match List.nth_opt li i with
          | None ->
              Warn.user_error "Index %d out of bounds for value %s" i (V.pp_v v)
          | Some v -> return (V.Val v))
      | v -> Warn.user_error "Trying to index non-indexable value %s" (V.pp_v v)

    let list_update i v li =
      let rec aux acc i li =
        match (li, i) with
        | [], _ -> None
        | _ :: t, 0 -> Some (List.rev_append acc (v :: t))
        | h :: t, i -> aux (h :: acc) (i - 1) t
      in
      aux [] i li

    let set_i i v vec =
      match (vec, v) with
      | V.Val (Constant.ConcreteVector li), V.Val c -> (
          match list_update i c li with
          | None ->
              Warn.user_error "Index %d out of bounds for value %s" i
                (V.pp_v vec)
          | Some li -> return (V.Val (Constant.ConcreteVector li)))
      | _ ->
          Warn.user_error "Trying to index non-indexable value %s" (V.pp_v vec)

    let read_from_bitvector positions bvs =
      let positions = Asllib.ASTUtils.slices_to_positions v_as_int positions in
      let arch_op1 = ASLValue.ASLArchOp.BVSlice positions in
      M.op1 (Op.ArchOp1 arch_op1) bvs

    let write_to_bitvector positions w v =
      let bv_src, bv_dst =
        match (w, v) with
        | ( V.Val (Constant.Concrete (ASLScalar.S_BitVector bv_src)),
            V.Val (Constant.Concrete (ASLScalar.S_BitVector bv_dst)) ) ->
            (bv_src, bv_dst)
        | _ -> Warn.fatal "Not yet implemented: writing to symbolic bitvector"
      in
      let positions = Asllib.ASTUtils.slices_to_positions v_as_int positions in
      let bv_res = Asllib.Bitvector.write_slice bv_dst bv_src positions in
      return (V.Val (Constant.Concrete (ASLScalar.S_BitVector bv_res)))

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
      | [ x ] -> return x
      | [ V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)); V.Var x ]
        when Asllib.Bitvector.to_int bv = 0 ->
          return (V.Var x)
      | bvs ->
          let as_concrete_bv = function
            | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)) -> bv
            | _ ->
                Warn.fatal
                  "Not yet implemented: concatenating symbolic bitvectors: \
                   [%s]."
                  (String.concat "," (List.map V.pp_v bvs))
          in
          let bv_res = Asllib.Bitvector.concat (List.map as_concrete_bv bvs) in
          return (V.Val (Constant.Concrete (ASLScalar.S_BitVector bv_res)))

    (**************************************************************************)
    (* Primitives and helpers                                                 *)
    (**************************************************************************)

    let virtual_to_loc_reg rv ii =
      let i = v_as_int rv in
      if i >= List.length AArch64Base.gprs || i < 0 then
        Warn.fatal "Invalid register number: %d" i
      else
        let arch_reg = AArch64Base.Ireg (List.nth AArch64Base.gprs i) in
        A.Location_reg (ii.A.proc, ASLBase.ArchReg arch_reg)

    let read_register (ii, poi) rval =
      let loc = virtual_to_loc_reg rval ii in
      let* v = read_loc MachSize.Quad true loc (use_ii_with_poi ii poi) in
      return [ v ]

    let write_register (ii, poi) v rval =
      let loc = virtual_to_loc_reg rval ii in
      let* () = write_loc MachSize.Quad loc v (use_ii_with_poi ii poi) in
      return []

    let read_memory (ii, poi) addr datasize =
      let sz = datasize_to_machsize datasize in
      let* v =
        read_loc sz true (A.Location_global addr) (use_ii_with_poi ii poi)
      in
      return [ v ]

    let write_memory (ii, poi) addr datasize value =
      let sz = datasize_to_machsize datasize in
      let* () =
        write_loc sz (A.Location_global addr) value (use_ii_with_poi ii poi)
      in
      return []

    let default_ProcState_fields =
      let zeros length =
        Constant.Concrete
          (ASLScalar.S_BitVector (Asllib.Bitvector.zeros length))
      and zero =
        Constant.Concrete (ASLScalar.S_BitVector Asllib.Bitvector.zero)
      in
      [
        ("D", zero);
        ("A", zero);
        ("I", zero);
        ("F", zero);
        ("EXLOCK", zero);
        ("PAN", zero);
        ("UAO", zero);
        ("DIT", zero);
        ("TCO", zero);
        ("PM", zero);
        ("PPEND", zero);
        ("BTYPE", zeros 2);
        ("ZA", zero);
        ("SM", zero);
        ("ALLINT", zero);
        ("SS", zero);
        ("IL", zero);
        ("EL", zeros 2);
        ("nRW", zero);
        ("SP", zero);
        ("Q", zero);
        ("GE", zeros 4);
        ("SSBS", zero);
        ("IT", zeros 8);
        ("J", zero);
        ("T", zero);
        ("E", zero);
        ("M", zeros 5);
      ]

    let read_pstate_nzcv (ii, poi) () =
      let to_cst bv = Constant.Concrete (ASLScalar.S_BitVector bv) |> return in
      let to_bit v =
        M.choiceT v (to_cst Asllib.Bitvector.one) (to_cst Asllib.Bitvector.zero)
      in
      let loc = A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.NZCV) in
      let* v = read_loc MachSize.Quad true loc (use_ii_with_poi ii poi) in
      let v = match v with V.Val _ -> v | _ -> V.zero in
      let* n = M.op1 (Op.ReadBit 3) v >>= to_bit
      and* z = M.op1 (Op.ReadBit 2) v >>= to_bit
      and* c = M.op1 (Op.ReadBit 1) v >>= to_bit
      and* v = M.op1 (Op.ReadBit 0) v >>= to_bit in
      let fields =
        ("N", n) :: ("Z", z) :: ("C", c) :: ("V", v) :: default_ProcState_fields
        |> Asllib.ASTUtils.canonical_fields |> List.map snd
      in
      return [ V.Val (Constant.ConcreteVector fields) ]

    let write_pstate_nzcv (ii, poi) v =
      let loc = A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.NZCV) in
      let* () = write_loc MachSize.Quad loc v (use_ii_with_poi ii poi) in
      return []

    let replicate bv v =
      let return_bv res =
        return [ V.Val (Constant.Concrete (ASLScalar.S_BitVector res)) ]
      in
      match v_as_int v with
      | 0 -> Asllib.Bitvector.of_string "" |> return_bv
      | 1 -> return [ bv ]
      | i ->
          let bv = v_to_bv bv in
          List.init i (Fun.const bv) |> Asllib.Bitvector.concat |> return_bv

    let uint bv =
      let* res = M.op1 (Op.ArchOp1 ASLValue.ASLArchOp.ToInt) bv in
      return [ res ]

    let sint bv =
      let* nbv = M.op1 Op.Not bv in
      let* ni = M.op1 (Op.ArchOp1 ASLValue.ASLArchOp.ToInt) nbv in
      let* res = M.op1 (Op.AddK 1) ni in
      return [ res ]

    (**************************************************************************)
    (* ASL environment                                                        *)
    (**************************************************************************)

    (* Helpers *)
    let build_primitive name args return_type body =
      let parameters =
        let open AST in
        let get_param t =
          match t.desc with
          | T_Bits (BitWidth_Determined { desc = E_Var x; _ }, _) ->
              Some (x, None)
          | _ -> None
        in
        args |> List.filter_map get_param (* |> List.sort String.compare *)
      and args =
        List.mapi (fun i arg_ty -> ("arg_" ^ string_of_int i, arg_ty)) args
      in
      AST.{ name; args; body; return_type; parameters }

    let arity_zero name return_type f =
      build_primitive name [] return_type @@ function
      | [] -> f ()
      | _ :: _ -> Warn.fatal "Arity error for function %s." name

    let arity_one name args return_type f =
      build_primitive name args return_type @@ function
      | [ x ] -> f x
      | [] | _ :: _ :: _ -> Warn.fatal "Arity error for function %s." name

    let arity_two name args return_type f =
      build_primitive name args return_type @@ function
      | [ x; y ] -> f x y
      | [] | [ _ ] | _ :: _ :: _ :: _ ->
          Warn.fatal "Arity error for function %s." name

    let arity_three name arg_types return_type f =
      build_primitive name arg_types return_type @@ function
      | [ x; y; z ] -> f x y z
      | _ -> Warn.fatal "Arity error for function %s." name

    (* Primitives *)
    let extra_funcs ii_env =
      let open AST in
      let with_pos = Asllib.ASTUtils.add_dummy_pos in
      let d = T_Int None |> with_pos in
      let reg = T_Int None |> with_pos in
      let var x = E_Var x |> with_pos in
      let lit x = E_Literal (V_Int x) |> with_pos in
      let bv x = T_Bits (BitWidth_Determined x, None) |> with_pos in
      let bv_var x = bv @@ var x in
      let bv_lit x = bv @@ lit x in
      let mul x y = E_Binop (MUL, x, y) |> with_pos in
      let t_named x = T_Named x |> with_pos in
      let getter = Asllib.ASTUtils.getter_name in
      let setter = Asllib.ASTUtils.setter_name in
      [
        arity_one (getter "_R") [ reg ]
          (Some (bv_lit 64))
          (read_register ii_env);
        arity_two (setter "_R") [ bv_lit 64; reg ] None (write_register ii_env);
        arity_two "read_memory" [ reg; d ] (Some d) (read_memory ii_env);
        arity_three "write_memory" [ reg; d; d ] None (write_memory ii_env);
        arity_zero (getter "PSTATE")
          (Some (t_named "ProcState"))
          (read_pstate_nzcv ii_env);
        arity_one (setter "PSTATE")
          [ t_named "ProcState" ]
          None (write_pstate_nzcv ii_env);
        arity_two "Replicate"
          [ bv_var "N"; d ]
          (Some (bv (mul (var "N") (var "arg_1"))))
          replicate;
        arity_one "UInt" [ bv_var "N" ] (Some d) uint;
        arity_one "SInt" [ bv_var "N" ] (Some d) uint;
      ]

    (**************************************************************************)
    (* Execution                                                              *)
    (**************************************************************************)

    let build_semantics _t ii =
      let ii_env = (ii, ref ii.A.program_order_index) in
      let module ASLBackend = struct
        type value = V.v
        type 'a m = 'a M.t
        type scope = string * int

        let debug_value = V.pp_v
        let v_of_int = V.intToV
        let v_of_parsed_v = v_of_parsed_v
        let v_to_int = v_to_int
        let bind_data = M.( >>= )
        let bind_seq = M.cseq
        let prod = M.( >>| )
        let choice = choice
        let return = M.unitT
        let on_write_identifier = on_write_identifier ii_env
        let on_read_identifier = on_read_identifier ii_env
        let binop = binop
        let unop = unop
        let create_vector = create_vector
        let get_i = get_i
        let set_i = set_i
        let read_from_bitvector = read_from_bitvector
        let write_to_bitvector = write_to_bitvector
        let concat_bitvectors = concat_bitvectors
      end in
      let module ASLInterpreter = Asllib.Interpreter.Make (ASLBackend) in
      let exec () = ASLInterpreter.run ii.A.inst (extra_funcs ii_env) in
      let* _ =
        match Asllib.Error.intercept exec () with
        | Ok m -> m
        | Error err -> Asllib.Error.error_to_string err |> Warn.fatal "%s"
      in
      M.addT !(snd ii_env) B.nextT

    let spurious_setaf _ = assert false
  end
end
