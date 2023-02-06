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
      | V.Val (Constant.Concrete i) -> V.Cst.Scalar.to_int i
      | v -> Warn.fatal "Cannot concretise symbolic value: %s" (V.pp_v v)

    let datasize_to_machsize v =
      match v_to_int v with
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
      M.( >>*= ) m1 (fun b ->
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
      | BNOT -> M.op1 Op.Inv
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
      let bv_res = Asllib.Bitvector.write_slice bv_dst bv_src positions in
      return (V.Val (Constant.Concrete (ASLScalar.S_BitVector bv_res)))

    let concat_bitvectors bvs =
      let as_concrete_bv = function
        | V.Val (Constant.Concrete (ASLScalar.S_BitVector bv)) -> bv
        | _ ->
            Warn.fatal "Not yet implemented: concatenating symbolic bitvectors."
      in
      let bv_res = Asllib.Bitvector.concat (List.map as_concrete_bv bvs) in
      return (V.Val (Constant.Concrete (ASLScalar.S_BitVector bv_res)))

    (**************************************************************************)
    (* Primitives and helpers                                                 *)
    (**************************************************************************)

    let virtual_to_loc_reg rv ii =
      let i = v_to_int rv in
      let arch_reg = AArch64Base.Ireg (List.nth AArch64Base.gprs (i - 1)) in
      A.Location_reg (ii.A.proc, ASLBase.ArchReg arch_reg)

    let read_register (ii, poi) rval datasize =
      let loc = virtual_to_loc_reg rval ii in
      let sz = datasize_to_machsize datasize in
      let* v = read_loc sz true loc (use_ii_with_poi ii poi) in
      return [ v ]

    let write_register (ii, poi) rval datasize v =
      let loc = virtual_to_loc_reg rval ii in
      let sz = datasize_to_machsize datasize in
      let* () = write_loc sz loc v (use_ii_with_poi ii poi) in
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

    let read_pstate_nzcv (ii, poi) () =
      let loc = A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.NZCV) in
      let* v = read_loc MachSize.Quad true loc (use_ii_with_poi ii poi) in
      return [ v ]

    let write_pstate_nzcv (ii, poi) v =
      let loc = A.Location_reg (ii.A.proc, ASLBase.ArchReg AArch64Base.NZCV) in
      let* () = write_loc MachSize.Quad loc v (use_ii_with_poi ii poi) in
      return []

    (**************************************************************************)
    (* ASL environment                                                        *)
    (**************************************************************************)

    (* Helpers *)
    let build_primitive name args return_type body =
      AST.{ name; args; body; return_type }

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
      let data = T_Int None |> with_pos in
      [
        arity_two "read_register" [ reg; d ] (Some data) (read_register ii_env);
        arity_three "write_register" [ reg; d; reg ] None
          (write_register ii_env);
        arity_two "read_memory" [ reg; d ] (Some data) (read_memory ii_env);
        arity_three "write_memory" [ reg; d; data ] None (write_memory ii_env);
        arity_zero "read_pstate_nzcv" (Some data) (read_pstate_nzcv ii_env);
        arity_one "write_pstate_nzcv" [ data ] None (write_pstate_nzcv ii_env);
      ]

    (* Main function arguments *)
    let main_env (ii, _poi) =
      let with_pos = Asllib.ASTUtils.add_dummy_pos in
      let assign x v =
        let open Asllib.AST in
        with_pos
          (S_Assign (with_pos (LE_Var x), with_pos (E_Literal (v_to_parsed_v v))))
      in
      let folder reg v acc =
        let open ASLBase in
        match reg with
        | ASLLocalId (scope, x) ->
            if scope_equal main_scope scope then assign x v :: acc else acc
        | _ -> acc
      in
      let to_assign = A.fold_reg_state folder ii.A.env.A.regs [] in
      Asllib.ASTUtils.stmt_from_list to_assign

    let rec list_remove_opt f acc = function
      | [] -> None
      | h :: t ->
          if f h then Some (h, List.rev_append acc t)
          else list_remove_opt f (h :: acc) t

    let add_main_env_to_ast (ii, _poi) =
      let open AST in
      match main_env (ii, _poi) with
      | { desc = S_Pass; _ } -> ii.A.inst
      | s -> (
          let is_main = function
            | D_Func f -> String.equal "main" f.name
            | _ -> false
          in
          match list_remove_opt is_main [] ii.A.inst with
          | Some (D_Func f, ast) ->
              let body =
                Asllib.ASTUtils.add_pos_from f.body (S_Then (s, f.body))
              in
              let main_func = D_Func { f with body } in
              main_func :: ast
          | _ -> ii.A.inst)

    (**************************************************************************)
    (* Execution                                                              *)
    (**************************************************************************)

    let build_semantics _t ii =
      let ii_env = (ii, ref ii.A.program_order_index) in
      let module ASLBackend = struct
        type value = V.v
        type 'a m = 'a M.t
        type scope = string * int

        let v_of_int = V.intToV
        let v_of_parsed_v = v_of_parsed_v
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
      let ast = add_main_env_to_ast ii_env in
      let exec () = ASLInterpreter.run ast (extra_funcs ii_env) in
      let* _ =
        match Asllib.Error.intercept exec () with
        | Ok m -> m
        | Error err -> Asllib.Error.error_to_string err |> Warn.fatal "%s"
      in
      M.addT !(snd ii_env) B.nextT

    let spurious_setaf _ = assert false
  end
end
