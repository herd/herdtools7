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

    let as_constant = function
      | V.Val c -> c
      | V.Var _ as v ->
          Warn.fatal "Cannot convert value %s into constant" (V.pp_v v)

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

    let parsed_ast_to_ast =
      let value_of_int i = V.intToV i in
      let value_of_bool b = if b then V.one else V.zero in
      let value_of_real _r = Warn.fatal "Cannot parse reals yet." in
      let value_of_string = V.stringToV in
      let value_of_tuple tr li =
        let li = List.map (fun v -> tr v |> as_constant) li in
        V.Val (Constant.ConcreteVector li)
      in
      let value_of_record tr li =
        let li = List.map (fun (_, v) -> tr v |> as_constant) li in
        V.Val (Constant.ConcreteVector li)
      in
      Asllib.ASTUtils.tr_values value_of_int value_of_bool value_of_real
        value_of_string value_of_tuple value_of_record value_of_record

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
      | sz -> M.op1 (Op.Mask sz)

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

    let choice m1 m2 m3 = M.( >>*= ) m1 (fun v -> M.choiceT v m2 m3)

    let binop op =
      let open AST in
      M.op
      @@
      match op with
      | AND -> Op.And
      | BAND -> Op.And (* TODO: convert to C style bool first? *)
      | BEQ -> Op.Eq (* TODO: convert to C style bool first? *)
      | BOR -> Op.Or (* TODO: convert to C style bool first? *)
      | DIV -> Op.Div
      | EOR -> Op.Xor
      | EQ_OP -> Op.Eq
      | GT -> Op.Gt
      | GEQ -> Op.Ge
      | LT -> Op.Lt
      | LEQ -> Op.Le
      | MINUS -> Op.Sub
      | MUL -> Op.Mul
      | NEQ -> Op.Ne
      | OR -> Op.Or
      | PLUS -> Op.Add
      | SHL -> Op.ShiftLeft
      | SHR -> Op.ShiftRight
      | IMPL | MOD | RDIV -> Warn.fatal "Not yet implemented operation."

    let unop op =
      let open AST in
      match op with
      | BNOT -> M.op Op.Eq V.zero
      | NEG -> M.op Op.Sub V.zero
      | NOT -> M.op1 Op.Not

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
    let pair a b = (a, b)

    let arity_zero name f =
      pair name @@ function
      | [] -> f ()
      | _ :: _ -> Warn.fatal "Arity error for function %s." name

    let arity_one name f =
      pair name @@ function
      | [ x ] -> f x
      | [] | _ :: _ :: _ -> Warn.fatal "Arity error for function %s." name

    let arity_two name f =
      pair name @@ function
      | [ x; y ] -> f x y
      | [] | [ _ ] | _ :: _ :: _ :: _ ->
          Warn.fatal "Arity error for function %s." name

    let arity_three name f =
      pair name @@ function
      | [ x; y; z ] -> f x y z
      | _ -> Warn.fatal "Arity error for function %s." name

    (* Primitives *)
    let extra_funcs ii_env =
      [
        arity_two "read_register" (read_register ii_env);
        arity_three "write_register" (write_register ii_env);
        arity_two "read_memory" (read_memory ii_env);
        arity_three "write_memory" (write_memory ii_env);
        arity_zero "read_pstate_nzcv" (read_pstate_nzcv ii_env);
        arity_one "write_pstate_nzcv" (write_pstate_nzcv ii_env);
      ]

    (* Main function arguments *)
    let fetch_main_args (ii, _poi) =
      let func_opt =
        Misc.find_opt
          (function
            | AST.D_Func f -> String.equal "main" f.AST.name | _ -> false)
          ii.A.inst
      in
      let arg_names =
        match func_opt with
        | Some (AST.D_Func f) -> f.AST.args
        | _ -> Warn.fatal "No function main defined."
      in
      let scope = ("main", 0) in
      let one_arg (x, _ty) =
        let reg = ASLBase.ASLLocalId (scope, x) in
        match A.look_reg reg ii.A.env.A.regs with
        | Some v -> v
        | None -> Warn.fatal "Undefined args for main function: %s" x
      in
      List.map one_arg arg_names

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
        let bind_data = M.( >>= )
        let bind_seq = M.cseq
        let prod = M.( >>| )
        let choice = choice
        let return = M.unitT
        let fatal msg = Warn.fatal "%s" msg
        let on_write_identifier = on_write_identifier ii_env
        let on_read_identifier = on_read_identifier ii_env
        let binop = binop
        let unop = unop
        let create_vector = create_vector
        let get_i = get_i
        let set_i = set_i
      end in
      let module ASLInterpreter = Asllib.Interpreter.Make (ASLBackend) in
      let ast = parsed_ast_to_ast ii.A.inst in
      let* _ =
        ASLInterpreter.run ast (extra_funcs ii_env) (fetch_main_args ii_env)
      in
      M.addT !(snd ii_env) B.nextT

    let spurious_setaf _ = assert false
  end
end
