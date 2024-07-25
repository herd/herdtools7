(****************************************************************************************************************)
(*  SPDX-FileCopyrightText: Copyright 2022-2024 Arm Limited and/or its affiliates <open-source-office@arm.com>  *)
(*  SPDX-License-Identifier: BSD-3-Clause                                                                       *)
(****************************************************************************************************************)

module type Syntax = sig
  val bnot : bool
  val neg : bool
  val not : bool
  val and_ : bool
  val band : bool
  val beq : bool
  val bor : bool
  val div : bool
  val divrm : bool
  val eor : bool
  val eq_op : bool
  val gt : bool
  val geq : bool
  val impl : bool
  val lt : bool
  val leq : bool
  val mod_ : bool
  val minus : bool
  val mul : bool
  val neq : bool
  val or_ : bool
  val plus : bool
  val pow : bool
  val rdiv : bool
  val shl : bool
  val shr : bool
  val l_int : bool
  val l_bool : bool
  val l_real : bool
  val l_bitvector : bool
  val l_string : bool
  val e_literal : bool
  val e_var : bool
  val e_ctc : bool
  val e_binop : bool
  val e_unop : bool
  val e_call : bool
  val e_slice : bool
  val e_cond : bool
  val e_getarray : bool
  val e_getfield : bool
  val e_getfields : bool
  val e_record : bool
  val e_concat : bool
  val e_tuple : bool
  val e_unknown : bool
  val e_pattern : bool
  val pattern_all : bool
  val pattern_any : bool
  val pattern_geq : bool
  val pattern_leq : bool
  val pattern_mask : bool
  val pattern_not : bool
  val pattern_range : bool
  val pattern_single : bool
  val pattern_tuple : bool
  val slice_single : bool
  val slice_range : bool
  val slice_length : bool
  val slice_star : bool
  val t_int : bool
  val t_bits : bool
  val t_real : bool
  val t_string : bool
  val t_bool : bool
  val t_enum : bool
  val t_tuple : bool
  val t_array : bool
  val t_record : bool
  val t_exception : bool
  val t_named : bool
  val constraint_exact : bool
  val constraint_range : bool
  val unconstrained : bool
  val wellconstrained : bool
  val underconstrained : bool
  val bitfield_simple : bool
  val bitfield_nested : bool
  val bitfield_type : bool
  val le_discard : bool
  val le_var : bool
  val le_slice : bool
  val le_setarray : bool
  val le_setfield : bool
  val le_setfields : bool
  val le_destructuring : bool
  val le_concat : bool
  val ldk_var : bool
  val ldk_let : bool
  val ldk_constant : bool
  val ldi_discard : bool
  val ldi_var : bool
  val ldi_tuple : bool
  val ldi_typed : bool
  val up : bool
  val down : bool
  val s_pass : bool
  val s_seq : bool
  val s_decl : bool
  val s_assign : bool
  val s_call : bool
  val s_return : bool
  val s_cond : bool
  val s_case : bool
  val s_assert : bool
  val s_for : bool
  val s_while : bool
  val s_repeat : bool
  val s_throw : bool
  val s_try : bool
  val s_print : bool
  val st_procedure : bool
  val st_function : bool
  val st_getter : bool
  val st_setter : bool
end

module type S = sig
  module Syntax : Syntax
end

module All : Syntax = struct
  let bnot = true
  let neg = true
  let not = true
  let and_ = true
  let band = true
  let beq = true
  let bor = true
  let div = true
  let divrm = true
  let eor = true
  let eq_op = true
  let gt = true
  let geq = true
  let impl = true
  let lt = true
  let leq = true
  let mod_ = true
  let minus = true
  let mul = true
  let neq = true
  let or_ = true
  let plus = true
  let pow = true
  let rdiv = true
  let shl = true
  let shr = true
  let l_int = true
  let l_bool = true
  let l_real = true
  let l_bitvector = true
  let l_string = true
  let e_literal = true
  let e_var = true
  let e_ctc = true
  let e_binop = true
  let e_unop = true
  let e_call = true
  let e_slice = true
  let e_cond = true
  let e_getarray = true
  let e_getfield = true
  let e_getfields = true
  let e_record = true
  let e_concat = true
  let e_tuple = true
  let e_unknown = true
  let e_pattern = true
  let pattern_all = true
  let pattern_any = true
  let pattern_geq = true
  let pattern_leq = true
  let pattern_mask = true
  let pattern_not = true
  let pattern_range = true
  let pattern_single = true
  let pattern_tuple = true
  let slice_single = true
  let slice_range = true
  let slice_length = true
  let slice_star = true
  let t_int = true
  let t_bits = true
  let t_real = true
  let t_string = true
  let t_bool = true
  let t_enum = true
  let t_tuple = true
  let t_array = true
  let t_record = true
  let t_exception = true
  let t_named = true
  let constraint_exact = true
  let constraint_range = true
  let unconstrained = true
  let wellconstrained = true
  let underconstrained = true
  let bitfield_simple = true
  let bitfield_nested = true
  let bitfield_type = true
  let le_discard = true
  let le_var = true
  let le_slice = true
  let le_setarray = true
  let le_setfield = true
  let le_setfields = true
  let le_destructuring = true
  let le_concat = true
  let ldk_var = true
  let ldk_let = true
  let ldk_constant = true
  let ldi_discard = true
  let ldi_var = true
  let ldi_tuple = true
  let ldi_typed = true
  let up = true
  let down = true
  let s_pass = true
  let s_seq = true
  let s_decl = true
  let s_assign = true
  let s_call = true
  let s_return = true
  let s_cond = true
  let s_case = true
  let s_assert = true
  let s_for = true
  let s_while = true
  let s_repeat = true
  let s_throw = true
  let s_try = true
  let s_print = true
  let st_procedure = true
  let st_function = true
  let st_getter = true
  let st_setter = true
end

module Stable : Syntax = struct
  include All

  let e_unknown = false
  let e_getarray = false
  let le_setarray = false
end

let default_config =
  (module struct
    module Syntax = Stable
  end : S)

module Parse = struct
  module Tbl = Hashtbl.Make (String)

  let default_hashtbl =
    let tbl : bool Tbl.t = Tbl.create 128 in
    let () =
      [
        ("bnot", true);
        ("neg", true);
        ("not", true);
        ("and", true);
        ("band", true);
        ("beq", true);
        ("bor", true);
        ("div", true);
        ("divrm", true);
        ("eor", true);
        ("eq_op", true);
        ("gt", true);
        ("geq", true);
        ("impl", true);
        ("lt", true);
        ("leq", true);
        ("mod", true);
        ("minus", true);
        ("mul", true);
        ("neq", true);
        ("or", true);
        ("plus", true);
        ("pow", true);
        ("rdiv", true);
        ("shl", true);
        ("shr", true);
        ("l_int", true);
        ("l_bool", true);
        ("l_real", true);
        ("l_bitvector", true);
        ("l_string", true);
        ("e_literal", true);
        ("e_var", true);
        ("e_ctc", true);
        ("e_binop", true);
        ("e_unop", true);
        ("e_call", true);
        ("e_slice", true);
        ("e_cond", true);
        ("e_getarray", true);
        ("e_getfield", true);
        ("e_getfields", true);
        ("e_record", true);
        ("e_concat", true);
        ("e_tuple", true);
        ("e_unknown", true);
        ("e_pattern", true);
        ("pattern_all", true);
        ("pattern_any", true);
        ("pattern_geq", true);
        ("pattern_leq", true);
        ("pattern_mask", true);
        ("pattern_not", true);
        ("pattern_range", true);
        ("pattern_single", true);
        ("pattern_tuple", true);
        ("slice_single", true);
        ("slice_range", true);
        ("slice_length", true);
        ("slice_star", true);
        ("t_int", true);
        ("t_bits", true);
        ("t_real", true);
        ("t_string", true);
        ("t_bool", true);
        ("t_enum", true);
        ("t_tuple", true);
        ("t_array", true);
        ("t_record", true);
        ("t_exception", true);
        ("t_named", true);
        ("constraint_exact", true);
        ("constraint_range", true);
        ("unconstrained", true);
        ("wellconstrained", true);
        ("underconstrained", true);
        ("bitfield_simple", true);
        ("bitfield_nested", true);
        ("bitfield_type", true);
        ("le_discard", true);
        ("le_var", true);
        ("le_slice", true);
        ("le_setarray", true);
        ("le_setfield", true);
        ("le_setfields", true);
        ("le_destructuring", true);
        ("le_concat", true);
        ("ldk_var", true);
        ("ldk_let", true);
        ("ldk_constant", true);
        ("ldi_discard", true);
        ("ldi_var", true);
        ("ldi_tuple", true);
        ("ldi_typed", true);
        ("up", true);
        ("down", true);
        ("s_pass", true);
        ("s_seq", true);
        ("s_decl", true);
        ("s_assign", true);
        ("s_call", true);
        ("s_return", true);
        ("s_cond", true);
        ("s_case", true);
        ("s_assert", true);
        ("s_for", true);
        ("s_while", true);
        ("s_repeat", true);
        ("s_throw", true);
        ("s_try", true);
        ("s_print", true);
        ("st_procedure", true);
        ("st_function", true);
        ("st_getter", true);
        ("st_setter", true);
      ]
      |> List.to_seq |> Tbl.add_seq tbl
    in
    tbl

  let of_hashtbl tbl =
    let module M = struct
      let bnot = Tbl.find tbl "bnot"
      let neg = Tbl.find tbl "neg"
      let not = Tbl.find tbl "not"
      let and_ = Tbl.find tbl "and"
      let band = Tbl.find tbl "band"
      let beq = Tbl.find tbl "beq"
      let bor = Tbl.find tbl "bor"
      let div = Tbl.find tbl "div"
      let divrm = Tbl.find tbl "divrm"
      let eor = Tbl.find tbl "eor"
      let eq_op = Tbl.find tbl "eq_op"
      let gt = Tbl.find tbl "gt"
      let geq = Tbl.find tbl "geq"
      let impl = Tbl.find tbl "impl"
      let lt = Tbl.find tbl "lt"
      let leq = Tbl.find tbl "leq"
      let mod_ = Tbl.find tbl "mod"
      let minus = Tbl.find tbl "minus"
      let mul = Tbl.find tbl "mul"
      let neq = Tbl.find tbl "neq"
      let or_ = Tbl.find tbl "or"
      let plus = Tbl.find tbl "plus"
      let pow = Tbl.find tbl "pow"
      let rdiv = Tbl.find tbl "rdiv"
      let shl = Tbl.find tbl "shl"
      let shr = Tbl.find tbl "shr"
      let l_int = Tbl.find tbl "l_int"
      let l_bool = Tbl.find tbl "l_bool"
      let l_real = Tbl.find tbl "l_real"
      let l_bitvector = Tbl.find tbl "l_bitvector"
      let l_string = Tbl.find tbl "l_string"
      let e_literal = Tbl.find tbl "e_literal"
      let e_var = Tbl.find tbl "e_var"
      let e_ctc = Tbl.find tbl "e_ctc"
      let e_binop = Tbl.find tbl "e_binop"
      let e_unop = Tbl.find tbl "e_unop"
      let e_call = Tbl.find tbl "e_call"
      let e_slice = Tbl.find tbl "e_slice"
      let e_cond = Tbl.find tbl "e_cond"
      let e_getarray = Tbl.find tbl "e_getarray"
      let e_getfield = Tbl.find tbl "e_getfield"
      let e_getfields = Tbl.find tbl "e_getfields"
      let e_record = Tbl.find tbl "e_record"
      let e_concat = Tbl.find tbl "e_concat"
      let e_tuple = Tbl.find tbl "e_tuple"
      let e_unknown = Tbl.find tbl "e_unknown"
      let e_pattern = Tbl.find tbl "e_pattern"
      let pattern_all = Tbl.find tbl "pattern_all"
      let pattern_any = Tbl.find tbl "pattern_any"
      let pattern_geq = Tbl.find tbl "pattern_geq"
      let pattern_leq = Tbl.find tbl "pattern_leq"
      let pattern_mask = Tbl.find tbl "pattern_mask"
      let pattern_not = Tbl.find tbl "pattern_not"
      let pattern_range = Tbl.find tbl "pattern_range"
      let pattern_single = Tbl.find tbl "pattern_single"
      let pattern_tuple = Tbl.find tbl "pattern_tuple"
      let slice_single = Tbl.find tbl "slice_single"
      let slice_range = Tbl.find tbl "slice_range"
      let slice_length = Tbl.find tbl "slice_length"
      let slice_star = Tbl.find tbl "slice_star"
      let t_int = Tbl.find tbl "t_int"
      let t_bits = Tbl.find tbl "t_bits"
      let t_real = Tbl.find tbl "t_real"
      let t_string = Tbl.find tbl "t_string"
      let t_bool = Tbl.find tbl "t_bool"
      let t_enum = Tbl.find tbl "t_enum"
      let t_tuple = Tbl.find tbl "t_tuple"
      let t_array = Tbl.find tbl "t_array"
      let t_record = Tbl.find tbl "t_record"
      let t_exception = Tbl.find tbl "t_exception"
      let t_named = Tbl.find tbl "t_named"
      let constraint_exact = Tbl.find tbl "constraint_exact"
      let constraint_range = Tbl.find tbl "constraint_range"
      let unconstrained = Tbl.find tbl "unconstrained"
      let wellconstrained = Tbl.find tbl "wellconstrained"
      let underconstrained = Tbl.find tbl "underconstrained"
      let bitfield_simple = Tbl.find tbl "bitfield_simple"
      let bitfield_nested = Tbl.find tbl "bitfield_nested"
      let bitfield_type = Tbl.find tbl "bitfield_type"
      let le_discard = Tbl.find tbl "le_discard"
      let le_var = Tbl.find tbl "le_var"
      let le_slice = Tbl.find tbl "le_slice"
      let le_setarray = Tbl.find tbl "le_setarray"
      let le_setfield = Tbl.find tbl "le_setfield"
      let le_setfields = Tbl.find tbl "le_setfields"
      let le_destructuring = Tbl.find tbl "le_destructuring"
      let le_concat = Tbl.find tbl "le_concat"
      let ldk_var = Tbl.find tbl "ldk_var"
      let ldk_let = Tbl.find tbl "ldk_let"
      let ldk_constant = Tbl.find tbl "ldk_constant"
      let ldi_discard = Tbl.find tbl "ldi_discard"
      let ldi_var = Tbl.find tbl "ldi_var"
      let ldi_tuple = Tbl.find tbl "ldi_tuple"
      let ldi_typed = Tbl.find tbl "ldi_typed"
      let up = Tbl.find tbl "up"
      let down = Tbl.find tbl "down"
      let s_pass = Tbl.find tbl "s_pass"
      let s_seq = Tbl.find tbl "s_seq"
      let s_decl = Tbl.find tbl "s_decl"
      let s_assign = Tbl.find tbl "s_assign"
      let s_call = Tbl.find tbl "s_call"
      let s_return = Tbl.find tbl "s_return"
      let s_cond = Tbl.find tbl "s_cond"
      let s_case = Tbl.find tbl "s_case"
      let s_assert = Tbl.find tbl "s_assert"
      let s_for = Tbl.find tbl "s_for"
      let s_while = Tbl.find tbl "s_while"
      let s_repeat = Tbl.find tbl "s_repeat"
      let s_throw = Tbl.find tbl "s_throw"
      let s_try = Tbl.find tbl "s_try"
      let s_print = Tbl.find tbl "s_print"
      let st_procedure = Tbl.find tbl "st_procedure"
      let st_function = Tbl.find tbl "st_function"
      let st_getter = Tbl.find tbl "st_getter"
      let st_setter = Tbl.find tbl "st_setter"
    end in
    (module struct
      module Syntax = M
    end : S)

  (** [iteri_lines filename cont] calls [cont lineno line] for every line in
      the file.

      If [cont lineno line], it interrupts the iteration without any error
      message. *)
  let iter_lines filename cont =
    let chan = Scanf.Scanning.from_file filename in
    let rec loop () =
      Scanf.bscanf chan "%l%[^\n]\n" (fun lineno line -> cont (lineno + 1) line);
      loop ()
    in
    try loop () with End_of_file -> ()

  let of_file filename =
    let eprintf = Printf.eprintf in
    let update_tbl tbl lineno name value =
      let () = if false then eprintf "Scanned line: %s <-- %B\n" name value in
      let name = String.lowercase_ascii name in
      if Tbl.mem tbl name then Tbl.replace tbl name value
      else eprintf "Ignoring line %d: unknown key %S.\n%!" lineno name
    in
    let parse_one_line tbl lineno line =
      let () = if false then eprintf "Scanned line: %S\n" line in
      try Scanf.sscanf line "%s %_[=:] %B" (update_tbl tbl lineno) with
      | Scanf.Scan_failure s ->
          eprintf "Config file scanning failure at line %d: %s\n%!" lineno s
      | End_of_file ->
          if String.length line > 0 then
            eprintf "Ignoring unparsable line %d: %S\n%!" lineno line
    in
    let tbl = Tbl.copy default_hashtbl in
    if false then eprintf "Scanning config from file %S.\n" filename;
    iter_lines filename (parse_one_line tbl);
    if false then eprintf "End parsing.\n";
    of_hashtbl tbl
end
