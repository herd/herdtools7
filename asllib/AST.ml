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

type unop = BNOT | NEG | NOT

type binop =
  | AND
  | BAND
  | BEQ
  | BOR
  | DIV
  | EOR
  | EQ_OP
  | GT
  | GEQ
  | IMPL
  | LT
  | LEQ
  | MOD
  | MINUS
  | MUL
  | NEQ
  | OR
  | PLUS
  | RDIV
  | SHL
  | SHR

type identifier = string

module ISet = Set.Make (String)
module IMap = Map.Make (String)

type ('i, 'b, 'r, 'bv) value =
  | VInt of 'i
  | VBool of 'b
  | VReal of 'r
  | VBitVector of 'bv

let value_of_vint i = VInt i
let value_of_vbool b = VBool b
let value_of_vreal r = VReal r
let value_of_vbitvector bv = VBitVector bv

type 'v expr =
  | ELiteral of 'v
  | EVar of identifier
  | EBinop of binop * 'v expr * 'v expr
  | EUnop of unop * 'v expr
  | ECall of identifier * 'v expr list
  | EGet of identifier * 'v expr list
  | ECond of 'v expr * 'v expr * 'v expr

type 'v lexpr = LEVar of identifier | LESet of identifier * 'v expr list

type 'v stmt =
  | SPass
  | SThen of 'v stmt * 'v stmt
  | SAssign of 'v lexpr * 'v expr
  | SCall of identifier * 'v expr list
  | SReturn of 'v expr list
  | SCond of 'v expr * 'v stmt * 'v stmt

type 'v func = identifier * identifier list * 'v stmt

type 'v decl =
  | Func of 'v func
  | Enum of string list
  | GlobalConst of identifier * 'v expr

type 'v t = 'v decl list

let rec stmt_from_list = function
  | [] -> SPass
  | [ x ] -> x
  | h :: t -> SThen (h, stmt_from_list t)


(*****************************************************************************)
(*                                                                           *)
(*                                 Serialize                                 *)
(*                                                                           *)
(*****************************************************************************)

module Serialize = struct
  (*
    Goals:
    1. Stable
    2. Fast?

    Trying to build something that looks like S-expressions

    Inspired by https://stackoverflow.com/questions/4258164/how-to-print-a-tree-structure-into-a-string-fast-in-ocaml
    *)

  let addb buf s = Buffer.add_string buf s

  let with_buf f =
    (* Same default value as Stdlib.Printf *)
    let b = Buffer.create 64 in
    let () = f b in
    Buffer.contents b

  let pp_list pp_elt buf =
    let pp_elt_with_sep elt =
      addb buf " ";
      pp_elt buf elt
    in
    function
    | [] -> ()
    | h :: t ->
        pp_elt buf h;
        List.iter pp_elt_with_sep t

  let pp_binop : binop -> string = function
    | AND -> "AND"
    | BAND -> "BAND"
    | BEQ -> "BEQ"
    | BOR -> "BOR"
    | DIV -> "DIV"
    | EOR -> "EOR"
    | EQ_OP -> "EQ_OP"
    | GT -> "GT"
    | GEQ -> "GEQ"
    | IMPL -> "IMPL"
    | LT -> "LT"
    | LEQ -> "LEQ"
    | MOD -> "MOD"
    | MINUS -> "MINUS"
    | MUL -> "MUL"
    | NEQ -> "NEQ"
    | OR -> "OR"
    | PLUS -> "PLUS"
    | RDIV -> "RDIV"
    | SHL -> "SHL"
    | SHR -> "SHR"

  let pp_unop = function BNOT -> "BNOT" | NOT -> "NOT" | NEG -> "NEG"

  let pp_value pp_int pp_bool pp_real pp_bv f = function
    | VInt i ->
        addb f "(VInt ";
        addb f (pp_int i);
        addb f ")"
    | VBool b ->
        addb f "(VBool ";
        addb f (pp_bool b);
        addb f ")"
    | VReal r ->
        addb f "(VReal ";
        addb f (pp_real r);
        addb f ")"
    | VBitVector bv ->
        addb f "(VBitVector ";
        addb f (pp_bv bv);
        addb f ")"

  let pp_string = addb
  let pp_parsed_int = string_of_int
  let pp_parsed_bool = string_of_bool
  let pp_parsed_real = string_of_float
  let pp_parsed_bitvector s = s

  let pp_parsed_value =
    pp_value pp_parsed_int pp_parsed_bool pp_parsed_real pp_parsed_bitvector

  let rec pp_expr pp_v f = function
    | ELiteral v ->
        addb f "(ELiteral ";
        pp_v f v;
        addb f ")"
    | EVar x ->
        addb f "(EVar ";
        addb f x;
        addb f ")"
    | EBinop (op, e1, e2) ->
        addb f "(EBinop ";
        addb f (pp_binop op);
        addb f " ";
        pp_expr pp_v f e1;
        addb f " ";
        pp_expr pp_v f e2;
        addb f ")"
    | EUnop (op, e) ->
        addb f "(EUnop ";
        addb f (pp_unop op);
        addb f " ";
        pp_expr pp_v f e;
        addb f ")"
    | ECall (name, args) ->
        addb f "(ECall ";
        addb f name;
        addb f " ";
        pp_expr_list pp_v f args;
        addb f ")"
    | EGet (name, args) ->
        addb f "(EGet ";
        addb f name;
        addb f " ";
        pp_expr_list pp_v f args;
        addb f ")"
    | ECond (e1, e2, e3) ->
        addb f "(ECond ";
        pp_expr pp_v f e1;
        addb f " ";
        pp_expr pp_v f e2;
        addb f " ";
        pp_expr pp_v f e3;
        addb f ")"

  and pp_expr_list pp_v = pp_list (pp_expr pp_v)

  let pp_lexpr pp_v f = function
    | LEVar x ->
        addb f "(LEVar ";
        addb f x;
        addb f ")"
    | LESet (name, args) ->
        addb f "(LESet ";
        addb f name;
        addb f " ";
        pp_expr_list pp_v f args;
        addb f ")"

  let rec pp_stmt pp_v f = function
    | SPass -> addb f "SPass"
    | SThen (s1, s2) ->
        addb f "(SThen ";
        pp_stmt pp_v f s1;
        addb f " ";
        pp_stmt pp_v f s2;
        addb f ")"
    | SAssign (le, e) ->
        addb f "(SAssign ";
        pp_lexpr pp_v f le;
        addb f " ";
        pp_expr pp_v f e;
        addb f ")"
    | SCall (name, args) ->
        addb f "(SCall ";
        addb f name;
        addb f " ";
        pp_expr_list pp_v f args;
        addb f ")"
    | SCond (e, s1, s2) ->
        addb f "(SCond ";
        pp_expr pp_v f e;
        addb f " ";
        pp_stmt pp_v f s1;
        addb f " ";
        pp_stmt pp_v f s2;
        addb f ")"
    | SReturn el ->
        addb f "(SReturn ";
        pp_expr_list pp_v f el;
        addb f ")"

  let pp_decl pp_v f = function
    | Func (name, arg_names, body) ->
        addb f "(Func ";
        addb f name;
        addb f " ";
        pp_list pp_string f arg_names;
        addb f " ";
        pp_stmt pp_v f body;
        addb f ")"
    | Enum names ->
        addb f "(Enum ";
        addb f " ";
        pp_list pp_string f names;
        addb f ")"
    | GlobalConst (x, e) ->
        addb f "(GlobalConst ";
        addb f x;
        addb f " ";
        pp_expr pp_v f e;
        addb f ")"

  let pp_t pp_v f ast =
    addb f "(Asllib.AST ";
    pp_list (pp_decl pp_v) f ast;
    addb f ")"

  let pp_parsed_t = pp_t pp_parsed_value

  let t_to_string value_to_string ast =
    let pp_v f v = addb f (value_to_string v) in
    with_buf @@ fun b -> pp_t pp_v b ast

  let parsed_t_to_string ast = with_buf @@ fun b -> pp_parsed_t b ast
end

(*****************************************************************************)
(*                                                                           *)
(*                               Pretty-printer                              *)
(*                                                                           *)
(*****************************************************************************)

module PP = struct
  open Format

  let pp_binop : binop -> string = function
    | AND -> "AND"
    | BAND -> "&&"
    | BEQ -> "<->"
    | BOR -> "||"
    | DIV -> "DIV"
    | EOR -> "EOR"
    | EQ_OP -> "=="
    | GT -> ">"
    | GEQ -> ">="
    | IMPL -> "-->"
    | LT -> "<"
    | LEQ -> "<="
    | MOD -> "%"
    | MINUS -> "-"
    | MUL -> "*"
    | NEQ -> "!="
    | OR -> "OR"
    | PLUS -> "+"
    | RDIV -> "/"
    | SHL -> "<<"
    | SHR -> ">>"

  let pp_unop = function BNOT -> "!" | NEG -> "-" | NOT -> "NOT"

  let pp_value pp_int pp_bool pp_real pp_bv f = function
    | VInt i -> pp_int f i
    | VBool b -> pp_bool f b
    | VReal r -> pp_real f r
    | VBitVector bv -> pp_bv f bv

  let pp_parsed_value =
    pp_value pp_print_int pp_print_bool pp_print_float pp_print_string

  let make_pp_sep s f () = pp_print_string f s
  let pp_comma = make_pp_sep ", "

  let rec pp_expr pp_v f = function
    | ELiteral v -> pp_v f v
    | EVar x -> pp_print_string f x
    | EBinop (b, e1, e2) ->
        fprintf f "(%a %s %a)" (pp_expr pp_v) e1 (pp_binop b) (pp_expr pp_v) e2
    | EUnop (u, e) -> fprintf f "(%s %a)" (pp_unop u) (pp_expr pp_v) e
    | ECall (name, args) -> fprintf f "%s(%a)" name (pp_expr_list pp_v) args
    | EGet (name, args) -> fprintf f "%s[%a]" name (pp_expr_list pp_v) args
    | ECond (e1, e2, e3) ->
        fprintf f "if %a then %a else %a end" (pp_expr pp_v) e1 (pp_expr pp_v)
          e2 (pp_expr pp_v) e3

  and pp_expr_list pp_v = pp_print_list ~pp_sep:pp_comma (pp_expr pp_v)

  let pp_lexpr pp_v f = function
    | LEVar x -> pp_print_string f x
    | LESet (x, args) -> fprintf f "%s[%a]" x (pp_expr_list pp_v) args

  let rec pp_stmt pp_v f = function
    | SPass -> pp_print_string f "pass"
    | SThen (s1, s2) -> fprintf f "%a ; %a" (pp_stmt pp_v) s1 (pp_stmt pp_v) s2
    | SAssign (le, e) -> fprintf f "%a = %a" (pp_lexpr pp_v) le (pp_expr pp_v) e
    | SCall (name, args) -> fprintf f "%s(%a)" name (pp_expr_list pp_v) args
    | SReturn el -> fprintf f "return %a" (pp_expr_list pp_v) el
    | SCond (e, s1, s2) ->
        fprintf f "if %a then %a else %a end" (pp_expr pp_v) e (pp_stmt pp_v) s1
          (pp_stmt pp_v) s2

  let pp_decl pp_v f = function
    | Func (name, args, body) ->
        fprintf f "func %s(%a) %a endfunc" name
          (pp_print_list ~pp_sep:pp_comma pp_print_string)
          args (pp_stmt pp_v) body
    | Enum sl ->
        fprintf f "enum {%a}"
          (pp_print_list ~pp_sep:(make_pp_sep "; ") pp_print_string)
          sl
    | GlobalConst (x, e) -> fprintf f "const %s = %a" x (pp_expr pp_v) e

  let pp_t pp_v = pp_print_list ~pp_sep:(make_pp_sep "%n") (pp_decl pp_v)
  let pp_parsed_t = pp_t pp_parsed_value
  let t_to_string_pp pp_v = asprintf "%a" (pp_t pp_v)

  let t_to_string v_to_string =
    let pp_v f v = pp_print_string f (v_to_string v) in
    t_to_string_pp pp_v

  let parsed_t_to_string = t_to_string_pp pp_parsed_value
end

(*****************************************************************************)
(*                                                                           *)
(*                                Other utils                                *)
(*                                                                           *)
(*****************************************************************************)

let use_expr include_funcs : 'v expr -> ISet.t =
  let rec use_ acc = function
    | ELiteral _ -> acc
    | EVar x -> ISet.add x acc
    | EBinop (_op, e1, e2) -> use_ (use_ acc e2) e1
    | EUnop (_op, e) -> use_ acc e
    | ECall (x, args) ->
        let acc = if include_funcs then ISet.add x acc else acc in
        List.fold_left use_ acc args
    | EGet (x, args) ->
        let acc = ISet.add x acc in
        List.fold_left use_ acc args
    | ECond (e1, e2, e3) -> use_ (use_ (use_ acc e1) e3) e2
  in
  use_ ISet.empty

let tr_values tr_int tr_bool tr_real tr_bitvector =
  let rec value_ = function
    | VInt i -> tr_int i
    | VBool b -> tr_bool b
    | VBitVector s -> tr_bitvector s
    | VReal r -> tr_real r
  and expr_ = function
    | ELiteral v -> ELiteral (value_ v)
    | EVar x -> EVar x
    | EUnop (op, e) -> EUnop (op, expr_ e)
    | EBinop (op, e1, e2) -> EBinop (op, expr_ e1, expr_ e2)
    | ECond (e1, e2, e3) -> ECond (expr_ e1, expr_ e2, expr_ e3)
    | ECall (x, es) -> ECall (x, List.map expr_ es)
    | EGet (x, es) -> EGet (x, List.map expr_ es)
  and lexpr_ = function
    | LEVar x -> LEVar x
    | LESet (x, args) -> LESet (x, List.map expr_ args)
  and stmt_ = function
    | SPass -> SPass
    | SAssign (le, e) -> SAssign (lexpr_ le, expr_ e)
    | SCond (e, s1, s2) -> SCond (expr_ e, stmt_ s1, stmt_ s2)
    | SCall (x, es) -> SCall (x, List.map expr_ es)
    | SReturn es -> SReturn (List.map expr_ es)
    | SThen (s1, s2) -> SThen (stmt_ s1, stmt_ s2)
  and decl_ = function
    | Func (x, args, body) -> Func (x, args, stmt_ body)
    | Enum s -> Enum s
    | GlobalConst (x, e) -> GlobalConst (x, expr_ e)
  in

  List.map decl_

type parsed_value = (int, bool, float, string) value
type parsed_t = parsed_value t
