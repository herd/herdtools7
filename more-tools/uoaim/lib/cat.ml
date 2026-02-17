(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module CoreAst = struct
  open AST

  type t = exp

  let of_ident s = Var (TxtLoc.none, s)
  let union l = Op (TxtLoc.none, Union, l)
  let of_set s = Op1 (TxtLoc.none, ToId, s)

  let seq : t list -> t = function
    | [] -> raise (Invalid_argument "empty list")
    | [ e ] -> e
    | es -> Op (TxtLoc.none, Seq, es)

  let plus e = Op1 (TxtLoc.none, Plus, e)

  let inter : t list -> t = function
    | [] -> raise (Invalid_argument "empty list")
    | [ e ] -> e
    | es -> Op (TxtLoc.none, Inter, es)

  let negate e = Op1 (TxtLoc.none, Comp, e)

  let pp =
    let open Format in
    let prec_of = function
      | Var _ -> 5
      | Op1 _ -> 4
      | Op (_, Inter, _) -> 3
      | Op (_, Seq, _) -> 2
      | Op (_, Union, _) -> 1
      | _ -> assert false
    in

    let rec pp_with_prec parent_prec fmt exp =
      let curr_prec = prec_of exp in
      let needs_parens = curr_prec < parent_prec in

      if needs_parens then fprintf fmt "(";

      begin match exp with
      | Var (_, name) -> fprintf fmt "%s" name
      | Op1 (_, ToId, s_exp) -> fprintf fmt "[%a]" (pp_with_prec 0) s_exp
      | Op (_, Union, exps) ->
          pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt " | ")
            (pp_with_prec curr_prec) fmt exps
      | Op (_, Seq, exps) ->
          pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt "; ")
            (pp_with_prec curr_prec) fmt exps
      | Op (_, Inter, exps) ->
          pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt " & ")
            (pp_with_prec curr_prec) fmt exps
      | Op1 (_, Plus, e) -> fprintf fmt "%a+" (pp_with_prec curr_prec) e
      | Op1 (_, Comp, e) -> fprintf fmt "~%a" (pp_with_prec curr_prec) e
      | Op1 (_, Inv, e) -> fprintf fmt "%a^-1" (pp_with_prec curr_prec) e
      | _ -> assert false
      end;

      if needs_parens then fprintf fmt ")"
    in
    pp_with_prec 0
end

module SetExp = struct
  include CoreAst
end

type set_exp = SetExp.t

module RelExp = struct
  open AST
  include CoreAst

  let as_ident = function Var (_, s) -> Some s | _ -> None

  let rec identifiers : t -> string list =
    let names_of_list l =
      List.fold_left List.append [] (List.map identifiers l)
    in
    function
    | Op (_, _, l) -> names_of_list l
    | Op1 (_, ToId, _) -> []
    | Op1 (_, _, e) -> identifiers e
    | Var (_, nm) -> [ nm ]
    | _ -> assert false

  (* Push inversion operator down the AST, to `Var` leaves. *)
  let invert =
    let rec go = function
      | Op1 (_, ToId, _) as e -> e
      | Op1 (_, Inv, e) -> e
      | Op1 (_, op, e) -> Op1 (TxtLoc.none, op, go e)
      | Op (_, Union, es) -> union (List.map go es)
      | Op (_, Inter, es) -> inter (List.map go es)
      | Op (_, Seq, es) -> seq (List.rev (List.map go es))
      | Var (_, s) -> Op1 (TxtLoc.none, Inv, Var (TxtLoc.none, s))
      | _ -> assert false
    in
    go

  (* Collect all directly inverted identifiers in an expression.
     [invert] guarantees that there are no other expression constructors to
     which [Inv] is applied. *)
  let inverted_idents =
    let rec go = function
      | Op1 (_, Inv, Var (_, s)) -> [ s ]
      | Op1 (_, ToId, _) -> []
      | Op1 (_, _, e) -> go e
      | Op (_, _, es) -> Misc.List.concat_map go es
      | Var _ -> []
      | _ -> assert false
    in
    go
end

type rel_exp = RelExp.t
