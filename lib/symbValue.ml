(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

module Make(Cst:Constant.S) = struct
  module Cst = Cst
  module Scalar = Cst.Scalar

  open Constant

  type csym = int

  let pp_csym i = sprintf "S%i" i
  let compare_csym v1 v2 = Misc.int_compare v1 v2

  let nextsym = ref 0

  let gensym () = nextsym := !nextsym + 1; !nextsym

  type cst = Cst.v

  type v =
    | Var of csym
    | Val of cst
(* A symbolic constant, computations much reduced on them... *)
  let fresh_var () = Var (gensym ())

  let from_var v = Var v

  let as_var = function
    | Var v -> Some v
    | Val _ -> None

  let pp hexa v =  match v with
  | Var s -> pp_csym s
  | Val x -> Cst.pp hexa x


  let pp_v v =  match v with
  | Var s -> pp_csym s
  | Val x -> Cst.pp_v x

  let equalityPossible v1 v2 =
    match (v1,v2) with
    | Val x1,Val x2 -> Cst.compare x1 x2 = 0
    | (Var _,_)
    | (_,Var _) -> true  (* WARNING: May want to optimize later *)

  let compare v1 v2 = match v1,v2 with
  | Val i1,Val i2 -> Cst.compare i1 i2
  | Var i1,Var i2 -> compare_csym i1 i2
  | Val _,Var _ -> 1
  | Var _,Val _ -> -1


  let intToV i  = Val (Cst.intToV i)
  and nameToV s = Val (Cst.nameToV s)
  and cstToV cst = Val cst

  let maybevToV m = match m with
  | Symbolic _ | Label _ | Tag _ as _m -> Val _m
  | Concrete s -> Val (Concrete (Scalar.of_string s))

  let as_symbol = function
    | Val v -> Cst.vToName v
    | Var _ -> assert false

  let zero = Val Cst.zero
  and one = Val Cst.one
  and two = intToV 2


(************************************)
(* Constraint compatible operations *)
(************************************)

(* generic *)
  exception Undetermined

  let is_zero v = match v with
  | Val cst -> Cst.eq cst Cst.zero
  | Var _ -> raise  Undetermined

  let is_one v = match v with
  | Val cst ->  Cst.eq cst Cst.one
  | Var _ -> raise  Undetermined

  let unop op_op op v1 = match v1 with
  | Val (Concrete i1) -> Val (Concrete (op i1))
  | Val (Symbolic _|Label _|Tag _ as x) ->
      Warn.user_error "Illegal operation %s on %s" (Op.pp_op1 true op_op) (Cst.pp_v x)
  | Var _ -> raise Undetermined

  let binop op_op op v1 v2 = match v1,v2 with
  | (Val (Concrete i1),Val (Concrete i2)) -> Val (Concrete (op i1 i2))
  | (Val (Concrete _),Val (Symbolic _|Label _|Tag _))
  | (Val (Symbolic _|Label _|Tag _),Val (Concrete _))
  | (Val (Symbolic _|Label _|Tag _),Val (Symbolic _|Label _|Tag _)) ->
      Warn.user_error
        "Illegal operation %s on constants %s and %s"
        (Op.pp_op op_op) (pp_v v1) (pp_v v2)
  | (Var _,_)|(_,Var _)
    -> raise Undetermined


(* specific binops, with some specific cases for symbolic constants *)

  let add v1 v2 =
(* Particular cases are important for symbolic constants *)
    if is_zero v1 then v2
    else if is_zero v2 then v1
    else match v1,v2 with
    | (Val (Concrete i1),Val (Symbolic (s,i2)))
    | (Val (Symbolic (s,i2)),Val (Concrete i1)) ->
        let i1 = Scalar.to_int i1 in
        Val (Symbolic (s,i1+i2))
    | _,_ -> (* General case *)
    binop Op.Add Scalar.add v1 v2

  and add_konst k v = match v with
  | Val (Concrete v) -> Val (Concrete (Scalar.addk v k))
  | Val (Symbolic (s,i)) -> Val (Symbolic (s,i+k))
  | Val (Label _|Tag _) ->
      Warn.user_error "Illegal addition on constants %s" (pp_v v)
  | Var _ -> raise Undetermined

  and orop v1 v2 =
    if is_zero v1 then v2
    else if is_zero v2 then v1
    else binop Op.Or Scalar.logor v1 v2

  and xor v1 v2 =
    if compare v1 v2 = 0 then zero else
    binop Op.Xor (Scalar.logxor) v1 v2

  let bool_to_v f v1 v2 = match f v1 v2 with
  | false -> zero
  | true -> one

  let bool_to_scalar b = match b with
  | false -> Scalar.zero
  | true -> Scalar.one

  let scalar_to_bool v = Scalar.compare v Scalar.zero <> 0

  let eq v1 v2 = match v1,v2 with
  | Var i1,Var i2 when Misc.int_eq i1 i2 -> one
  | Val (Symbolic _|Label _|Tag _ as s1),Val (Symbolic _|Label _|Tag _ as s2) ->
      bool_to_v Cst.eq s1 s2
(* Assume concrete and others always to differ *)
  | (Val (Symbolic _|Label _|Tag _), Val (Concrete _))
  | (Val (Concrete _), Val (Symbolic _|Label _|Tag _)) -> zero
  | _,_ ->
      binop
        Op.Eq
        (fun s1 s2 -> bool_to_scalar (Scalar.compare s1 s2 = 0))
        v1 v2

  let ne v1 v2 = if is_zero (eq v1 v2) then one else zero

  let lt =
    binop Op.Lt
      (fun s1 s2 -> bool_to_scalar (Scalar.lt s1 s2))

  let gt v1 v2 = lt v2 v1

  let le =
    binop Op.Lt
      (fun s1 s2 -> bool_to_scalar (Scalar.le s1 s2))

  let ge v1 v2 = le v2 v1

  open Op

  let mask_one k = Scalar.shift_left Scalar.one k

(* Ops on tagged locations *)
  let op_tagged op_op op v = match v with
  |  Val (Symbolic (a,o)) -> Val (op a o)
  |  Val (Concrete _|Label _|Tag _) ->
      Warn.user_error "Illegal %s" op_op
  | Var _ -> raise Undetermined

  (*  Returns the location of the tag associated to a location *)
  let op_tagloc (a,_) _ =  Symbolic ((Misc.add_atag a,None),0)
  let tagloc = op_tagged "tagloc" op_tagloc

  (* Decompose tagged locations *)
  let op_tagextract (_,t) _ = Tag t
  let tagextract v = op_tagged "tagextract" op_tagextract v
  let op_locextract (a,_) o = Symbolic ((a,None),o)
  let locextract v = op_tagged "locextract" op_locextract v

  let op1 op =
    let open! Scalar in
    match op with
  | Not -> unop op (fun v -> bool_to_scalar (not (scalar_to_bool v)))
  | SetBit k ->
      unop op (fun s -> logor (mask_one k) s)
  | UnSetBit k ->
      unop op
        (fun s -> logand (lognot (mask_one k)) s)
  | ReadBit k ->
      unop op
        (fun s ->
          bool_to_scalar (Scalar.compare (logand (mask_one k) s) zero <> 0))
  | LogicalRightShift 0
  | LeftShift 0
  | AddK 0 -> fun s -> s
  | LeftShift k ->
      unop  op (fun s -> Scalar.shift_left s k)
  | LogicalRightShift k ->
      unop op (fun s -> Scalar.shift_right_logical s k)
  | AddK k -> add_konst k
  | AndK k -> unop op (fun s -> Scalar.logand s (Scalar.of_string k))
  | Mask sz -> unop op (Scalar.mask sz)
  | TagLoc -> tagloc
  | TagExtract -> tagextract
  | LocExtract -> locextract

  let op op = match op with
  | Add -> add
  | Sub -> binop op (Scalar.sub)
  | Mul -> binop op (Scalar.mul)
  | Div -> binop op (Scalar.div)
  | And -> binop op (Scalar.logand)
  | Or -> orop
  | Xor -> xor
  | Nor -> binop op (fun x1 x2 -> Scalar.lognot (Scalar.logor x1 x2))
  | AndNot2 -> binop op (fun x1 x2 -> Scalar.logand x1 (Scalar.lognot x2))
  | ShiftLeft ->
      binop op (fun x y -> Scalar.shift_left x (Scalar.to_int y))
  | Lt -> lt
  | Gt -> gt
  | Eq -> eq
  | Ne -> ne
  | Le -> le
  | Ge -> ge
  | Max ->
      binop op
        (fun x y -> if Scalar.lt x y then y else x)
  | Min ->
      binop op
        (fun x y -> if Scalar.lt x y then x else y)

  let op3 If v1 v2 v3 = match v1 with
  | Val (Concrete x) -> if scalar_to_bool x then v2 else v3
  | Val (Symbolic _ |Label _|Tag _ as s) ->
      Warn.user_error "illegal if on symbolic constant %s" (Cst.pp_v s)
  | Var _ -> raise Undetermined

  let fold_over_vals f init = f (fresh_var ()) init

  module OrderedValue = struct
    type t = v
    let compare = compare
  end


  module ValueSet = MySet.Make(OrderedValue)

  module OrderedVar = struct
    type t = csym
    let compare = compare_csym
  end

  module Solution = Map.Make(OrderedVar)

  type solution = v Solution.t

  let is_var_determined v = match v with
  | Var _ -> false
  | Val _ -> true

  let determined_val v = match v with
  | Var _ -> None
  | Val i -> Some i

  let simplify_var soln v = match v with
  | Val _ -> v
  | Var x ->  try Solution.find  x soln with Not_found -> v

end
