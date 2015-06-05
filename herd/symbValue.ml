(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

module Cst = SymbConstant
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
and maybevToV m = Val m

let zero = intToV 0
and one = intToV 1
and two = intToV 2


let is_zero v = match v with
| Val (Concrete i) -> i=0
| Val (Symbolic _)  -> false
| Var _ -> assert false

let is_one v = match v with
| Val (Concrete i) -> i=1
| Val (Symbolic _)  -> false
| Var _ -> assert false

(************************************)
(* Constraint compatible operations *)
(************************************)

(* generic *)
exception Undetermined

let unop op v1 = match v1 with
| Val (Concrete i1) -> Val (Concrete (op i1))
| Val (Symbolic s1) ->
    Warn.user_error "illegal operation on %s" s1
| Var _ -> raise Undetermined

let binop op_op op v1 v2 = match v1,v2 with
| (Val (Concrete i1),Val (Concrete i2)) -> Val (Concrete (op i1 i2))
| (Val (Concrete _),Val (Symbolic _))
| (Val (Symbolic _),Val (Concrete _))
| (Val (Symbolic _),Val (Symbolic _)) ->
    Warn.user_error
      "illegal operation %s on constants %s and %s"
      (Op.pp_op op_op) (pp_v v1) (pp_v v2)
| _,_ -> raise Undetermined


(* specific binops, with some specific cases for symbolic constants *)

let add v1 v2 = match v1,v2 with
| (Val (Concrete 0),v)
| (v,Val (Concrete 0)) -> v (* Important for symbolic constants *)
| _,_ -> binop Op.Add (+) v1 v2

and orop v1 v2 = match v1,v2 with
| Val (Concrete 0),v
| v,Val (Concrete 0) -> v
| _,_ -> binop Op.Or (lor) v1 v2

and xor v1 v2 = 
  if compare v1 v2 = 0 then zero else
  binop Op.Xor (lxor) v1 v2
(* match v1,v2 with *)
(* | Val v1,Val v2 when Cst.compare v1 v2 = 0 ->  *)
(* (\* For concrete values only, otherwise causality violation *\) *)
(*     zero *)
(* | _,_ -> binop Op.Xor (lxor) v1 v2 *)

let bool_to_int f v1 v2 = match f v1 v2 with
  | false -> 0
  | true -> 1

let eq v1 v2 = match v1,v2 with
| Var i1,Var i2 when Misc.int_eq i1 i2 -> one
| Val (Symbolic s1),Val (Symbolic s2) ->
    intToV (bool_to_int Misc.string_eq s1 s2)
(* Assume symbolic value not to be zero *)
| Val (Symbolic _), Val (Concrete 0)
| Val (Concrete 0), Val (Symbolic _) -> zero
| _,_ ->
    binop Op.Eq (bool_to_int Misc.int_eq) v1 v2

let ne v1 v2 = match v1,v2 with
| Var i1,Var i2 when not (Misc.int_eq i1 i2) -> one
| Val (Symbolic s1),Val (Symbolic s2) ->
    intToV (bool_to_int (fun s1 s2 -> (not (Misc.string_eq s1 s2))) s1 s2)
(* Assume symbolic value not to be zero *)
| Val (Symbolic _), Val (Concrete 0)
| Val (Concrete 0), Val (Symbolic _) -> one
| _,_ ->
    binop Op.Ne
      (bool_to_int (fun v1 v2 -> not (Misc.int_eq v1 v2))) v1 v2

let lt v1 v2 = match v1,v2 with
| Val (Symbolic s1),Val (Symbolic s2) ->
    intToV (bool_to_int (<) s1 s2)
| Val (Symbolic _), Val (Concrete 0) -> one
| Val (Concrete 0), Val (Symbolic _) -> zero
| _,_ ->
    binop Op.Lt (bool_to_int (<)) v1 v2

let gt v1 v2 = lt v2 v1

let le v1 v2 = match v1,v2 with
| Val (Symbolic s1),Val (Symbolic s2) ->
    intToV (bool_to_int (<=) s1 s2)
| Val (Symbolic _), Val (Concrete 0) -> one
| Val (Concrete 0), Val (Symbolic _) -> zero
| _,_ ->
    binop Op.Le (bool_to_int (<=)) v1 v2

let ge v1 v2 = le v2 v1

open Op

let op1 op = match op with
| Not ->
    unop (fun i -> match i with 0 -> 1 | _ -> 0)
| SetBit k -> unop (fun i -> (1 lsl k) lor i)
| UnSetBit k -> unop (fun i -> (lnot (1 lsl k)) land i)
| ReadBit k ->
    unop
      (fun i -> if (1 lsl k) land i = 0 then 0 else 1)

let op op = match op with
| Add -> add
| Sub -> binop op (-)
| Mul -> binop op ( * )
| Div -> binop op ( / )
| And -> binop op (land)
| Or -> orop
| Xor -> xor
| Nor -> binop op (fun x1 x2 -> lnot (x1 lor x2))
| ShiftLeft -> binop op (lsl)
| Lt -> lt
| Gt -> gt
| Eq -> eq
| Ne -> ne
| Le -> le
| Ge -> ge

let op3 If v1 v2 v3 = match v1 with
| Val (Concrete i1) ->
    begin match i1 with
    | 0 -> v3
    | _ -> v2
    end
| Val (Symbolic s) ->
    Warn.user_error "illegal if on symbolic constant %s" s
| Var _ -> raise Undetermined

let fold_over_vals f init = (f (fresh_var ()) init)

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


