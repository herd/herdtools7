(****************************************************************************)(*                           the diy toolsuite                              *)
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
  open PTEVal

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
  | Symbolic _ | Label _ | Tag _ | PteVal _ as _m -> Val _m
  | Concrete s -> Val (Concrete (Scalar.of_string s))

  let zero = Val Cst.zero
  and one = Val Cst.one
  and two = intToV 2
  and default_tag = Val Constant.default_tag

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

  let protect_is p v =  try p v with Undetermined -> false

  let unop op_op op v1 = match v1 with
  | Val (Concrete i1) -> Val (Concrete (op i1))
  | Val (Symbolic _|Label _|Tag _|PteVal _ as x) ->
      Warn.user_error "Illegal operation %s on %s"
        (Op.pp_op1 true op_op) (Cst.pp_v x)
  | Var _ -> raise Undetermined

  let binop op_op op v1 v2 = match v1,v2 with
  | (Val (Concrete i1),Val (Concrete i2)) -> Val (Concrete (op i1 i2))
  | (Val (Concrete _),Val (Symbolic _|Label _|Tag _|PteVal _))
  | (Val (Symbolic _|Label _|Tag _|PteVal _),Val (Concrete _))
  | (Val (Symbolic _|Label _|Tag _|PteVal _),Val (Symbolic _|Label _|Tag _|PteVal _)) ->
      Warn.user_error
        "Illegal operation %s on constants %s and %s"
        (Op.pp_op op_op) (pp_v v1) (pp_v v2)
  | (Var _,_)|(_,Var _)
    -> raise Undetermined


(* specific binops, with some specific cases for symbolic constants *)

  let add v1 v2 =
(* Particular cases are important for symbolic constants *)
    if protect_is is_zero v1 then v2
    else if protect_is is_zero v2 then v1
    else match v1,v2 with
    | (Val (Concrete i1),Val (Symbolic (Virtual (s,i2))))
    | (Val (Symbolic (Virtual (s,i2))),Val (Concrete i1)) ->
        let i1 = Scalar.to_int i1 in
        Val (Symbolic (Virtual (s,i1+i2)))
    | (Val (Concrete i1),Val (Symbolic (Physical (s,i2))))
    | (Val (Symbolic (Physical (s,i2))),Val (Concrete i1)) ->
        let i1 = Scalar.to_int i1 in
        Val (Symbolic (Physical (s,i1+i2)))
    | _,_ -> (* General case *)
        binop Op.Add Scalar.add v1 v2

  and sub v1 v2 = (* Used for comparison for by some arch, so let us compare *)
    match v1,v2 with
    | (Val (Tag _),Val (Tag _))
    | (Val (Symbolic _),Val (Symbolic _))
    | (Val (Label _),Val (Label _))
      ->
        Val (Concrete (Scalar.of_int (compare  v1 v2)))
    | _,_
      ->
        binop Op.Sub Scalar.sub v1 v2


  and add_konst k v = match v with
  | Val (Concrete v) -> Val (Concrete (Scalar.addk v k))
  | Val (Symbolic (Virtual (s,i))) -> Val (Symbolic (Virtual (s,i+k)))
  | Val (Symbolic (Physical (s,i))) -> Val (Symbolic (Physical (s,i+k)))
  | Val (Symbolic (System _)|Label _|Tag _|PteVal _) ->
      Warn.user_error "Illegal addition on constants %s" (pp_v v)
  | Var _ -> raise Undetermined

  and orop v1 v2 =
    if protect_is is_zero v1 then v2
    else if protect_is is_zero v2 then v1
    else binop Op.Or Scalar.logor v1 v2

  and xor v1 v2 =
    if compare v1 v2 = 0 then zero else
    binop Op.Xor (Scalar.logxor) v1 v2

  and maskop op sz v = match v with
  | Val (Tag _) -> v (* tags are small enough for any mask be idempotent *)
  | _ ->  unop op (Scalar.mask sz) v

  and shift_right_logical v1 v2 = match v1,v2 with
  | Val (Symbolic (Virtual ((s,_),_))),Val (Concrete v) when
      Scalar.compare v (Scalar.of_int 12) = 0 ->
        let msg =
          sprintf
            "Illegal operation %s on constants %s and %s"
            (Op.pp_op Op.Lsr) (pp_v v1) (pp_v v2) in
        (* Beware: AArch64 only, otherwise a fatal error. *)
        raise (Cst.Result (`AArch64,Symbolic (System (TLB,s)),msg))
  | _,_ ->
      binop Op.Lsr (fun x y -> Scalar.shift_right_logical x (Scalar.to_int y))
        v1 v2

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
    binop Op.Le
      (fun s1 s2 -> bool_to_scalar (Scalar.le s1 s2))

  let ge v1 v2 = le v2 v1

  open Op

  let mask_one k = Scalar.shift_left Scalar.one k
  let mask_many nbBits k =
    let rec pow a = function (* Why Ocaml hasn't pow function in it's standard library ??? *)
      | 0 -> 1 | 1 -> a
      | n ->
          let b = pow a (n / 2) in
          b * b * (if n mod 2 = 0 then 1 else a) in
    Scalar.shift_left (Scalar.of_int ((pow 2 nbBits) - 1)) k

(* Ops on tagged locations *)
  let settag v1 v2 = match v1,v2 with
  | Val (Symbolic (Virtual ((a,_),o))),Val (Tag t) -> Val (Symbolic (Virtual ((a,Some t),o)))
  | Val cst1,Val cst2 ->
      Warn.user_error "Illegal settag on %s and %s"
        (Cst.pp_v cst1)  (Cst.pp_v cst2)
  | (Var _,_)|(_,Var _) ->
      raise Undetermined

  let op_tagged op_op op v = match v with
  |  Val (Symbolic (Virtual (s,o))) -> Val (op s o)
  |  Val (Concrete _|Symbolic (Physical _ | System _)|Label _|Tag _|PteVal _) ->
      Warn.user_error "Illegal tagged operation %s on %s" op_op (pp_v v)
  | Var _ -> raise Undetermined

        (*  Returns the location of the tag associated to a location *)

  let tagloc v =  match v with
  | Val (Symbolic (Virtual ((a,_),_)|Physical (a,_))) ->
       Val (Symbolic (System (TAG,a)))
  | Val (Concrete _| Symbolic (System _)|Label _|Tag _|PteVal _) ->
      Warn.user_error "Illegal tagloc on %s" (pp_v v)
  | Var _ -> raise Undetermined


      (* Decompose tagged locations *)
  let op_tagextract (_,t) _ = match t with
  | Some t -> Tag t
  | None -> Constant.default_tag

  let tagextract v = op_tagged "tagextract" op_tagextract v
  (*let op_locextract (a,_) o = Symbolic (Virtual ((a,None),o))*)

  let locextract v =
    match v with 
    | Val (Symbolic (Virtual ((s,_),o))) -> Val (Symbolic (Virtual ((s,None),o)))
    | Val (Symbolic (Physical _)) as aphy -> aphy 
    | Val (Concrete _|Symbolic (System _)|Label _|Tag _|PteVal _) ->
      Warn.user_error "Illegal locextract on %s" (pp_v v)
    | Var _ -> raise Undetermined

  let op_pte_tlb op_op op v = match v with
  |  Val (Symbolic (Virtual (a,_))) -> Val (op a)
  |  Val (Concrete _|Label _|Tag _|Symbolic _|PteVal _) ->
      Warn.user_error "Illegal %s on %s" op_op (pp_v v)
  | Var _ -> raise Undetermined

  let op_pteloc (a,_) = Symbolic (System (PTE,a))
  let pteloc = op_pte_tlb "pteloc" op_pteloc

  let op_pte_val op_op op v = match v with
  | Val (PteVal a) -> Val (op a)
  | Var _ -> raise Undetermined
  | _ -> Warn.user_error "Illegal pte operation %s on %s" op_op (pp_v v)

  let op_afloc a = Cst.intToV a.af
  let afloc = op_pte_val "afloc" op_afloc 

  let op_set_pteval op op_op v = match v with
    | Val (PteVal pte_v) -> Val (PteVal (op pte_v))
    | Var _ -> raise Undetermined
    | _ -> Warn.user_error "Illegal %s on %s" op_op (pp_v v)

  let setaf = op_set_pteval (fun v -> { v with af = 1}) "setaf"

  let op_dbloc a = Cst.intToV a.db 
  let dbloc = op_pte_val "dbloc" op_dbloc
  let setdb = op_set_pteval (fun v ->  {v with db = 1}) "setdb"

  let op_dbmloc a = Cst.intToV a.dbm
  let dbmloc = op_pte_val "dbmloc" op_dbmloc

  let op_validloc a = Cst.intToV a.valid
  let validloc = op_pte_val "validloc" op_validloc

  let op_oaloc a = Cst.nameToV a.oa
  let oaloc = op_pte_val "oaloc" op_oaloc

  let op_tlbloc (a,_) = Symbolic (System (TLB,a))
  let tlbloc = op_pte_tlb "tlbloc" op_tlbloc

  let op_phyloc (a,_) = Symbolic (Physical (a,0))
  let phyloc = op_pte_tlb "phyloc" op_phyloc

  let is_virtual v = match v with
  | Val c -> Constant.is_virtual c
  | Var _ -> raise Undetermined

  let as_virtual v = match v with
  | Val c -> Constant.as_virtual c
  | Var _ -> raise Undetermined
  let is_virtual_v v =  if is_virtual v then one else zero

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
    | Mask sz -> maskop op sz
    | TagLoc -> tagloc
    | TagExtract -> tagextract
    | LocExtract -> locextract
    | UnSetXBits (nb, k) ->
        unop op
          (fun s -> logand (lognot (mask_many nb k)) s)
    | TLBLoc -> tlbloc
    | PTELoc -> pteloc
    | PhyLoc -> phyloc
    | IsVirtual -> is_virtual_v
    | AF -> afloc 
    | SetAF -> setaf 
    | DB -> dbloc 
    | SetDB -> setdb 
    | DBM -> dbmloc
    | Valid -> validloc
    | OA -> oaloc  

  let op op = match op with
  | Add -> add
  | Sub -> sub
  | Mul -> binop op (Scalar.mul)
  | Div -> binop op (Scalar.div)
  | And -> binop op (Scalar.logand)
  | Or -> orop
  | Xor -> xor
  | Nor -> binop op (fun x1 x2 -> Scalar.lognot (Scalar.logor x1 x2))
  | AndNot2 -> binop op (fun x1 x2 -> Scalar.logand x1 (Scalar.lognot x2))
  | ShiftLeft ->
      binop op (fun x y -> Scalar.shift_left x (Scalar.to_int y))
  | Lsr ->
      shift_right_logical
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
  | SetTag -> settag

  let op3 If v1 v2 v3 = match v1 with
  | Val (Concrete x) -> if scalar_to_bool x then v2 else v3
  | Val (Symbolic _ |Label _|Tag _ | PteVal _ as s) ->
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
