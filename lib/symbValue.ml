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

  let do_pp pp_val = function
  | Var s -> pp_csym s
  | Val x -> pp_val x

  let pp hexa = do_pp (Cst.pp hexa)

  let pp_unsigned hexa = do_pp (Cst.pp_unsigned hexa)

  let pp_v =  do_pp Cst.pp_v

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
  let stringToV i  = Val (Cst.stringToV i)
  and nameToV s = Val (Cst.nameToV s)
  and cstToV cst = Val cst

  let rec tr_cst c = match c with
    | Symbolic _ | Label _ | Tag _ | PteVal _ as m -> m
    | Concrete s -> Concrete (Scalar.of_string s)
    | ConcreteVector (sz,ms) -> ConcreteVector (sz,List.map tr_cst ms)

  let maybevToV c = Val (tr_cst c)


  let as_symbol = function
    | Val v -> Cst.vToName v
    | Var _ -> assert false

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

  let bit_at k = function
    | Val (Concrete v) -> Val (Concrete (Cst.Scalar.bit_at k v))
    | Val (ConcreteVector _|Symbolic _|Label _|Tag _|PteVal _ as x) ->
        Warn.user_error "Illegal operation on %s" (Cst.pp_v x)
    | Var _ -> raise Undetermined

  let unop op_op op v1 = match v1 with
    | Val (Concrete i1) ->
        Val (Concrete (op i1))
    | Val (ConcreteVector _|Symbolic _|Label _|Tag _|PteVal _ as x) ->
        Warn.user_error "Illegal operation %s on %s"
          (Op.pp_op1 true op_op) (Cst.pp_v x)
    | Var _ -> raise Undetermined

  let binop op_op op v1 v2 = match v1,v2 with
  | Val (Concrete i1), Val (Concrete i2) ->
      Val (Concrete (op i1 i2))
  | Val c1, Val c2 ->
      Warn.user_error
        "Illegal operation %s on constants %s and %s"
        (Op.pp_op op_op) (Cst.pp_v c1) (Cst.pp_v c2)
  | (Var _,_)|(_,Var _)
    -> raise Undetermined

  (* Morello operators. *)
  (* NB: These may perform arithmetic on the capability of a Symbolic Virtual
   *     value, instead of the value itself. *)

  let scalar_of_cap c =
    let tag = not (Int64.equal (Int64.logand c 0x200000000L) 0L) in
    Scalar.set_tag tag (Scalar.shift_left (Scalar.of_int64 c) 95)

  let cap_of_scalar a =
    let tag = if Scalar.get_tag a then 0x200000000L else 0L in
    Int64.logor
      (Scalar.to_int64 (Scalar.shift_right_logical a 95))
      tag

  (* Concrete -> Concrete
     Symbolic -> Concrete *)
  let unop_c op_op op v = match v with
    | Val (Concrete i) ->
        Val (Concrete (op i))
    | Val (Symbolic (Virtual {cap=c;_})) ->
        Val (Concrete (op (scalar_of_cap c)))
    | Val cst ->
        Warn.user_error "Illegal operation %s on %s"
          (Op.pp_op1 true op_op) (Cst.pp_v cst)
    | Var _ -> raise Undetermined

  (* Concrete,Concrete -> Concrete
     Symbolic,Concrete -> Symbolic *)
  let binop_cs_c op_op op v1 v2 = match v1,v2 with
  | (Val (Concrete i1),Val (Concrete i2)) -> Val (Concrete (op i1 i2))
  | (Val (Symbolic (Virtual ({cap=c;_} as s))),Val (Concrete i)) ->
      Val (Symbolic (Virtual {s with cap=cap_of_scalar (op (scalar_of_cap c) i)}))
  | Val cst1,Val cst2 ->
        Warn.user_error "Illegal operation %s on %s and %s"
          (Op.pp_op op_op) (Cst.pp_v cst1) (Cst.pp_v cst2)
  | (Var _,_)|(_,Var _)
    -> raise Undetermined

  (* Concrete,Concrete -> Concrete
     Concrete,Symbolic -> Concrete *)
  let binop_c_cs op_op op v1 v2 = match v1,v2 with
  | (Val (Concrete i1),Val (Concrete i2)) -> Val (Concrete (op i1 i2))
  | (Val (Concrete i),Val (Symbolic (Virtual {cap=c;_}))) ->
      Val (Concrete (op i (scalar_of_cap c)))
  | Val cst1,Val cst2 ->
        Warn.user_error "Illegal operation %s on %s and %s"
          (Op.pp_op op_op) (Cst.pp_v cst1) (Cst.pp_v cst2)
  | (Var _,_)|(_,Var _)
    -> raise Undetermined

  let mk_val_virtual s = Val (Symbolic (Virtual s))

  (* Concrete,Concrete -> Concrete
     Concrete,Symbolic -> Concrete
     Symbolic,Concrete -> Symbolic
     Symbolic,Symbolic -> Symbolic *)
  let binop_cs_cs op_op op v1 v2 = match v1,v2 with
  | (Val (Concrete i1),Val (Concrete i2)) -> Val (Concrete (op i1 i2))
  | (Val (Concrete i),Val (Symbolic (Virtual {cap=c;_}))) ->
      Val (Concrete (op i (scalar_of_cap c)))
  | (Val (Symbolic (Virtual ({cap=c;_} as s))),Val (Concrete i)) ->
      mk_val_virtual {s with cap=cap_of_scalar (op (scalar_of_cap c) i)}
  | (Val (Symbolic (Virtual ({cap=c1;_} as s))),
     Val (Symbolic (Virtual {cap=c2;_}))) ->
      mk_val_virtual
        {s with cap=cap_of_scalar (op (scalar_of_cap c1) (scalar_of_cap c2))}
  | Val cst1,Val cst2 ->
        Warn.user_error "Illegal operation %s on %s and %s"
          (Op.pp_op op_op) (Cst.pp_v cst1) (Cst.pp_v cst2)
  | (Var _,_)|(_,Var _)
    -> raise Undetermined

  (* Concrete,Concrete -> Concrete
     Concrete,Symbolic -> Concrete
     Symbolic,Concrete -> Concrete
     Symbolic,Symbolic -> Concrete *)
  let binop_cs_cs_c op_op op v1 v2 = match v1,v2 with
  | (Val (Concrete i1),Val (Concrete i2)) -> Val (Concrete (op i1 i2))
  | (Val (Concrete i),Val (Symbolic (Virtual {cap=c;_}))) ->
      Val (Concrete (op i (scalar_of_cap c)))
  | (Val (Symbolic (Virtual {cap=c;_})),Val (Concrete i)) ->
      Val (Concrete (op (scalar_of_cap c) i))
  | (Val (Symbolic (Virtual {cap=c1;_})),Val (Symbolic (Virtual {cap=c2;_}))) ->
      Val (Concrete (op (scalar_of_cap c1) (scalar_of_cap c2)))
  | Val cst1,Val cst2 ->
        Warn.user_error "Illegal operation %s on %s and %s"
          (Op.pp_op op_op) (Cst.pp_v cst1) (Cst.pp_v cst2)
  | (Var _,_)|(_,Var _)
    -> raise Undetermined

(* specific binops, with some specific cases for symbolic constants *)

  let add v1 v2 =
(* Particular cases are important for symbolic constants *)
    if protect_is is_zero v1 then v2
    else if protect_is is_zero v2 then v1
    else match v1,v2 with
    | (Val (Concrete i1),Val (Symbolic (Virtual ({offset=i2;_} as sym))))
    | (Val (Symbolic (Virtual ({offset=i2;_} as sym))),Val (Concrete i1)) ->
        let i1 = Scalar.to_int i1 in
        Val (Symbolic (Virtual {sym with offset=i1+i2}))
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
    | (Val (PteVal _),Val (PteVal _))
      ->
        Val (Concrete (Scalar.of_int (compare  v1 v2)))
    (* 0 is sometime used as invalid PTE, no orpat because warning 57
       cannot be disabled in some versions ?  *)
    | (Val (PteVal _),Val cst)
        when Cst.eq cst Cst.zero ->
          Val (Cst.one)
    | (Val cst,Val (PteVal _))
        when Cst.eq cst Cst.zero ->
          Val (Cst.one)
    | _,_
      ->
        binop Op.Sub Scalar.sub v1 v2


  and add_konst k v = match v with
  | Val (Concrete v) -> Val (Concrete (Scalar.addk v k))
  | Val (Symbolic (Virtual ({offset=i;_} as s))) ->
    Val (Symbolic (Virtual {s with offset=i+k}))
  | Val (Symbolic (Physical (s,i))) -> Val (Symbolic (Physical (s,i+k)))
  | Val (ConcreteVector _|Symbolic (System _)|Label _|Tag _|PteVal _ as c) ->
      Warn.user_error "Illegal addition on constants %s" (Cst.pp_v c)
  | Var _ -> raise Undetermined

  and orop v1 v2 =
    if protect_is is_zero v1 then v2
    else if protect_is is_zero v2 then v1
    else
      match v1,v2 with
      | (Val (Symbolic _),Val (Symbolic _))
           -> Warn.user_error "Illegal | on %s and %s" (pp_v v1) (pp_v v2)
      | (Val (Concrete _),Val (Symbolic _)) ->
          binop_cs_cs Op.Or Scalar.logor v2 v1
      | Val (PteVal p),Val (Concrete v) ->
          (* Machine level mask operation on pteval's AArch64 specific *)
          let msg =
            sprintf
              "Illegal operation %s on constants %s and %s"
              (Op.pp_op Op.Or) (pp_v v1) (pp_v v2) in
          let p =
            if Scalar.compare v (Scalar.one) = 0 then
              { p with PTEVal.valid=1; }
            else if
              let scalar_db = Scalar.shift_left Scalar.one 7 in
              Scalar.compare v scalar_db = 0
            then
              { p with PTEVal.db=0; }
            else if
              let scalar_af = Scalar.shift_left Scalar.one 10 in
              Scalar.compare v scalar_af = 0
            then
              { p with PTEVal.af=1; }
            else if
              let scalar_dbm = Scalar.shift_left Scalar.one 51 in
              Scalar.compare v scalar_dbm = 0
            then
              { p with PTEVal.dbm=1; }
            else
              Warn.user_error "%s" msg in
          raise (Cst.Result (`AArch64,PteVal p,msg))
      | _ -> binop_cs_cs Op.Or Scalar.logor v1 v2

  and xor v1 v2 =
    if compare v1 v2 = 0 then zero else
    binop Op.Xor (Scalar.logxor) v1 v2

  and maskop op sz v = match v,sz with
  | Val (Tag _),_ -> v (* tags are small enough for any mask be idempotent *)
  | Val (PteVal _),MachSize.Quad -> v (* AArch64 specific *)
  | _ ->  unop op (Scalar.mask sz) v

  and sxtop op sz v = unop op (Scalar.sxt sz) v

  and shift_right_logical v1 v2 = match v1,v2 with
  | Val (Symbolic (Virtual {name=s;_})),Val (Concrete v) when
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

  and andnot2 v1 v2 = match v1,v2 with
  | Val (PteVal p),Val (Concrete v) ->
        let msg =
          sprintf
            "Illegal operation %s on constants %s and %s"
            (Op.pp_op Op.AndNot2) (pp_v v1) (pp_v v2) in
        let p =
           if Scalar.compare v (Scalar.one) = 0 then
             { p with PTEVal.valid=0; }
           else if
             let scalar_db = Scalar.shift_left Scalar.one 7 in
             Scalar.compare v scalar_db = 0
           then
             { p with PTEVal.db=1; }
           else if
             let scalar_af = Scalar.shift_left Scalar.one 10 in
             Scalar.compare v scalar_af = 0
           then
             { p with PTEVal.af=0; }
           else if
             let scalar_dbm = Scalar.shift_left Scalar.one 51 in
             Scalar.compare v scalar_dbm = 0
           then
             { p with PTEVal.dbm=0; }
           else
             Warn.user_error "%s" msg in
        raise (Cst.Result (`AArch64,PteVal p,msg))
  | _,_ ->
      binop Op.AndNot2
        (fun x1 x2 -> Scalar.logand x1 (Scalar.lognot x2)) v1 v2

  let bool_to_v f v1 v2 = match f v1 v2 with
  | false -> zero
  | true -> one

  let bool_to_scalar b = match b with
  | false -> Scalar.zero
  | true -> Scalar.one

  let scalar_to_bool v = Scalar.compare v Scalar.zero <> 0

  let eq v1 v2 = match v1,v2 with
  | Var i1,Var i2 when Misc.int_eq i1 i2 -> one
  | Val (Symbolic _|Label _|Tag _|PteVal _|ConcreteVector _ as s1),Val (Symbolic _|Label _|Tag _|PteVal _|ConcreteVector _ as s2) ->
      bool_to_v Cst.eq s1 s2
(* Assume concrete and others always to differ *)
  | (Val (Symbolic _|Label _|Tag _|ConcreteVector _|PteVal _), Val (Concrete _))
  | (Val (Concrete _), Val (Symbolic _|Label _|Tag _|ConcreteVector _|PteVal _)) -> zero
  | _,_ ->
      binop
        Op.Eq
        (fun s1 s2 -> bool_to_scalar (Scalar.compare s1 s2 = 0))
        v1 v2

  let ne v1 v2 = if is_zero (eq v1 v2) then one else zero

  let lt v1 v2 = match v1,v2 with
(* Need to compare symbols to zero, for setting X86_64 flags *)
    | Val (Symbolic _),Val c when Cst.eq c Cst.zero -> zero
    | Val c,Val (Symbolic _) when Cst.eq c Cst.zero -> one
    | _,_ ->
       binop Op.Lt
         (fun s1 s2 -> bool_to_scalar (Scalar.lt s1 s2)) v1 v2

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
  | Val (Symbolic (Virtual s)),Val (Tag t) ->
    Val (Symbolic (Virtual {s with tag=Some t}))
  | Val cst1,Val cst2 ->
      Warn.user_error "Illegal settag on %s and %s"
        (Cst.pp_v cst1)  (Cst.pp_v cst2)
  | (Var _,_)|(_,Var _) ->
      raise Undetermined

  let op_tagged op_op op v = match v with
  |  Val (Symbolic (Virtual ({offset=o;_} as a))) -> Val (op a o)
  |  Val (Symbolic (Physical _|System _)
          |Concrete _|Label _|Tag _|ConcreteVector _|PteVal _)
     -> Warn.user_error "Illegal tagged operation %s on %s" op_op (pp_v v)
  | Var _ -> raise Undetermined

  (*  Returns the location of the tag associated to a location *)
  let op_tagloc f {name=a;_} _ =
    Symbolic (Virtual {default_symbolic_data with name=f a})
  let capatagloc = op_tagged "capatagloc" (op_tagloc Misc.add_ctag)

  let tagloc v =  match v with
  | Val (Symbolic (Virtual {name=a;_}|Physical (a,_))) ->
       Val (Symbolic (System (TAG,a)))
  | Val (Concrete _|ConcreteVector _|Symbolic (System _)|Label _|Tag _|PteVal _) ->
      Warn.user_error "Illegal tagloc on %s" (pp_v v)
  | Var _ -> raise Undetermined

  let check_ctag = function
    | Val (Symbolic (Virtual {name=s;_})) -> Misc.check_ctag s
    | Val (Symbolic (Physical _|System _)) -> false
    | Var _
    | Val (Concrete _|ConcreteVector _|Label _|Tag _|PteVal _) ->
        Warn.fatal "Illegal check_ctag" (* NB: not an user error *)

  (* Decompose tagged locations *)
  let op_tagextract {tag=t;_} _ = match t with
  | Some t -> Tag t
  | None -> Constant.default_tag

  let tagextract v = op_tagged "tagextract" op_tagextract v

  let op_locextract {name=a;cap=c;_} o =
    Symbolic (Virtual {default_symbolic_data with name=a;cap=c;offset=o})
  let locextract v = op_tagged "locextract" op_locextract v

  let op_pte_tlb op_op op v = match v with
  |  Val (Symbolic (Virtual s)) -> Val (op s)
  |  Val (Concrete _|ConcreteVector _|Label _|Tag _|Symbolic _|PteVal _) ->
      Warn.user_error "Illegal %s on %s" op_op (pp_v v)
  | Var _ -> raise Undetermined

  let pteloc v = match v with
  | Val (Symbolic (Virtual {name=a;_})) -> Val (Symbolic (System (PTE,a)))
  | Val (Symbolic (System (PTE,a))) -> Val (Symbolic (System (PTE2,a)))
  | Val (Concrete _|ConcreteVector _|Label _|Tag _|Symbolic _|PteVal _) ->
      Warn.user_error "Illegal pteloc on %s" (pp_v v)
  | Var _ -> raise Undetermined

  let offset v = match v with
  | Val (Symbolic (Virtual {offset=o;_}|Physical (_,o))) -> intToV o
  | Val (Symbolic (System ((PTE|PTE2|TLB|TAG),_))) -> zero
  | Val (Concrete _|ConcreteVector _|Label _|Tag _|PteVal _) ->
      Warn.user_error "Illegal offset on %s" (pp_v v)
  | Var _ -> raise Undetermined

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

  let op_el0loc a = Cst.intToV a.el0
  let el0loc = op_pte_val "el0loc" op_el0loc

  let op_oaloc a = Cst.nameToV a.oa
  let oaloc = op_pte_val "oaloc" op_oaloc

  let op_tlbloc {name=a;_} = Symbolic (System (TLB,a))
  let tlbloc = op_pte_tlb "tlbloc" op_tlbloc

  let is_virtual v = match v with
  | Val c -> Constant.is_virtual c
  | Var _ -> raise Undetermined

  let as_virtual v = match v with
  | Val c -> Constant.as_virtual c
  | Var _ -> raise Undetermined

  let is_virtual_v v =  if is_virtual v then one else zero

  let andnot x1 x2 =
    Scalar.logand x1 (Scalar.lognot x2)

  let cap_perm_global = 1
  let cap_perm_mutable_load = 1 lsl 6
  let cap_perm_unseal = 1 lsl 10
  let cap_perm_seal = 1 lsl 11
  let cap_perm_store_local = 1 lsl 12
  let cap_perm_store_cap = 1 lsl 13
  let cap_perm_load_cap = 1 lsl 14
  (* let cap_perm_execute = 1 lsl 15 *)
  let cap_perm_store = 1 lsl 16
  let cap_perm_load = 1 lsl 17

  let lo64 x =
    Scalar.mask MachSize.Quad x

  let hi64 x =
    Scalar.shift_left (Scalar.shift_right_logical x 64) 64

  let ones k =
    Scalar.addk (Scalar.shift_left Scalar.one k) (-1)

  (* Check that an immediate is not bigger than a certain size *)
  let check_immediate imm sz =
    if scalar_to_bool (Scalar.shift_right_logical imm sz) then
      Warn.user_error "illegal immediate value %s" (Scalar.pp false imm)

  (* Returns the object type *)
  let cap_get_object_type c =
    Scalar.logand (Scalar.shift_right_logical c 95) (ones 15)

  (* Returns the capability c with the object type set to x *)
  let cap_set_object_type c x =
    let result = Scalar.logor (Scalar.shift_left x 95)
      (andnot c (Scalar.shift_left (ones 15) 95)) in
    Scalar.set_tag (Scalar.get_tag c) result

  (* Returns true if the input capability is sealed *)
  let cap_is_sealed c =
    scalar_to_bool (cap_get_object_type c)

  (* Returns true if a capability has all permissions in a given bit mask, false
     otherwise *)
  let cap_check_permissions c mask =
    let perms = Scalar.to_int (Scalar.shift_right_logical c 110) in
    (perms lor (lnot mask)) land 0x3ffff = 0x3ffff

  (* Returns true if the capability is local, false otherwise *)
  let cap_is_local c =
    not (scalar_to_bool (Scalar.logand (Scalar.shift_right_logical c 110) Scalar.one))

  (* Returns the input capability with permissions cleared according to a given
     bit mask *)
  let cap_clear_perms c mask =
    Scalar.set_tag (Scalar.get_tag c) (andnot c (Scalar.shift_left (Scalar.of_int mask) 110))

  (* Perform the following processing
      - If the Capability was loaded without LoadCap permission clear the tag
      - Remove MutableLoad, Store, StoreCap and StoreLocalCap permissions in a
        loaded capability if accessed without MutableLoad permission *)
  let cap_squash_post_load_cap v a =
    let mask = cap_perm_store lor cap_perm_store_cap lor
      cap_perm_store_local lor cap_perm_mutable_load in
    let v = if not (cap_check_permissions a cap_perm_load_cap)
      then Scalar.set_tag false v
      else v in
    let v = if not (cap_check_permissions a cap_perm_mutable_load) &&
        Scalar.get_tag v && not (cap_is_sealed v)
      then cap_clear_perms v mask
      else v in
    v

  (* Returns an unsealed version of the input capability *)
  let cap_unseal c =
    cap_set_object_type c Scalar.zero

  let alignd c k =
    check_immediate k 6; (* Check user input *)
    let align = ones (Scalar.to_int k) in
    let result = andnot c align in
    (* NB: bounds check skipped *)
    let tagclear = cap_is_sealed c in
    Scalar.set_tag (Scalar.get_tag c && not tagclear) result

  let alignu c k =
    check_immediate k 6; (* Check user input *)
    let align = ones (Scalar.to_int k) in
    let newvalue = andnot (Scalar.add c align) align in
    let result = Scalar.logor (hi64 c) (lo64 newvalue) in
    (* NB: bounds check skipped *)
    let tagclear = cap_is_sealed c in
    Scalar.set_tag (Scalar.get_tag c && not tagclear) result

  let capaadd v1 v2 = match v1,v2 with
    | (Val (Symbolic (Virtual ({cap=c;offset=o;_} as s))),Val (Concrete i)) ->
        let i = Scalar.to_int i in
        let c = scalar_of_cap c in
        let tagclear = cap_is_sealed c in
        let c = Scalar.set_tag (Scalar.get_tag c && not tagclear) c in
        mk_val_virtual {s with cap=cap_of_scalar c;offset=o+i}
    | (Val (Concrete c)),(Val (Concrete increment)) -> (* General case *)
        let result = Scalar.logor (hi64 c) (lo64 (Scalar.add c increment)) in
        (* NB: bounds check skipped *)
        let tagclear = cap_is_sealed c in
        Val (Concrete (Scalar.set_tag (Scalar.get_tag c && not tagclear) result))
    | Val _,Val _ ->
        Warn.user_error "Illegal capaadd on %s and %s" (pp_v v1) (pp_v v2)
    | (Var _,_)|(_,Var _)
      -> raise Undetermined

  let capasub c decrement =
    let result = Scalar.logor (hi64 c) (lo64 (Scalar.sub c decrement)) in
    (* NB: bounds check skipped *)
    let tagclear = cap_is_sealed c in
    Scalar.set_tag (Scalar.get_tag c && not tagclear) result

  let capasubs v1 v2 =
    let tag1 = if Scalar.get_tag v1 then 1 else 0 in
    let tag2 = if Scalar.get_tag v2 then 1 else 0 in
    if tag1 = tag2
      then Scalar.mask MachSize.Quad (Scalar.sub v1 v2)
      else Scalar.of_int ((tag1 - tag2) land 3)

  let check_perms perms a v =
    let conditionnal_perms x =
      if Scalar.get_tag x then
        cap_perm_store_cap lor
        if cap_is_local x then
          cap_perm_store_local
        else 0
      else 0 in
    let conditionnal_perms_stct x =
      if Scalar.compare (Scalar.bit_at 0 x) Scalar.one = 0 then
        cap_perm_store_cap
      else 0 in
    let mask = match perms with
      | "r" | "r_c" -> cap_perm_load
      | "w" -> cap_perm_store
      | "rw" -> cap_perm_load lor cap_perm_store
      | "w_c" -> cap_perm_store lor (conditionnal_perms v)
      | "rw_c" -> cap_perm_load lor cap_perm_store lor (conditionnal_perms v)
      | "tw_c" -> cap_perm_store lor (conditionnal_perms_stct v)
      | "tr_c" -> cap_perm_load_cap
      | _ -> assert false in
    if cap_check_permissions a mask then Scalar.one else Scalar.zero

  let check_seal v1 v2 =
    let otype = lo64 v2 in
    Scalar.get_tag v1 && Scalar.get_tag v2 && not (cap_is_sealed v1) &&
      not (cap_is_sealed v2) && cap_check_permissions v2 cap_perm_seal &&
      (* NB: bounds check skipped *)
      Scalar.le otype (ones 15)

  let seal v1 v2 =
    let otype = lo64 v2 in
    let tag = check_seal v1 v2 in
    let result = cap_set_object_type v1 otype in
    Scalar.set_tag tag result

  let unseal v1 v2 =
    let value = lo64 v2 in
    let otype = cap_get_object_type v1 in
    (* NB: bounds check skipped *)
    let tag = Scalar.get_tag v1 && Scalar.get_tag v2 && cap_is_sealed v1 &&
      not (cap_is_sealed v2) && cap_check_permissions v2 cap_perm_unseal &&
      Scalar.compare otype value = 0 in
    let result = cap_unseal v1 in
    let result = if not (cap_check_permissions v2 cap_perm_global)
      then cap_clear_perms result cap_perm_global
      else result in
    Scalar.set_tag tag result

  let build v1 v2 =
    let datawassealed = cap_is_sealed v1 in
    let data = if datawassealed then cap_unseal v1 else v1 in
    (* NB: bounds check skipped *)
    let tagclear = not (Scalar.get_tag v2) || cap_is_sealed v2 in
    Scalar.set_tag (not tagclear || (not datawassealed && Scalar.get_tag v1)) data

  let do_setvalue v1 v2 =
    let result = Scalar.logor (hi64 v1) (lo64 v2) in
    let tagclear = cap_is_sealed v1 in
    Scalar.set_tag (Scalar.get_tag v1 && not tagclear) result

  let setvalue v1 v2 = match v1,v2 with
    | (Val (Symbolic (Virtual {cap=c;_})),Val (Concrete i)) ->
        let c = scalar_of_cap c in
        Val (Concrete (do_setvalue c i))
    | (Val (Concrete i1)),(Val (Concrete i2)) ->
        Val (Concrete (do_setvalue i1 i2))
    | Val _,Val _ ->
        Warn.user_error "Illegal setvalue on %s and %s" (pp_v v1) (pp_v v2)
    | (Var _,_)|(_,Var _)
      -> raise Undetermined

  let clrperm c x =
    let result = andnot c (Scalar.shift_right_logical x 110) in
    let tagclear = cap_is_sealed c in
    Scalar.set_tag (Scalar.get_tag c && not tagclear) result

  let cpytype key data =
    let result = if cap_is_sealed data
      then Scalar.logor (hi64 key) (cap_get_object_type data)
      else Scalar.logor key (ones 64) in
    let tagclear = cap_is_sealed key in
    Scalar.set_tag (Scalar.get_tag key && not tagclear) result

  let cthi c x =
    let result = Scalar.logor (Scalar.shift_left x 64) (lo64 c) in
    Scalar.set_tag false result

  let cseal v1 v2 =
    let otype = lo64 v2 in
    if Scalar.compare (Scalar.logand otype (ones 15)) (ones 15) <> 0 &&
      check_seal v1 v2
    then cap_set_object_type v1 otype
    else v1

  let capastrip v = match v with
  | Val (Symbolic (Virtual s)) -> mk_val_virtual {s with cap=0L}
  | Val cst -> Warn.user_error "Illegal capastrip on %s" (Cst.pp_v cst)
  | Var _ -> raise Undetermined

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
    | Sxt sz -> sxtop op sz
    | Inv -> unop op Scalar.lognot
    | TagLoc -> tagloc
    | CapaTagLoc -> capatagloc
    | TagExtract -> tagextract
    | LocExtract -> locextract
    | UnSetXBits (nb, k) ->
        unop op
          (fun s -> logand (lognot (mask_many nb k)) s)
    | CapaGetTag -> unop_c op (fun s -> bool_to_scalar (Scalar.get_tag s))
    | CheckSealed -> unop_c op (fun s -> Scalar.logand (Scalar.shift_right_logical s 95) (ones 15))
    | CapaStrip -> capastrip
    | TLBLoc -> tlbloc
    | PTELoc -> pteloc
    | Offset -> offset
    | IsVirtual -> is_virtual_v
    | AF -> afloc
    | SetAF -> setaf
    | DB -> dbloc
    | SetDB -> setdb
    | DBM -> dbmloc
    | Valid -> validloc
    | EL0 -> el0loc
    | OA -> oaloc

  let op op = match op with
  | Add -> add
  | Alignd -> binop op alignd
  | Alignu -> binop op alignu
  | CapaAdd -> capaadd
  | Build -> binop_cs_cs op build
  | ClrPerm -> binop_cs_c op clrperm
  | CpyType -> binop_c_cs op cpytype
  | CSeal -> binop_cs_c op cseal
  | Cthi -> binop_cs_c op cthi
  | Seal -> binop_cs_c op seal
  | SetValue -> setvalue
  | CapaSub -> binop op capasub
  | CapaSubs -> binop op capasubs
  | Unseal -> binop_cs_c op unseal
  | Sub -> sub
  | Mul -> binop op (Scalar.mul)
  | Div -> binop op (Scalar.div)
  | And -> binop op (Scalar.logand)
  | ASR ->
          binop op (fun x y -> Scalar.shift_right_arithmetic x (Scalar.to_int y))
  | Or -> orop
  | Xor -> xor
  | Nor -> binop op (fun x1 x2 -> Scalar.lognot (Scalar.logor x1 x2))
  | AndNot2 -> andnot2
  | ShiftRight ->
      binop op (fun x y -> Scalar.shift_right_logical x (Scalar.to_int y))
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
  | CapaSetTag -> binop_cs_c op (fun c x -> Scalar.set_tag (scalar_to_bool x) c)
  | SquashMutable -> fun v1 v2 -> binop_cs_cs op cap_squash_post_load_cap v2 v1
  | CheckPerms perms -> binop_cs_cs_c op (check_perms perms)

  let op3 If v1 v2 v3 = match v1 with
  | Val (Concrete x) -> if scalar_to_bool x then v2 else v3
  | Val (ConcreteVector _|Symbolic _ |Label _|Tag _ | PteVal _ as s) ->
      Warn.user_error "illegal if on symbolic constant %s" (Cst.pp_v s)
  | Var _ -> raise Undetermined

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

(* Convenience *)

  let map_const f v =
    match v with
    | Var _ -> v
    | Val c -> Val (f c)

  let map_scalar f = map_const (Constant.map_scalar f)
end
