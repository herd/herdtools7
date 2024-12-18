(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

let nextsym = ref 0

let reset_gensym () = nextsym := 0

let gensym () =
  nextsym := !nextsym + 1;
  !nextsym

module
  Make
    (Cst:Constant.S)
    (ArchOp:ArchOp.S with
       type scalar = Cst.Scalar.t
       and type pteval = Cst.PteVal.t
       and type instr = Cst.Instr.t) = struct

  module Cst = Cst

  type arch_op = ArchOp.op
  type arch_extra_op1 = ArchOp.extra_op1
  type 'a arch_constr_op1 = 'a ArchOp.constr_op1
  type arch_op1 = arch_extra_op1 arch_constr_op1

  let pp_arch_op = ArchOp.pp_op
  let pp_arch_op1 = ArchOp.pp_op1

  type op1_t = arch_op1 Op.op1
  type op_t = arch_op Op.op

  open Constant

  type csym = int

  let pp_csym i = sprintf "S%i" i
  let equal_csym v1 v2 = v1 == v2
  let compare_csym v1 v2 = Misc.int_compare v1 v2


  type cst = Cst.v

  type v =
    | Var of csym
    | Val of cst

(* A symbolic constant, computations much reduced on them... *)
  let fresh_var () = Var (gensym ())

  let from_var v = Var v

(* Basic utilities *)
  let as_constant = function
    | Var _ -> None
    | Val c -> Some c

  let as_scalar v = Option.bind (as_constant v) Constant.as_scalar

  let do_pp pp_val = function
  | Var s -> pp_csym s
  | Val x -> pp_val x

  let pp hexa = do_pp (Cst.pp hexa)

  let pp_unsigned hexa = do_pp (Cst.pp_unsigned hexa)


  let pp_v =  do_pp Cst.pp_v
  let pp_v_old =  do_pp Cst.pp_v_old

  let printable = function
    | Val (c) ->
       Val (Constant.map_scalar Cst.Scalar.printable c)
    | v -> v

  let equalityPossible v1 v2 =
    match (v1,v2) with
    | Val x1,Val x2 -> Cst.compare x1 x2 = 0
    | (Var _,_)
    | (_,Var _) -> true  (* WARNING: May want to optimize later *)

  let compare v1 v2 =
    match v1,v2 with
    | Val i1,Val i2 -> Cst.compare i1 i2
    | Var i1,Var i2 -> compare_csym i1 i2
    | Val _,Var _ -> 1
    | Var _,Val _ -> -1

  let equal v1 v2 =  match v1,v2 with
  | Val i1,Val i2 -> Cst.eq i1 i2
  | Var i1,Var i2 -> equal_csym i1 i2
  | (Val _,Var _)|(Var _,Val _) -> false

  let intToV i  = Val (Cst.intToV i)
  let stringToV i  = Val (Cst.stringToV i)
  and nameToV s = Val (Cst.nameToV s)
  and instructionToV i = Val (Constant.Instruction i)
  and cstToV cst = Val cst

  let maybevToV c = Val (Cst.tr c)

  let as_symbol = function
    | Val v -> Cst.vToName v
    | Var _ -> assert false

  let freeze csym = Frozen csym

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

  let as_int = function
    | Val cst -> Cst.as_int cst
    | Var _ -> None

  let protect_is p v =  try p v with Undetermined -> false

  let bit_at k = function
    | Val (Concrete v) -> Val (Concrete (Cst.Scalar.bit_at k v))
    | Val
        (ConcreteVector _|ConcreteRecord _|Symbolic _|Label _|
         Tag _|PteVal _|Instruction _|Frozen _ as x)
      ->
        Warn.user_error "Illegal operation on %s" (Cst.pp_v x)
    | Var _ -> raise Undetermined

  let pp_unop = Op.pp_op1 true ArchOp.pp_op1

  let unop op_op op v1 =
  match v1 with
    | Val (Concrete i1) ->
        Val (Concrete (op i1))
    | Val (ConcreteVector _|ConcreteRecord _|Symbolic _|Label _|Tag _|PteVal _|Frozen _ as x) ->
        Warn.user_error "Illegal operation %s on %s"
          (pp_unop op_op) (Cst.pp_v x)
    | Val (Instruction _ as x) ->
      Warn.warn_always "FIXME: operation %s on %s suspicious with -variant self"
          (pp_unop op_op) (Cst.pp_v x) ;
      v1
    | Var _ -> raise Undetermined

  let binop op_op op v1 v2 = match v1,v2 with
  | Val (Concrete i1), Val (Concrete i2) ->
      Val (Concrete (op i1 i2))
  | Val c1, Val c2 ->
      Warn.user_error
        "Illegal operation %s on constants %s and %s"
        (Op.pp_op op_op ArchOp.pp_op) (Cst.pp_v c1) (Cst.pp_v c2)
  | (Var _,_)|(_,Var _)
    -> raise Undetermined

  (* Morello operators. *)
  (* NB: These may perform arithmetic on the capability of a Symbolic Virtual
   *     value, instead of the value itself. *)

  let scalar_of_cap c =
    let tag = not (Int64.equal (Int64.logand c 0x200000000L) 0L) in
    Cst.Scalar.set_tag tag (Cst.Scalar.shift_left (Cst.Scalar.of_int64 c) 95)

  let cap_of_scalar a =
    let tag = if Cst.Scalar.get_tag a then 0x200000000L else 0L in
    Int64.logor
      (Cst.Scalar.to_int64 (Cst.Scalar.shift_right_logical a 95))
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
          (pp_unop op_op) (Cst.pp_v cst)
    | Var _ -> raise Undetermined

  (* Concrete,Concrete -> Concrete
     Symbolic,Concrete -> Symbolic *)
  let binop_cs_c op_op op v1 v2 = match v1,v2 with
  | (Val (Concrete i1),Val (Concrete i2)) -> Val (Concrete (op i1 i2))
  | (Val (Symbolic (Virtual ({cap=c;_} as s))),Val (Concrete i)) ->
      Val (Symbolic (Virtual {s with cap=cap_of_scalar (op (scalar_of_cap c) i)}))
  | Val cst1,Val cst2 ->
        Warn.user_error "Illegal operation %s on %s and %s"
          (Op.pp_op op_op ArchOp.pp_op) (Cst.pp_v cst1) (Cst.pp_v cst2)
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
          (Op.pp_op op_op ArchOp.pp_op) (Cst.pp_v cst1) (Cst.pp_v cst2)
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
          (Op.pp_op op_op ArchOp.pp_op) (Cst.pp_v cst1) (Cst.pp_v cst2)
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
          (Op.pp_op op_op ArchOp.pp_op) (Cst.pp_v cst1) (Cst.pp_v cst2)
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
        let i1 = Cst.Scalar.to_int i1 in
        Val (Symbolic (Virtual {sym with offset=i1+i2}))
    | (Val (Concrete i1),Val (Symbolic (Physical (s,i2))))
    | (Val (Symbolic (Physical (s,i2))),Val (Concrete i1)) ->
        let i1 = Cst.Scalar.to_int i1 in
        Val (Symbolic (Physical (s,i1+i2)))
    | (Val (Symbolic _) as v,Val cst)
        when Cst.is_zero cst -> v
    | (Val cst,(Val (Symbolic _) as v))
        when Cst.is_zero cst -> v
    | _,_ -> (* General case *)
        binop Op.Add Cst.Scalar.add v1 v2

  and sub v1 v2 = (* Used for comparison for by some arch, so let us compare *)
    match v1,v2 with
    | (Val (Tag _),Val (Tag _))
    | (Val (Symbolic _),Val (Symbolic _))
    | (Val (Label _),Val (Label _))
    | (Val (PteVal _),Val (PteVal _))
    | (Val (Instruction _),Val (Instruction _))
      ->
        Val (Concrete (Cst.Scalar.of_int (compare  v1 v2)))
    (* 0 is sometime used as invalid PTE, no orpat because warning 57
       cannot be disabled in some versions ?  *)
    | (Val (PteVal _),Val cst)
        when Cst.eq cst Cst.zero ->
          Val (Cst.one)
    | (Val cst,Val (PteVal _))
        when Cst.eq cst Cst.zero ->
          Val (Cst.one)
    | (Val (Symbolic (Virtual ({offset=o;_} as sym))),Val (Concrete d)) ->
        let d = Cst.Scalar.to_int d in
        Val (Symbolic (Virtual {sym with offset=o-d}))
    | (Val (Symbolic (Physical (s,o))),Val (Concrete d)) ->
        let d = Cst.Scalar.to_int d in
        Val (Symbolic (Physical (s,o-d)))
    | _,_
      ->
        binop Op.Sub Cst.Scalar.sub v1 v2


  and add_konst k v = match v with
  | Val (Concrete v) -> Val (Concrete (Cst.Scalar.addk v k))
  | Val (Symbolic (Virtual ({offset=i;_} as s))) ->
    Val (Symbolic (Virtual {s with offset=i+k}))
  | Val (Symbolic (Physical (s,i))) -> Val (Symbolic (Physical (s,i+k)))
  | Val (ConcreteVector _|ConcreteRecord _
       | Symbolic ((TagAddr _|System _))|Label _
       |Tag _|PteVal _|Instruction _|Frozen _ as c) ->
      Warn.user_error "Illegal addition on constants %s +%d" (Cst.pp_v c) k
  | Var _ -> raise Undetermined

  and orop v1 v2 =
    if protect_is is_zero v1 then v2
    else if protect_is is_zero v2 then v1
    else
      match v1,v2 with
      | (Val (Symbolic _),Val (Symbolic _))
           -> Warn.user_error "Illegal | on %s and %s" (pp_v v1) (pp_v v2)
      | (Val (Concrete _),Val (Symbolic _)) ->
          binop_cs_cs Op.Or Cst.Scalar.logor v2 v1
      | Val (PteVal p),Val (Concrete v) ->
          (* Machine level mask operation on pteval's  *)
          begin match ArchOp.orop p v with
          | Some p ->  Val (PteVal p)
          | None ->
             Warn.user_error
               "Illegal operation %s on constants %s and %s"
               (Op.pp_op Op.Or ArchOp.pp_op) (pp_v v1) (pp_v v2)
          end
      | _ -> binop_cs_cs Op.Or Cst.Scalar.logor v1 v2

  and xor v1 v2 =
    if equal v1 v2 && Cst.Scalar.unique_zero then zero else
    match v1,v2 with
      (* Scalar constants `Concrete _` an their compositions
       * as vectors or records, cannot be checked below
       * when several zero's exist, because the value of c1 ^ c2
       * depends on scalar type. In that case one should perform
       * the exclusive or operation. See PR #970.
       *)
    | (Val (Symbolic _ as c1),Val (Symbolic _ as c2))
    | (Val (PteVal _ as c1),Val (PteVal _ as c2))
    | (Val (Instruction _ as c1),Val (Instruction _ as c2))
    | (Val (Label _ as c1),Val (Label _ as c2))
    | (Val (Tag _ as c1),Val (Tag _ as c2))
      when Cst.eq c1 c2
      -> zero
    | _ -> binop Op.Xor Cst.Scalar.logxor v1 v2

  and maskop op sz v = match v,sz with
  | Val (Tag _),_ -> v (* tags are small enough for any mask be idempotent *)
  | Val (PteVal _|Instruction _|Symbolic _|Label _ as c),_ ->
     begin
       match ArchOp.mask c sz with
       | Some c -> Val c
       | None -> unop op (Cst.Scalar.mask sz) v
     end
  | _ ->  unop op (Cst.Scalar.mask sz) v

  and sxtop op sz v = unop op (Cst.Scalar.sxt sz) v

  and shift_right_logical v1 v2 = match v1,v2 with
    | Val (Symbolic (Virtual {name=s;_})),Val (Concrete c) ->
       begin
         match ArchOp.shift_address_right s c with
         | Some c -> Val c
         | None ->
            Warn.user_error
              "Illegal operation %s on constants %s and %s"
              (Op.pp_op Op.Lsr ArchOp.pp_op) (pp_v v1) (pp_v v2)
       end

(*
         (* Beware: AArch64 only, otherwise a fatal error. *)
        raise (Cst.Result (`AArch64,Symbolic (System (TLB,s)),msg))
 *)
  | _,_ ->
      binop Op.Lsr (fun x y -> Cst.Scalar.shift_right_logical x (Cst.Scalar.to_int y))
        v1 v2

  and andop v1 v2 = match v1,v2 with
    | (Val (PteVal p),Val (Concrete v))
    | (Val (Concrete v),Val (PteVal p))
      ->
       begin match ArchOp.andop p v with
       | Some v -> Val (Concrete v)
       | None  ->
          Warn.user_error
            "Illegal operation %s on constants %s and %s"
            (Op.pp_op Op.And ArchOp.pp_op) (pp_v v1) (pp_v v2)
       end
    |  _,_ ->
        binop Op.And Cst.Scalar.logand v1 v2

  and andnot2 v1 v2 = match v1,v2 with
    | Val (PteVal p),Val (Concrete v) ->
       begin match ArchOp.andnot2 p v with
       | Some p -> Val (PteVal p)
       | None  ->
          Warn.user_error
            "Illegal operation %s on constants %s and %s"
            (Op.pp_op Op.AndNot2 ArchOp.pp_op) (pp_v v1) (pp_v v2)
       end
  | _,_ ->
      binop Op.AndNot2
        (fun x1 x2 -> Cst.Scalar.logand x1 (Cst.Scalar.lognot x2)) v1 v2

  let bool_to_v f v1 v2 = match f v1 v2 with
  | false -> zero
  | true -> one

  let bool_to_scalar b = match b with
  | false -> Cst.Scalar.zero
  | true -> Cst.Scalar.one

  let scalar_to_bool v = Cst.Scalar.compare v Cst.Scalar.zero <> 0

  let eq v1 v2 = match v1,v2 with
  | Var i1,Var i2 when Misc.int_eq i1 i2 -> one
  | Val (Symbolic _|Label _|Tag _|PteVal _|ConcreteVector _|Instruction _ as s1),Val (Symbolic _|Label _|Tag _|PteVal _|ConcreteVector _|Instruction _ as s2) ->
      bool_to_v Cst.eq s1 s2
(* Assume concrete and others always to differ *)
  | (Val (Symbolic _|Label _|Tag _|ConcreteVector _|PteVal _|Instruction _), Val (Concrete _))
  | (Val (Concrete _), Val (Symbolic _|Label _|Tag _|ConcreteVector _|PteVal _|Instruction _)) -> zero
  | _,_ ->
      binop
        Op.Eq
        (fun s1 s2 -> bool_to_scalar (Cst.Scalar.equal s1 s2))
        v1 v2

  let ne v1 v2 = if is_zero (eq v1 v2) then one else zero

  let lt v1 v2 = match v1,v2 with
(* Need to compare symbols to zero, for setting X86_64 flags *)
    | Val (Symbolic _),Val c when Cst.eq c Cst.zero -> zero
    | Val c,Val (Symbolic _) when Cst.eq c Cst.zero -> one
    | _,_ ->
       binop Op.Lt
         (fun s1 s2 -> bool_to_scalar (Cst.Scalar.lt s1 s2)) v1 v2

  let gt v1 v2 = lt v2 v1

  let le =
    binop Op.Le
      (fun s1 s2 -> bool_to_scalar (Cst.Scalar.le s1 s2))

  let ge v1 v2 = le v2 v1

  open Op

  let mask_one k = Cst.Scalar.shift_left Cst.Scalar.one k
  let mask_many nbBits k =
    let rec pow a = function (* Why Ocaml hasn't pow function in it's standard library ??? *)
      | 0 -> 1 | 1 -> a
      | n ->
          let b = pow a (n / 2) in
          b * b * (if n mod 2 = 0 then 1 else a) in
    Cst.Scalar.shift_left (Cst.Scalar.of_int ((pow 2 nbBits) - 1)) k

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
  |  Val (Symbolic (Physical _|TagAddr _|System _)
          |Concrete _|Label _
          |Tag _|ConcreteRecord _|ConcreteVector _
          |PteVal _|Instruction _
          |Frozen _)
     -> Warn.user_error "Illegal tagged operation %s on %s" op_op (pp_v v)
  | Var _ -> raise Undetermined

  (*  Returns the location of the tag associated to a location *)
  let op_tagloc f {name=a;_} _ =
    Symbolic (Virtual {default_symbolic_data with name=f a})
  let capatagloc = op_tagged "capatagloc" (op_tagloc Misc.add_ctag)

  let tagloc v =  match v with
    | Val (Symbolic (Virtual {name=a;offset=o;_}))
      ->
       Val (Symbolic (TagAddr (VIR,a,MachSize.granule_align o)))
    | Val (Symbolic (Physical (a,o)))
      ->
       Val (Symbolic (TagAddr (PHY,a,MachSize.granule_align o)))
    | Val
        (Concrete _|ConcreteRecord _|ConcreteVector _
         |Symbolic ((TagAddr _|System _))
         |Label _|Tag _|PteVal _
         |Instruction _|Frozen _)
      ->
       Warn.user_error "Illegal tagloc on %s" (pp_v v)
  | Var _ -> raise Undetermined

  let check_ctag = function
    | Val (Symbolic (Virtual {name=s;_})) -> Misc.check_ctag s
    | Val (Symbolic (Physical _|System _|TagAddr _)) -> false
    | Var _
    | Val
        (Concrete _|ConcreteRecord _|ConcreteVector _
        |Label _|Tag _
        |PteVal _|Instruction _
        |Frozen _)
      ->
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
  |  Val
       (Concrete _|ConcreteRecord _|ConcreteVector _
       |Label _|Tag _
       |Symbolic _|PteVal _
       |Instruction _|Frozen _)
     ->
      Warn.user_error "Illegal %s on %s" op_op (pp_v v)
  | Var _ -> raise Undetermined

  let pteloc v = match v with
  | Val (Symbolic (Virtual {name=a;_})) -> Val (Symbolic (System (PTE,a)))
  | Val (Symbolic (System (PTE,a))) -> Val (Symbolic (System (PTE2,a)))
  | Val
      (Concrete _|ConcreteRecord _|ConcreteVector _
      |Label _|Tag _
      |Symbolic _|PteVal _
      |Instruction _|Frozen _)
    ->
     Warn.user_error "Illegal pteloc on %s" (pp_v v)
  | Var _ -> raise Undetermined

  let illegal_offset v =
    Warn.user_error "Illegal offset on %s" @@ pp_v v

  let offset v = match v with
    | Val (Symbolic x) ->
      begin
        match Constant.get_index x with
        | Some o -> intToV o
        | None ->  illegal_offset v
      end
  | Val
      (Concrete _|ConcreteRecord _|ConcreteVector _
      |Label _|Tag _
      |PteVal _|Instruction _
      |Frozen _) ->
      illegal_offset v
  | Var _ -> raise Undetermined

  let op_tlbloc {name=a;_} = Symbolic (System (TLB,a))
  let tlbloc = op_pte_tlb "tlbloc" op_tlbloc

  let is_virtual v = match v with
  | Val c -> Constant.is_virtual c
  | Var _ -> raise Undetermined

  let as_virtual v = match v with
  | Val c -> Constant.as_virtual c
  | Var _ -> raise Undetermined

  let is_virtual_v v =  if is_virtual v then one else zero

  let is_instrloc v = match v with
  | Val c -> Constant.is_label c
  | Var _ -> false

  let is_instr_v =
    function
    | Val (Constant.Instruction _) -> one
    | Val _ -> zero
    | Var _ -> raise Undetermined

  let andnot x1 x2 =
    Cst.Scalar.logand x1 (Cst.Scalar.lognot x2)

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
    Cst.Scalar.mask MachSize.Quad x

  let hi64 x =
    Cst.Scalar.shift_left (Cst.Scalar.shift_right_logical x 64) 64

  let ones k =
    Cst.Scalar.addk (Cst.Scalar.shift_left Cst.Scalar.one k) (-1)

  (* Check that an immediate is not bigger than a certain size *)
  let check_immediate imm sz =
    if scalar_to_bool (Cst.Scalar.shift_right_logical imm sz) then
      Warn.user_error "illegal immediate value %s" (Cst.Scalar.pp false imm)

  (* Returns the object type *)
  let cap_get_object_type c =
    Cst.Scalar.logand (Cst.Scalar.shift_right_logical c 95) (ones 15)

  (* Returns the capability c with the object type set to x *)
  let cap_set_object_type c x =
    let result = Cst.Scalar.logor (Cst.Scalar.shift_left x 95)
      (andnot c (Cst.Scalar.shift_left (ones 15) 95)) in
    Cst.Scalar.set_tag (Cst.Scalar.get_tag c) result

  (* Returns true if the input capability is sealed *)
  let cap_is_sealed c =
    scalar_to_bool (cap_get_object_type c)

  (* Returns true if a capability has all permissions in a given bit mask, false
     otherwise *)
  let cap_check_permissions c mask =
    let perms = Cst.Scalar.to_int (Cst.Scalar.shift_right_logical c 110) in
    (perms lor (lnot mask)) land 0x3ffff = 0x3ffff

  (* Returns true if the capability is local, false otherwise *)
  let cap_is_local c =
    not (scalar_to_bool (Cst.Scalar.logand (Cst.Scalar.shift_right_logical c 110) Cst.Scalar.one))

  (* Returns the input capability with permissions cleared according to a given
     bit mask *)
  let cap_clear_perms c mask =
    Cst.Scalar.set_tag (Cst.Scalar.get_tag c) (andnot c (Cst.Scalar.shift_left (Cst.Scalar.of_int mask) 110))

  (* Perform the following processing
      - If the Capability was loaded without LoadCap permission clear the tag
      - Remove MutableLoad, Store, StoreCap and StoreLocalCap permissions in a
        loaded capability if accessed without MutableLoad permission *)
  let cap_squash_post_load_cap v a =
    let mask = cap_perm_store lor cap_perm_store_cap lor
      cap_perm_store_local lor cap_perm_mutable_load in
    let v = if not (cap_check_permissions a cap_perm_load_cap)
      then Cst.Scalar.set_tag false v
      else v in
    let v = if not (cap_check_permissions a cap_perm_mutable_load) &&
        Cst.Scalar.get_tag v && not (cap_is_sealed v)
      then cap_clear_perms v mask
      else v in
    v

  (* Returns an unsealed version of the input capability *)
  let cap_unseal c =
    cap_set_object_type c Cst.Scalar.zero

  let alignd c k =
    check_immediate k 6; (* Check user input *)
    let align = ones (Cst.Scalar.to_int k) in
    let result = andnot c align in
    (* NB: bounds check skipped *)
    let tagclear = cap_is_sealed c in
    Cst.Scalar.set_tag (Cst.Scalar.get_tag c && not tagclear) result

  let alignu c k =
    check_immediate k 6; (* Check user input *)
    let align = ones (Cst.Scalar.to_int k) in
    let newvalue = andnot (Cst.Scalar.add c align) align in
    let result = Cst.Scalar.logor (hi64 c) (lo64 newvalue) in
    (* NB: bounds check skipped *)
    let tagclear = cap_is_sealed c in
    Cst.Scalar.set_tag (Cst.Scalar.get_tag c && not tagclear) result

  let capaadd v1 v2 = match v1,v2 with
    | (Val (Symbolic (Virtual ({cap=c;offset=o;_} as s))),Val (Concrete i)) ->
        let i = Cst.Scalar.to_int i in
        let c = scalar_of_cap c in
        let tagclear = cap_is_sealed c in
        let c = Cst.Scalar.set_tag (Cst.Scalar.get_tag c && not tagclear) c in
        mk_val_virtual {s with cap=cap_of_scalar c;offset=o+i}
    | (Val (Concrete c)),(Val (Concrete increment)) -> (* General case *)
        let result = Cst.Scalar.logor (hi64 c) (lo64 (Cst.Scalar.add c increment)) in
        (* NB: bounds check skipped *)
        let tagclear = cap_is_sealed c in
        Val (Concrete (Cst.Scalar.set_tag (Cst.Scalar.get_tag c && not tagclear) result))
    | Val _,Val _ ->
        Warn.user_error "Illegal capaadd on %s and %s" (pp_v v1) (pp_v v2)
    | (Var _,_)|(_,Var _)
      -> raise Undetermined

  let capasub c decrement =
    let result = Cst.Scalar.logor (hi64 c) (lo64 (Cst.Scalar.sub c decrement)) in
    (* NB: bounds check skipped *)
    let tagclear = cap_is_sealed c in
    Cst.Scalar.set_tag (Cst.Scalar.get_tag c && not tagclear) result

  let capasubs v1 v2 =
    let tag1 = if Cst.Scalar.get_tag v1 then 1 else 0 in
    let tag2 = if Cst.Scalar.get_tag v2 then 1 else 0 in
    if tag1 = tag2
      then Cst.Scalar.mask MachSize.Quad (Cst.Scalar.sub v1 v2)
      else Cst.Scalar.of_int ((tag1 - tag2) land 3)

  let check_perms perms a v =
    let conditionnal_perms x =
      if Cst.Scalar.get_tag x then
        cap_perm_store_cap lor
        if cap_is_local x then
          cap_perm_store_local
        else 0
      else 0 in
    let conditionnal_perms_stct x =
      if Cst.Scalar.compare (Cst.Scalar.bit_at 0 x) Cst.Scalar.one = 0 then
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
    if cap_check_permissions a mask then Cst.Scalar.one else Cst.Scalar.zero

  let check_seal v1 v2 =
    let otype = lo64 v2 in
    Cst.Scalar.get_tag v1 && Cst.Scalar.get_tag v2 && not (cap_is_sealed v1) &&
      not (cap_is_sealed v2) && cap_check_permissions v2 cap_perm_seal &&
      (* NB: bounds check skipped *)
      Cst.Scalar.le otype (ones 15)

  let seal v1 v2 =
    let otype = lo64 v2 in
    let tag = check_seal v1 v2 in
    let result = cap_set_object_type v1 otype in
    Cst.Scalar.set_tag tag result

  let unseal v1 v2 =
    let value = lo64 v2 in
    let otype = cap_get_object_type v1 in
    (* NB: bounds check skipped *)
    let tag = Cst.Scalar.get_tag v1 && Cst.Scalar.get_tag v2 && cap_is_sealed v1 &&
      not (cap_is_sealed v2) && cap_check_permissions v2 cap_perm_unseal &&
      Cst.Scalar.compare otype value = 0 in
    let result = cap_unseal v1 in
    let result = if not (cap_check_permissions v2 cap_perm_global)
      then cap_clear_perms result cap_perm_global
      else result in
    Cst.Scalar.set_tag tag result

  let build v1 v2 =
    let datawassealed = cap_is_sealed v1 in
    let data = if datawassealed then cap_unseal v1 else v1 in
    (* NB: bounds check skipped *)
    let tagclear = not (Cst.Scalar.get_tag v2) || cap_is_sealed v2 in
    Cst.Scalar.set_tag (not tagclear || (not datawassealed && Cst.Scalar.get_tag v1)) data

  let do_setvalue v1 v2 =
    let result = Cst.Scalar.logor (hi64 v1) (lo64 v2) in
    let tagclear = cap_is_sealed v1 in
    Cst.Scalar.set_tag (Cst.Scalar.get_tag v1 && not tagclear) result

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
    let result = andnot c (Cst.Scalar.shift_right_logical x 110) in
    let tagclear = cap_is_sealed c in
    Cst.Scalar.set_tag (Cst.Scalar.get_tag c && not tagclear) result

  let cpytype key data =
    let result = if cap_is_sealed data
      then Cst.Scalar.logor (hi64 key) (cap_get_object_type data)
      else Cst.Scalar.logor key (ones 64) in
    let tagclear = cap_is_sealed key in
    Cst.Scalar.set_tag (Cst.Scalar.get_tag key && not tagclear) result

  let cthi c x =
    let result = Cst.Scalar.logor (Cst.Scalar.shift_left x 64) (lo64 c) in
    Cst.Scalar.set_tag false result

  let cseal v1 v2 =
    let otype = lo64 v2 in
    if Cst.Scalar.compare (Cst.Scalar.logand otype (ones 15)) (ones 15) <> 0 &&
      check_seal v1 v2
    then cap_set_object_type v1 otype
    else v1

  let capastrip v = match v with
  | Val (Symbolic (Virtual s)) -> mk_val_virtual {s with cap=0L}
  | Val cst -> Warn.user_error "Illegal capastrip on %s" (Cst.pp_v cst)
  | Var _ -> raise Undetermined

  let optointeger v1 v2 =
    match v1,v2 with
    | Val (Concrete _),_ -> v1
    | (Var _,_)|(_,Var _) -> raise Undetermined
    | Val _,Val (Concrete _) -> v2
    | _,_ ->
       Warn.user_error "Illegal ToInteger on %s and %s" (pp_v v1) (pp_v v2)

  let op1 op =
    let open! Cst.Scalar in
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
            bool_to_scalar (Cst.Scalar.compare (logand (mask_one k) s) zero <> 0))
    | LogicalRightShift 0
    | LeftShift 0
    | AddK 0 -> fun s -> s
    | LeftShift k ->
        unop  op (fun s -> Cst.Scalar.shift_left s k)
    | LogicalRightShift k ->
        unop op (fun s -> Cst.Scalar.shift_right_logical s k)
    | ArithRightShift k ->
        unop op (fun s -> Cst.Scalar.shift_right_arithmetic s k)
    | AddK k -> add_konst k
    | AndK k -> unop op (fun s -> Cst.Scalar.logand s (Cst.Scalar.of_string k))
    | Mask sz -> maskop op sz
    | Sxt sz -> sxtop op sz
    | Rbit sz ->
       let module R = Rbit.Make(Cst.Scalar) in
       unop op (R.rbit sz)
    | RevBytes (csz,sz) ->
       let module R = Rbit.Make(Cst.Scalar) in
       unop op (R.revbytes csz sz)
    | Inv -> unop op Cst.Scalar.lognot
    | Abs -> unop op Cst.Scalar.abs
    | TagLoc -> tagloc
    | CapaTagLoc -> capatagloc
    | TagExtract -> tagextract
    | LocExtract -> locextract
    | UnSetXBits (nb, k) ->
        unop op
          (fun s -> logand (lognot (mask_many nb k)) s)
    | CapaGetTag -> unop_c op (fun s -> bool_to_scalar (Cst.Scalar.get_tag s))
    | CheckSealed -> unop_c op (fun s -> Cst.Scalar.logand (Cst.Scalar.shift_right_logical s 95) (ones 15))
    | CapaStrip -> capastrip
    | TLBLoc -> tlbloc
    | PTELoc -> pteloc
    | Offset -> offset
    | IsVirtual -> is_virtual_v
    | IsInstr -> is_instr_v
    | Promote -> unop op Cst.Scalar.promote
    | Demote -> unop op Cst.Scalar.demote
    | ArchOp1 op ->
        (function
         | Var _ -> raise Undetermined
         | Val c as v ->
             begin
               match ArchOp.do_op1 op c with
               | None ->
                   Warn.user_error "Illegal arch operation %s on %s"
                     (ArchOp.pp_op1 true op) (pp_v v)
               | Some c -> Val c
             end)

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
  | Mul -> binop op (Cst.Scalar.mul)
  | Div -> binop op (Cst.Scalar.div)
  | Rem -> binop op (Cst.Scalar.rem)
  | And -> andop
  | ASR ->
          binop op (fun x y -> Cst.Scalar.shift_right_arithmetic x (Cst.Scalar.to_int y))
  | Or -> orop
  | Xor -> xor
  | Nor -> binop op (fun x1 x2 -> Cst.Scalar.lognot (Cst.Scalar.logor x1 x2))
  | AndNot2 -> andnot2
  | ShiftRight ->
      binop op (fun x y -> Cst.Scalar.shift_right_logical x (Cst.Scalar.to_int y))
  | ShiftLeft ->
      binop op (fun x y -> Cst.Scalar.shift_left x (Cst.Scalar.to_int y))
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
        (fun x y -> if Cst.Scalar.lt x y then y else x)
  | Min ->
      binop op
        (fun x y -> if Cst.Scalar.lt x y then x else y)
  | UMax ->
      binop op
        (fun x y -> if Cst.Scalar.unsigned_compare x y < 0 then y else x)
  | UMin ->
      binop op
        (fun x y -> if Cst.Scalar.unsigned_compare x y < 0 then x else y)
  | SetTag -> settag
  | CapaSetTag -> binop_cs_c op (fun c x -> Cst.Scalar.set_tag (scalar_to_bool x) c)
  | SquashMutable -> fun v1 v2 -> binop_cs_cs op cap_squash_post_load_cap v2 v1
  | CheckPerms perms -> binop_cs_cs_c op (check_perms perms)
  | ToInteger -> optointeger
  | ArchOp o -> (
      fun v1 v2 ->
        match (v1, v2) with
        | Var _, _ | _, Var _ -> raise Undetermined
        | Val c1, Val c2 -> (
            match ArchOp.do_op o c1 c2 with
            | Some c -> Val c
            | None ->
                Warn.user_error "Illegal operation %s on %s and %s"
                  (ArchOp.pp_op o) (pp_v v1) (pp_v v2)))

  let op3 If v1 v2 v3 = match v1 with
  | Val (Concrete x) -> if scalar_to_bool x then v2 else v3
  | Val
      (ConcreteVector _|ConcreteRecord _|Symbolic _
      |Label _|Tag _
      |PteVal _|Instruction _
      | Frozen _ as s) ->
      Warn.user_error "illegal if on symbolic constant %s" (Cst.pp_v s)
  | Var _ -> raise Undetermined

  module OrderedValue = struct
    type t = v
    let compare = compare
  end

  module ValueSet = MySet.Make(OrderedValue)
  module ValueMap = MyMap.Make(OrderedValue)

  module OrderedVar = struct
    type t = csym
    let compare = compare_csym
  end

  module Solution = Map.Make(OrderedVar)

  type solution = v Solution.t

  let is_var_determined v = match v with
  | Var _ -> false
  | Val _ -> true

  let undetermined_vars v = match v with
  | Var _ -> ValueSet.singleton v
  | Val _ -> ValueSet.empty


  let map_csym f v =
    match v with
    | Var x -> f x
    | Val _ -> v

  let simplify_var soln v =
    match v with
    | Var x | Val (Constant.Frozen x) -> (
        try Solution.find x soln with Not_found -> Var x)
    | _ -> v

  (* Convenience *)


  let map_const f v =
    match v with
    | Var _ -> v
    | Val c -> Val (f c)

  let map_scalar f = map_const (Constant.map_scalar f)


(* Lift constant location classification *)
  let access_of_value = function
  | Var _ -> assert false
  | Val cst -> Cst.access_of_constant cst


end
