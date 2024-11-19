(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2021-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

type 'op1 unop =
  | AF (* get AF from PTE entry *)
  | SetAF (* set AF to 1 in PTE entry *)
  | DB (* get DB from PTE entry *)
  | SetDB (* set DB to 1 in PTE entry *)
  | DBM (* get DBM from PTE entry *)
  | Valid (* get Valid bit from PTE entry *)
  | EL0 (* get EL0 bit from PTE entry *)
  | OA (* get OA from PTE entry *)
  | Tagged (* get Tag attribute from PTE entry *)
  | MakeCanonical (* Make a virtual address canonical *)
  | AddErrorCode of PAC.key (* Add a PAC error code in a virtual address *)
  | Extra1 of 'op1

type 'op binop =
  | Extra of 'op
  (* Add a PAC field to a virtual address using a modifier and a key *)
  | AddPAC of bool * PAC.key
  (* Test if two variabels may be equals modulo the equality of two PAC fields *)
  | CollisionPossible
  (* Assume the presence of a hash collision *)
  | AssumeCollision
  (* Assume the abscence of a hash collision *)
  | AssumeNoCollision

type predicate
  = Eq of PAC.t * PAC.t
  | Ne of PAC.t * PAC.t

module
   Make
     (S:Scalar.S)
     (Extra:ArchOp.S with type scalar = S.t) : ArchOp.S
   with type extra_op1 = Extra.op1
    and type 'a constr_op1 = 'a unop
    and type extra_op = Extra.op
    and type 'a constr_op = 'a binop
    and type scalar = S.t
    and type pteval = AArch64PteVal.t
    and type instr = AArch64Base.instruction
    and type predicate = predicate
  = struct

    type extra_op = Extra.op
    type 'a constr_op = 'a binop
    type op = Extra.op binop
    type extra_op1 = Extra.op1
    type 'a constr_op1 = 'a unop
    type op1 = extra_op1 constr_op1

    let pp_op = function
      | Extra extra -> Extra.pp_op extra
      | AddPAC (uniq, key) ->
          if uniq then
            Printf.sprintf "AddOnePac:%s" (PAC.pp_upper_key key)
          else
            Printf.sprintf "AddPAC:%s" (PAC.pp_upper_key key)
      | CollisionPossible -> "CollisionPossible"
      | AssumeCollision -> "AssumeCollision"
      | AssumeNoCollision -> "AssumeNoCollision"

    let pp_op1 hexa = function
      | AF -> "AF"
      | SetAF -> "SetAF"
      | DB -> "DB"
      | SetDB -> "SetDB"
      | DBM -> "DBM"
      | Valid -> "Valid"
      | EL0 -> "EL0"
      | OA -> "OA"
      | Tagged -> "Tagged"
      | MakeCanonical -> "MakeCanonical"
      | AddErrorCode k -> Printf.sprintf "ErrorCode:%s" (PAC.pp_upper_key k)
      | Extra1 op1 -> Extra.pp_op1 hexa op1

    type scalar = S.t
    type pteval = AArch64PteVal.t
    type instr = AArch64Base.instruction
    type cst = (scalar,pteval,instr) Constant.t

    type nonrec predicate = predicate
    exception Constraint of predicate * cst

    let compare_predicate eq1 eq2 =
      match eq1,eq2 with
      | Eq (p1,p2), Eq (p3,p4)
      | Ne (p1,p2), Ne (p3,p4) -> begin
        match PAC.compare p1 p3 with
        | 0 -> PAC.compare p2 p4
        | r -> r
      end
      | Eq _, Ne _ -> -1
      | Ne _, Eq _ -> 1

    let pp_predicate = function
      | Eq (p1,p2) -> begin
        match PAC.get_name p1,PAC.get_name p2 with
        | Some name, _ | _, Some name ->
            Printf.sprintf "%s == %s" (PAC.pp p1 name 0) (PAC.pp p2 name 0)
        | _, _ ->
            "true"
      end
      | Ne (p1,p2) -> begin
        match PAC.get_name p1,PAC.get_name p2 with
        | Some name, _ | _, Some name ->
            Printf.sprintf "%s != %s" (PAC.pp p1 name 0) (PAC.pp p2 name 0)
        | _, _ ->
            "false"
      end

    let inverse_predicate = function
      | Eq (p1,p2) -> Ne (p1,p2)
      | Ne (p1,p2) -> Ne (p1,p2)

    let eq_satisfiable c1 c2 =
      match Constant.collision c1 c2 with
      | Some (p1,p2) -> Some (Eq (p1,p2))
      | None -> None

    let pp_cst hexa v =
      let module InstrPP = AArch64Base.MakePP(struct
        let is_morello = true
      end) in
      Constant.pp
        (S.pp hexa)
        (AArch64PteVal.pp hexa)
        (InstrPP.dump_instruction)
        v

    open AArch64PteVal

    let boolToCst =
      let open Constant in
      let zero = Concrete S.zero
      and one = Concrete S.one in
      fun b -> if b then one else zero

    let op_get_pteval op (v:cst) =
      let open Constant in
      match v with
      | PteVal p -> Some (boolToCst (op p))
      | _ -> None

    let op_set_pteval op (v:cst) =
      let open Constant in
      match v with
      | PteVal p -> Some (PteVal (op p))
      | _ -> None

    let assumeContradiction () =
      let open Constant in
      let one = Concrete S.one in
      raise (Constraint (Ne (PAC.canonical,PAC.canonical),one))

    let assumeNoCollision c1 c2 =
      let open Constant in
      let one = Concrete S.one in
      match collision c1 c2 with
      | Some (p1,p2) ->
          raise (Constraint (Ne (p1,p2),one))
      | None ->
          let instr_eq i1 i2 = Misc.polymorphic_compare i1 i2 = 0 in
          if Constant.eq S.equal AArch64PteVal.eq instr_eq c1 c2
          then assumeContradiction ()
          else Some one

    let assumeCollision c1 c2 =
      let open Constant in
      let one = Concrete S.one in
      match collision c1 c2 with
      | Some (p1,p2) ->
          raise (Constraint (Eq (p1,p2),one))
      | _ ->
          let instr_eq i1 i2 = Misc.polymorphic_compare i1 i2 = 0 in
          if Constant.eq S.equal AArch64PteVal.eq instr_eq c1 c2
          then Some one
          else assumeContradiction ()

    let addErrorCode key =
      let open Constant in function
      | Symbolic (Virtual ({name} as v)) ->
          let pac = PAC.error name key in
          Some (Symbolic (Virtual {v with pac}))
      | _ ->
          None

    (* Remove the PAC field of a virtual address *)
    let makeCanonical cst =
      if Constant.is_virtual cst
      then Some (Constant.make_canonical cst)
      else None

    let getaf = op_get_pteval (fun p -> p.af <> 0)
    let setaf = op_set_pteval (fun p -> { p with af=1; })

    let getdb = op_get_pteval (fun p -> p.db <> 0)
    let setdb = op_set_pteval (fun p -> { p with db=1; })

    let getdbm = op_get_pteval (fun p -> p.dbm <> 0)

    let getvalid = op_get_pteval (fun p -> p.valid <> 0)

    let getel0 = op_get_pteval (fun p -> p.el0 <> 0)

    let gettagged = op_get_pteval (fun p -> Attrs.mem "TaggedNormal" p.attrs)

    let getoa v =
      let open Constant in
      match v with
      | PteVal {oa;_} -> Some (Symbolic (oa2symbol oa))
      | _ -> None

    let exit _ = raise Exit
    let toExtra cst = Constant.map Misc.identity exit exit cst
    and fromExtra cst = Constant.map Misc.identity exit exit cst

    (* Add a PAC field to a virtual address, this function can only add a PAC
       field if the input pointer is canonical, otherwise it raise an error, it is
       used to model the `pac*` instruction without the variant const-pac-field *)
    let addOnePAC key pointer modifier =
      let open Constant in
      match pointer with
      | Symbolic (Virtual {pac}) when not (PAC.is_canonical pac) ->
          None
      | Symbolic (Virtual ({name; pac; offset} as v)) ->
        let modifier = pp_cst true modifier in
        let pac = PAC.add name key modifier offset pac in
        Some (Symbolic (Virtual {v with pac}))
      | _ ->
          None

    (* Add a PAC field to a virtual address, this function can add a PAC field if
      the input pointer already contain a PAC field, in this case it use the XOR
      of the two pac fields, it is use in the `auth*` function and in the
      `pac*` instruction in presence of the variant const-pac-field *)
    let addPAC key pointer modifier =
      let open Constant in
      match pointer with
      | Symbolic (Virtual ({name; pac; offset} as v)) ->
        let modifier = pp_cst true modifier in
        let pac = PAC.add name key modifier offset pac in
        Some (Symbolic (Virtual {v with pac}))
      | _ -> None

    (* Return if a hahs collision between two different PAC field is possible,
       reutrn zero if the PAC fields are syntactically differents *)
    let collisionPossible (c1:cst) (c2:cst) =
      let open Constant in
      let zero = Concrete S.zero
      and one = Concrete S.one in
      match Constant.collision c1 c2 with
      | Some _ ->
          Some one
      | None ->
          Some zero

    let do_op = function
      | AddPAC (true, key) -> addOnePAC key
      | CollisionPossible -> collisionPossible
      | AddPAC (false, key) -> addPAC key
      | AssumeCollision -> assumeCollision
      | AssumeNoCollision -> assumeNoCollision
      | Extra op -> fun c1 c2 ->
          try
            match Extra.do_op op (toExtra c1) (toExtra c2) with
            | None -> None
            | Some cst -> Some (fromExtra cst)
          with Exit -> None


    let do_op1 = function
      | AF -> getaf
      | SetAF -> setaf
      | DB -> getdb
      | SetDB -> setdb
      | DBM -> getdbm
      | Valid -> getvalid
      | EL0 -> getel0
      | OA -> getoa
      | Tagged -> gettagged
      | MakeCanonical -> makeCanonical
      | AddErrorCode k -> addErrorCode k
      | Extra1 op1 ->
         fun cst ->
           try
             match Extra.do_op1 op1 (toExtra cst) with
             | None -> None
             | Some cst -> Some (fromExtra cst)
           with Exit -> None


    let shift_address_right s c =
      let open Constant in
      if S.equal (S.of_int 12) c then Some (Symbolic (System (TLB,s)))
      else None

    let mask_valid = S.one
    let mask_el0 = S.shift_left S.one 6
    let mask_db = S.shift_left S.one 7
    let mask_af = S.shift_left S.one 10
    let mask_dbm = S.shift_left S.one 51
    let mask_all_neg =
      S.lognot
        (S.logor mask_el0
           (S.logor
              (S.logor mask_valid  mask_db)
              (S.logor  mask_af  mask_dbm)))

    let is_zero v = S.equal S.zero v
    let is_set v m = not (is_zero (S.logand v m))

    let orop p m =
      if is_set m mask_all_neg then None
      else
        let p = if is_set m mask_valid then { p with valid=1; } else p in
        let p = if is_set m mask_el0 then { p with el0=1; } else p in
        let p = if is_set m mask_db then { p with db=0; } else p in
        let p = if is_set m mask_af then { p with af=1; } else p in
        let p = if is_set m mask_dbm then { p with dbm=1; } else p in
        Some p

    and andnot2 p m =
      if is_set m mask_all_neg then None
      else
        let p = if is_set m mask_valid then { p with valid=0; } else p in
        let p = if is_set m mask_el0 then { p with el0=0; } else p in
        let p = if is_set m mask_db then { p with db=1; } else p in
        let p = if is_set m mask_af then { p with af=0; } else p in
        let p = if is_set m mask_dbm then { p with dbm=0; } else p in
        Some p

    and andop p m =
      let r = S.zero in
      let r =
        if is_set m mask_valid && p.valid=1
        then S.logor r mask_valid else r  in
      let r =
        if is_set m mask_el0 && p.el0=1
        then S.logor r mask_el0 else r  in
      let r =
        if is_set m mask_db && p.db=0;
        then S.logor r mask_db else r  in
      let r =
        if is_set m mask_af &&  p.af=1;
        then S.logor r mask_af else r  in
      let r =
        if is_set m mask_dbm && p.dbm=1
        then S.logor r mask_dbm else r  in
      Some r


    let mask c sz =
      let open MachSize in
      let open Constant in
      match c,sz with
(* The following are 64bits quantities, the last two being virtual addresses *)
      | ((PteVal _|Symbolic _|Label _),Quad)
(* Non-signed 32bit quantity *)
      | (Instruction _,(Word|Quad))
        -> Some c
      | _,_ -> None
  end
