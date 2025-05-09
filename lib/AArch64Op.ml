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
  | CheckCanonical (* Check is a virtual address is canonical *)
  | MakeCanonical (* Make a virtual address canonical *)
  | GICGetIntid (* Get the INTID location from an IntidUpdateVal *)
  | GICGetField of string (* Get the the update fields from an IntidUpdateVal *)
  | GICSetIntid
  | IntidSetAct of bool
  | IntidGetAct
  | IntidSetPend of bool
  | IntidGetPend
  | IntidSetEnabled of bool
  | IntidGetEnabled
  | IntidSetPri of int
  | IntidGetPri
  | IntidSetAff of int
  | IntidGetAff
  | IntidSetHM of int
  | IntidIsEdge
  | Extra1 of 'op1

type 'op binop =
  | Extra of 'op
  | AddPAC of bool * PAC.key

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
    and type intidval = AArch64IntidVal.t
    and type instr = AArch64Base.instruction
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
      | CheckCanonical -> "CheckCanonical"
      | MakeCanonical -> "MakeCanonical"
      | GICGetIntid -> "GICGetIntid"
      | GICGetField s -> "GICGetField:" ^ s
      | GICSetIntid -> "GICSetIntid"
      | IntidSetAct v -> "IntidSetAct:" ^ (string_of_bool v)
      | IntidGetAct -> "IntidGetAct"
      | IntidSetPend v -> "IntidSetPend:" ^ (string_of_bool v)
      | IntidGetPend -> "IntidGetPend"
      | IntidSetEnabled v -> "IntidSetEnabled:" ^ (string_of_bool v)
      | IntidGetEnabled -> "IntidGetEnabled"
      | IntidSetPri v -> "IntidSetPri:" ^ (string_of_int v)
      | IntidGetPri -> "IntidGetPri"
      | IntidSetAff v -> "IntidSetAff:" ^ (string_of_int v)
      | IntidGetAff -> "IntidGetAff"
      | IntidSetHM v -> "IntidSetHM:" ^ (string_of_int v)
      | IntidIsEdge -> "IntidIsEdge"
      | Extra1 op1 -> Extra.pp_op1 hexa op1

    type scalar = S.t
    type pteval = AArch64PteVal.t
    type intidval = AArch64IntidVal.t
    type instr = AArch64Base.instruction
    type cst = (scalar,pteval,intidval,instr) Constant.t

    let pp_cst hexa v =
      let module InstrPP = AArch64Base.MakePP(struct
        let is_morello = true
      end) in
      Constant.pp (S.pp hexa) (AArch64PteVal.pp hexa)
        (AArch64IntidVal.pp) (InstrPP.dump_instruction) v

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

    (* Check that the PAC field of a virtual address is canonical *)
    let checkCanonical =
      let open Constant in function
      | Symbolic (Virtual {pac}) ->
          Some (boolToCst (PAC.is_canonical pac))
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

    let op_get_intid_update_val op = function
      | Constant.IntidUpdateVal v -> Some (op v)
      | _ -> None

    let get_gicintid = op_get_intid_update_val
        (fun v -> (Constant.mk_sym_intid (Option.get v.IntidUpdateVal.intid)))

    let get_gicfield field =
      let f v =
        match v.IntidUpdateVal.field with
          | Some (f, v) when String.equal f field ->
            let open AArch64IntidVal in
            let c =
              if String.equal field HM.label then
                S.of_int (HM.of_string v)
              else
                S.of_string v in
            Constant.Concrete c
          | _ ->
            Warn.user_error
              "No field named %s in %s" field (IntidUpdateVal.pp v) in
      op_get_intid_update_val f

    let gic_setintid v =
      let open Constant in
      let open IntidUpdateVal in
      match v with
      | Symbolic (System (INTID, s)) ->
        Some (IntidUpdateVal({intid=Some s; field=Some ("valid", "1")}))
      | _ -> None

    let op_get_intid_field op v =
      let open Constant in
      match v with
      | IntidVal i -> Some (Concrete (op i))
      | _ -> None

    let op_set_intid op v =
      let open Constant in
      match v with
      | IntidVal i -> Some (IntidVal (op i))
      | _ -> None

    let intid_set_act v =
      op_set_intid (fun i -> {i with AArch64IntidVal.active=v})

    let intid_get_act () =
      op_get_intid_field (fun i -> if i.AArch64IntidVal.active then S.one else S.zero)

    let intid_set_pend v =
      op_set_intid (fun i -> {i with AArch64IntidVal.pending=v})

    let intid_get_pend () =
      op_get_intid_field (fun i -> if i.AArch64IntidVal.pending then S.one else S.zero)

    let intid_set_enabled v =
      op_set_intid (fun i -> {i with AArch64IntidVal.enabled=v})

    let intid_get_enabled () =
      op_get_intid_field (fun i -> if i.AArch64IntidVal.enabled then S.one else S.zero)

    let intid_set_pri v =
      op_set_intid (fun i -> {i with AArch64IntidVal.priority=v})

    let intid_get_pri () =
      let open Constant in
      op_get_intid_field (fun i -> S.of_int (i.AArch64IntidVal.priority))

    let intid_set_aff v =
      op_set_intid (fun i -> {i with AArch64IntidVal.target=v})

    let intid_get_aff () =
      let open Constant in
      op_get_intid_field (fun i -> S.of_int i.AArch64IntidVal.target)

    let intid_set_hm v =
      op_set_intid (fun i -> {i with AArch64IntidVal.hm=v})

    let intid_is_edge () =
      let open Constant in
      let open AArch64IntidVal.HM in
      op_get_intid_field (fun i -> if (is_edge i.AArch64IntidVal.hm) then S.one else S.zero)


    let exit _ = raise Exit
    let toExtra cst = Constant.map Misc.identity exit exit exit cst
    and fromExtra cst = Constant.map Misc.identity exit exit exit cst

    (* Add a PAC field to a virtual address, this function can only add a PAC
       field if the input pointer is canonical, otherwise it raise an error, it is
       used to model the `pac*` instruction without the variant const-pac-field *)
    let addOnePAC key pointer modifier =
      let open Constant in
      match pointer with
      | Symbolic (Virtual {pac}) when not (PAC.is_canonical pac) ->
          None
      | Symbolic (Virtual ({pac; offset} as v)) ->
        let modifier = pp_cst true modifier in
        let pac = PAC.add key modifier offset pac in
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
      | Symbolic (Virtual ({pac; offset} as v)) ->
        let modifier = pp_cst true modifier in
        let pac = PAC.add key modifier offset pac in
        Some (Symbolic (Virtual {v with pac}))
      | _ -> None

    let do_op = function
      | AddPAC (true, key) -> addOnePAC key
      | AddPAC (false, key) -> addPAC key
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
      | CheckCanonical -> checkCanonical
      | MakeCanonical -> makeCanonical
      | GICGetIntid -> get_gicintid
      | GICSetIntid -> gic_setintid
      | GICGetField f -> get_gicfield f
      | IntidSetAct v -> intid_set_act v
      | IntidGetAct -> intid_get_act ()
      | IntidSetPend v -> intid_set_pend v
      | IntidGetPend -> intid_get_pend ()
      | IntidSetEnabled v -> intid_set_enabled v
      | IntidGetEnabled -> intid_get_enabled ()
      | IntidSetPri v -> intid_set_pri v
      | IntidGetPri -> intid_get_pri ()
      | IntidSetAff v -> intid_set_aff v
      | IntidGetAff -> intid_get_aff ()
      | IntidSetHM v -> intid_set_hm v
      | IntidIsEdge -> intid_is_edge ()
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
