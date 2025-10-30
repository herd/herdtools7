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
  | SetOA (* store OA into PAR_EL1 *)
  | SetF (* set F to 1 in PAR_EL1 *)
  | Tagged (* get Tag attribute from PTE entry *)
  | CheckCanonical (* Check is a virtual address is canonical *)
  | MakeCanonical (* Make a virtual address canonical *)
  | Extra1 of 'op1

type 'op binop =
  | Extra of 'op
  | AddPAC of bool * PAC.key

module
   Make
     (S:Scalar.S)
     (Extra:ArchOp.WithTr with type scalar = S.t and type instr = AArch64Base.instruction) : ArchOp.S
   with type extra_op1 = Extra.op1
    and type 'a constr_op1 = 'a unop
    and type extra_op = Extra.op
    and type 'a constr_op = 'a binop
    and type scalar = S.t
    and type pteval = AArch64PteVal.t
    and type addrreg = AArch64AddrReg.t
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
      | SetOA -> "SetOA"
      | SetF -> "SetF"
      | Tagged -> "Tagged"
      | CheckCanonical -> "CheckCanonical"
      | MakeCanonical -> "MakeCanonical"
      | Extra1 op1 -> Extra.pp_op1 hexa op1 |> Printf.sprintf "Extra:%s"

    type scalar = S.t
    type pteval = AArch64PteVal.t
    type addrreg = AArch64AddrReg.t
    type instr = AArch64Base.instruction
    type cst = (scalar,pteval,addrreg,instr) Constant.t

    let pp_cst hexa v =
      let module InstrPP = AArch64Base.MakePP(struct
        let is_morello = true
      end) in
      Constant.pp (S.pp hexa) (AArch64PteVal.pp hexa) (AArch64AddrReg.pp hexa)
      (InstrPP.dump_instruction) v

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
      | Symbolic (Virtual a) ->
          Some (boolToCst (PAC.is_canonical a.pac))
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

    let gettagged = op_get_pteval (fun p -> Attrs.is_tagged p.attrs)

    let getoa v =
      let open Constant in
      match v with
      | PteVal {oa;_} -> Some (Symbolic (oa2symbol oa))
      | _ -> None

    let setoa v =
      let open Constant in
      match v with
      | Symbolic (Physical (s,0)) -> Some (AddrReg { AArch64AddrReg.oa = OutputAddress.PHY s; AArch64AddrReg.f = 0 })
      | _ -> None

    let setf v =
      let open Constant in
      match v with
      | Symbolic (Physical _) -> Some (AddrReg { AArch64AddrReg.oa = OutputAddress.PHY ""; AArch64AddrReg.f = 1 })
      | _ -> None

    let trToExtra cst =
      Constant.map
        Misc.identity Extra.toExtraPteVal Extra.toExtraAddrReg
        Misc.identity cst
    and trFromExtra cst =
      Constant.map
        Misc.identity Extra.fromExtraPteVal Extra.fromExtraAddrReg
        Misc.identity cst

    (* Add a PAC field to a virtual address, this function can only add a PAC
       field if the input pointer is canonical, otherwise it raise an error, it is
       used to model the `pac*` instruction without the variant const-pac-field *)
    let addOnePAC key pointer modifier =
      let open Constant in
      match pointer with
      | Symbolic (Virtual a) when not (PAC.is_canonical a.pac) ->
          None
      | Symbolic (Virtual ({pac; offset; _} as v)) ->
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
      | Symbolic (Virtual ({pac; offset; _} as v)) ->
        let modifier = pp_cst true modifier in
        let pac = PAC.add key modifier offset pac in
        Some (Symbolic (Virtual {v with pac}))
      | _ -> None

    let do_op = function
      | AddPAC (true, key) -> addOnePAC key
      | AddPAC (false, key) -> addPAC key
      | Extra op -> fun c1 c2 ->
        try
          match Extra.do_op op (trToExtra c1) (trToExtra c2) with
          | None -> None
          | Some cst -> Some (trFromExtra cst)
        with Exit -> None


    let do_op1 op =
      match op with
      | AF -> getaf
      | SetAF -> setaf
      | DB -> getdb
      | SetDB -> setdb
      | DBM -> getdbm
      | Valid -> getvalid
      | EL0 -> getel0
      | OA -> getoa
      | SetOA -> setoa
      | SetF -> setf
      | Tagged -> gettagged
      | CheckCanonical -> checkCanonical
      | MakeCanonical -> makeCanonical
      | Extra1 op1 ->
          fun cst ->
           try
             match Extra.do_op1 op1 (trToExtra cst) with
             | None ->  None
             | Some cst -> Some (trFromExtra cst)
           with Exit -> None


    let shift_address_right s c =
      let open Constant in
      if S.equal (S.of_int 12) c then Some (Symbolic (System (TLB,s)))
      else None

    let orop p m = AArch64PteVal.orop p @@ S.to_int64 m
    and andnot2 p m = AArch64PteVal.andnot2 p @@ S.to_int64 m
    and andop p m  =
      AArch64PteVal.andop p @@ S.to_int64 m
      |> Misc.app_opt S.of_int64

    (* Share code, placement in ASLOp not ideal *)
    let mask = ASLOp.mask

  end
