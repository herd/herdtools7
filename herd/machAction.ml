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

(** Implementation of the action interface for machine models *)

module type A = sig
  include Arch_herd.S

  type lannot
  val empty_annot : lannot
  val barrier_sets : (string * (barrier -> bool)) list
  val annot_sets : (string * (lannot -> bool)) list
  val pp_annot : lannot -> string
  include Explicit.S
  val is_atomic : lannot -> bool
  val is_isync : barrier -> bool
  val pp_isync : string

  module ArchAction :
  ArchAction.S
    with type v = V.v
    and type loc = location
    and type value_set = V.ValueSet.t
    and type solution = V.solution
    and type arch_lannot = lannot
    and type arch_explicit = explicit
end

module type Config = sig
  val hexa : bool
  val variant : Variant.t -> bool
end

module Make (C:Config) (A : A) : sig
  type action =
    | Access of Dir.dirn * A.location * A.V.v * A.lannot * A.explicit * MachSize.sz * Access.t
    | Barrier of A.barrier
    | Commit of bool (* true = bcc / false = pred *) * string option
(* Atomic modify, (location,value read, value written, annotation *)
    | Amo of A.location * A.V.v * A.V.v * A.lannot * A.explicit * MachSize.sz * Access.t
(* NB: Amo used in some arch only (e.g., Arm, RISCV) *)
    | Fault of A.inst_instance_id * A.location * string option
(* Unrolling control *)
    | TooFar
(* TLB Invalidate event, operation (for print and level), address, if any.
   No adresss means complete invalidation at level *)
    | Inv of A.TLBI.op * A.location option
(* Data cache operation event *)
    | DC of AArch64Base.DC.op * A.location option
(* Arch specific actions *)
    | Arch of A.ArchAction.t

  val tag_access :
    MachSize.sz -> Dir.dirn -> A.location -> A.V.v -> action

  include Action.S with type action := action and module A = A

  val access_of_location_std : A.location -> Access.t

end = struct

  module A = A
  module V = A.V
  open Dir
  open Access

  let kvm = C.variant Variant.Kvm

  let access_of_constant cst =
    let open Constant in
    match cst with
    | Symbolic (Virtual _) -> VIR
    | Symbolic (Physical _) -> PHY
    | Symbolic (System ((PTE|PTE2),_)) -> Access.PTE
    | Symbolic (System (TLB,_)) -> Access.TLB
    | Symbolic (System (TAG,_)) -> Access.TAG
    | Label _|Tag _
    | ConcreteVector _|Concrete _|PteVal _ as v ->
        Warn.fatal "access_of_constant %s as an address"
          (V.pp_v (V.Val v)) (* assert false *)


(* precondition: v is a constant symbol *)
  let access_of_value v = match v with
  | V.Var _ -> assert false
  | V.Val cst -> access_of_constant cst

  let access_of_location_init = function
    | A.Location_reg _ -> REG
    | A.Location_global v
      -> access_of_value v

  let access_of_location_std =
    let open Constant in
    function
    | A.Location_reg _ -> REG
    | A.Location_global (V.Val (Symbolic (Virtual _))|V.Var _)
      -> VIR
    | A.Location_global (V.Val (Symbolic ((System (PTE,_))))) as loc
        ->
          if kvm then Access.PTE
          else Warn.fatal "PTE %s while -variant kvm is not active"
                 (A.pp_location loc)
    | A.Location_global v ->
        Warn.fatal
          "access_of_location_std on non-standard symbol '%s'"
          (V.pp_v v)


  type action =
    | Access of Dir.dirn * A.location * A.V.v * A.lannot * A.explicit * MachSize.sz * Access.t
    | Barrier of A.barrier
    | Commit of bool * string option
    | Amo of
        A.location * A.V.v * A.V.v * A.lannot * A.explicit *
        MachSize.sz * Access.t
    | Fault of A.inst_instance_id * A.location * string option
    | TooFar
    | Inv of A.TLBI.op * A.location option
    | DC of AArch64Base.DC.op * A.location option
    | Arch of A.ArchAction.t

  let tag_access sz d l v =
    Access (d,l,v,A.empty_annot,A.exp_annot,sz,Access.TAG)

  let mk_init_write l sz v =
    match l,v with
    | (A.Location_global lg,A.V.Val (Constant.Concrete _))
          when A.V.check_ctag lg ->
        tag_access sz W l v
    | _,A.V.Val (Constant.Tag _) ->
        tag_access sz W l v
    | _ ->
        Access(W,l,v,A.empty_annot,A.exp_annot,sz,access_of_location_init l)

  let pp_action a = match a with
  | Access (d,l,v,an,exp_an,sz,_) ->
      Printf.sprintf "%s%s%s%s%s=%s"
        (pp_dirn d)
        (A.pp_location l)
        (A.pp_annot an)
        (A.pp_explicit exp_an)
        (if sz = MachSize.Word then "" else MachSize.pp_short sz)
        (V.pp C.hexa v)
  | Barrier b -> A.pp_barrier_short b
  | Commit (_,m) ->
     Printf.sprintf "Branching%s"
       (match m with None -> "" | Some txt -> "("^txt^")")
  | Amo (loc,v1,v2,an,exp_an,sz,_) ->
      Printf.sprintf "RMW(%s)%s%s%s(%s>%s)"
        (A.pp_annot an)
        (A.pp_explicit exp_an)
        (A.pp_location loc) (MachSize.pp_short sz)
        (V.pp C.hexa v1) (V.pp C.hexa v2)
  | Fault (ii,loc,msg) ->
      Printf.sprintf "Fault(proc:%s,poi:%s,loc:%s%s)"
        (A.pp_proc ii.A.proc)
        (A.pp_prog_order_index ii.A.program_order_index)
        (A.pp_location_old loc)
        (match msg with
         | None -> ""
         | Some msg -> Printf.sprintf ",type:%s" msg)
  | TooFar -> "TooFar"
  | Inv (op,None) ->
      Printf.sprintf "TLBI(%s)" (A.TLBI.pp_op op)
  | Inv (op,Some loc) ->
      Printf.sprintf "TLBI(%s,%s)" (A.TLBI.pp_op op) (A.pp_location loc)
  | DC (op,None) ->
      Printf.sprintf "DC(%s)" (AArch64Base.DC.pp_op op)
  | DC(op,Some loc) ->
      Printf.sprintf "DC(%s,%s)" (AArch64Base.DC.pp_op op) (A.pp_location loc)
  | Arch a -> A.ArchAction.pp a

(* Utility functions to pick out components *)
  let value_of a = match a with
  | Access (_,_ , v,_,_,_,_)
    -> Some v
  | Barrier _|Commit _|Amo _|Fault _|TooFar|Inv _|DC _
    -> None
  | Arch a -> A.ArchAction.value_of a

  let read_of a = match a with
  | Access (R,_,v,_,_,_,_)
  | Amo (_,v,_,_,_,_,_)
    -> Some v
  | Arch a -> A.ArchAction.read_of a
  | Access (W, _, _, _,_,_,_)|Barrier _|Commit _|Fault _
  | TooFar|Inv _|DC _
    -> None

  and written_of a = match a with
  | Access (W,_,v,_,_,_,_)
  | Amo (_,_,v,_,_,_,_)
    -> Some v
  | Arch a -> A.ArchAction.written_of a
  | Access (R, _, _, _,_,_,_)|Barrier _|Commit _|Fault _
  | TooFar|Inv _|DC _
    -> None

  let location_of a = match a with
  | Access (_, l, _,_,_,_,_)
  | Amo (l,_,_,_,_,_,_)
  | Fault (_,l,_)
  | Inv (_,Some l)
  | DC(_,Some l)
    -> Some l
  | Arch a -> A.ArchAction.location_of a
  | Barrier _|Commit _ | TooFar| Inv (_,None) | DC (_,None) -> None

(* relative to memory *)
  let is_mem_arch_action a =
    match A.ArchAction.location_of a with
    | Some a -> A.is_global a
    | None -> false

  let is_mem_store a = match a with
  | Access (W,A.Location_global _,_,_,_,_,_)
  | Amo (A.Location_global _,_,_,_,_,_,_)
    -> true
  | Arch a ->
     is_mem_arch_action a && A.ArchAction.is_store a
  | _ -> false

  let is_mem_load a = match a with
  | Access (R,A.Location_global _,_,_,_,_,_)
  | Amo (A.Location_global _,_,_,_,_,_,_)
    -> true
  | Arch a ->
     is_mem_arch_action a && A.ArchAction.is_load a
  | _ -> false

  let is_additional_mem_load _ = false

  let is_mem a = match a with
  | Access (_,A.Location_global _,_,_,_,_,_)
  | Amo (A.Location_global _,_,_,_,_,_,_)
    -> true
  | Arch a -> is_mem_arch_action a
  | _ -> false

  let is_pt a = match a with
  | Access (_,A.Location_global (A.V.Val c),_,_,_,_,_)
  | Amo (A.Location_global (A.V.Val c),_,_,_,_,_,_)
    -> Constant.is_pt c
  | Arch a ->
     begin
       match A.ArchAction.location_of a with
       | Some (A.Location_global (A.V.Val c)) -> Constant.is_pt c
       | _ -> false
     end
  | _ -> false

  let is_additional_mem _ = false

  let is_atomic a = match a with
  | Access (_,_,_,an,_,_,_) ->
      is_mem a && A.is_atomic an
  | Arch a ->
     is_mem_arch_action a && A.is_atomic (A.ArchAction.get_lannot a)
  | _ -> false

  let is_tag = function
    | Access (_,_,_,_,_,_,Access.TAG) -> true
    | Access _|Barrier _|Commit _
    | Amo _|Fault _|TooFar|Inv _|DC _|Arch _ -> false

  let is_inv = function
    | Inv _ -> true
    | Access _|Amo _|Commit _|Barrier _|Fault _|TooFar|DC _|Arch _ -> false

  let is_dc = function
    | DC _ -> true
    | Access _|Amo _|Commit _|Barrier _ |Fault _|TooFar|Inv _|Arch _ -> false

  let is_ci = function
    | DC(op,_) as a -> is_dc a && AArch64Base.DC.ci op
    | _ -> false

  let is_c = function
    | DC(op,_) as a -> is_dc a && AArch64Base.DC.c op
    | _ -> false

  let is_i = function
    | DC(op,_) as a -> is_dc a && AArch64Base.DC.i op
    | _ -> false

  let is_at_level lvl = function
    | Inv(op,_) -> A.TLBI.is_at_level lvl op
    | _ -> false

  let is_fault = function
    | Fault _ -> true
    | Access _|Amo _|Commit _|Barrier _ | TooFar | Inv _ | DC _|Arch _ -> false

  let to_fault = function
    | Fault (i,A.Location_global x,msg) -> Some ((i.A.proc,i.A.labels),x,msg)
    | Fault _|Access _|Amo _|Commit _|Barrier _ |TooFar|Inv _|DC _|Arch _
      -> None

  let get_mem_dir a = match a with
  | Access (d,A.Location_global _,_,_,_,_,_) -> d
  | _ -> assert false

  let get_mem_size a = match a with
  | Access (_,A.Location_global _,_,_,_,sz,_) -> sz
  | Arch a -> A.ArchAction.get_size a
  | _ -> assert false

  let is_PA_access = function
    | Access (_,_,_,_,_,_,(Access.PHY|Access.PHY_PTE))
    | Amo  (_,_,_,_,_,_,(Access.PHY|Access.PHY_PTE))
        -> true
    | _ -> false

  let is_invalid_val = let open Constant in function
    | Some (A.V.Val (PteVal v)) -> V.is_zero (V.intToV v.PTEVal.valid)
    | _ -> false

  let invalid_pte act = is_invalid_val (written_of act) || is_invalid_val (read_of act)

  let is_valid_val =
    let open Constant in function
    | Some (A.V.Val (PteVal v)) -> V.is_one (V.intToV v.PTEVal.valid)
    | _ -> false

  let valid_pte act = is_valid_val (written_of act) || is_valid_val (read_of act)

  let af0_val =
    let open Constant in function
    | Some (A.V.Val (PteVal v)) -> V.is_zero (V.intToV v.PTEVal.af)
    | _ -> false

  and db0_val =
    let open Constant in function
    | Some (A.V.Val (PteVal v)) -> V.is_zero (V.intToV v.PTEVal.db)
    | _ -> false

  let val_pteaf0 act = af0_val (read_of act) || af0_val (written_of act)
  and val_ptedb0 act = db0_val (read_of act) || db0_val (written_of act)

  let get_pteval act = match written_of act,read_of act with
  | None,None -> None
  | (Some v,None)
  | (_,Some v) (* written value has priority... *)
      ->
        let open Constant in
        begin match v with
        |  A.V.Val (PteVal v) -> Some v
        | _ -> None
        end

  let is_implicit_pte_read = function
  | Access (R,_,_,_,_,_,Access.PTE) -> true
  | _ -> false

  let lift_explicit_predicate p act = match act with
    | Access(_,_,_,_,e,_,_)|Amo (_,_,_,_,e,_,_) -> p e
    | Arch a -> p (A.ArchAction.get_explicit a)
    | _ -> false

  let is_explicit = lift_explicit_predicate A.is_explicit_annot

(* relative to the registers of the given proc *)
  let is_reg_store a (p:int) = match a with
  | Access (W,A.Location_reg (q,_),_,_,_,_,_) -> p = q
  | _ -> false

  let is_reg_load a (p:int) = match a with
  | Access (R,A.Location_reg (q,_),_,_,_,_,_) -> p = q
  | _ -> false

  let is_reg a (p:int) = match a with
  | Access (_,A.Location_reg (q,_),_,_,_,_,_) -> p = q
  | _ -> false

(* Store/Load anywhere *)
  let is_store a = match a with
  | Access (W,_,_,_,_,_,_)|Amo _ -> true
  | Arch a -> A.ArchAction.is_load a
  | Access (R,_,_,_,_,_,_)|Barrier _|Commit _|Fault _|TooFar| Inv _ | DC _ -> false

  let is_load a = match a with
  | Access (R,_,_,_,_,_,_)|Amo _ -> true
  | Arch a -> A.ArchAction.is_store a
  | Access (W,_,_,_,_,_,_)|Barrier _|Commit _|Fault _|TooFar|Inv _ | DC _ -> false

  let get_kind = function
    | (Access (_,_,_,_,_,_,k)|Amo (_,_,_,_,_,_,k)) ->k
    | Arch a -> A.ArchAction.get_kind a
    | _ -> assert false

  let compatible_accesses a1 a2 =
    let k1 = get_kind a1 and k2 = get_kind a2 in
    Access.compatible k1 k2

  let is_reg_any a = match a with
  | Access (_,A.Location_reg _,_,_,_,_,_) -> true
  | _ -> false

  let is_reg_store_any a = match a with
  | Access (W,A.Location_reg _,_,_,_,_,_) -> true
  | _ -> false

  let is_reg_load_any a = match a with
  | Access (R,A.Location_reg _,_,_,_,_,_) -> true
  | _ -> false

(* Barriers *)
  let is_barrier a = match a with
  | Barrier _ -> true
  | _ -> false

  let barrier_of a = match a with
  | Barrier b -> Some b
  | _ -> None

  let same_barrier_id _ _ = assert false

(* Commits *)

  let is_commit_bcc a = match a with
  | Commit (b,_) -> b
  | _ -> false

  let is_commit_pred a = match a with
  | Commit (b,_) -> not b
  | _ -> false

  let is_pod a = match a with
  | Commit _ -> true
  | _ -> false

(* Unroll control *)
  let toofar = TooFar

  let is_toofar = function
    | TooFar -> true
    | _ -> false

(* Architecture-specific sets *)

  let arch_sets =
    let bsets =
      List.map
        (fun (tag,p) ->
          let p act = match act with
          | Barrier b -> p b
          | _ -> false
          in tag,p) A.barrier_sets
    and asets =
      List.map
        (fun (tag,p) ->
          let p act = match act with
          | Access(_,_,_,annot,_,_,_)|Amo (_,_,_,annot,_,_,_) -> p annot
          | Arch a -> p (A.ArchAction.get_lannot a)
          | _ -> false
          in tag,p) A.annot_sets
    and esets =
      List.map
        (fun (tag,p) ->
          let p = lift_explicit_predicate p in
          tag,p) A.explicit_sets

    and lsets =
      List.map
        (fun lvl -> A.pp_level lvl,is_at_level lvl)
        A.levels

    and aasets =
      List.map
        (fun (tag,p) ->
          let p act = match act with
            | Arch act -> p act
            | _ -> false in
          tag,p)
      A.ArchAction.sets

    in
    ("T",is_tag)::
    ("FAULT",is_fault)::
    ("TLBI",is_inv)::
    ("DC",is_dc)::
    ("CI",is_ci)::
    ("C",is_c)::("I",is_i)::
    (if kvm then
      fun k ->
        ("PA",is_PA_access)::
        ("PTE",is_pt)::
        ("PTEINV",invalid_pte)::
        ("PTEV",valid_pte)::
        ("PTEAF0",val_pteaf0)::
        ("PTEDB0",val_ptedb0)::
        k
    else
      fun k -> k)
      (bsets @ asets @ esets @ lsets @ aasets)

  let arch_rels =
    if kvm then
      let inv_domain_act =

        let inv_domain_sym a1 a2 =
          let open Constant in
          match a1,a2 with
          | ((Virtual {name=s1;_}|Physical (s1,_)|System (PTE,s1)),System (TLB,s2))
          | (System (TLB,s2),(Virtual {name=s1;_}|Physical (s1,_)|System (PTE,s1)))
            -> Misc.string_eq s1 s2
          | _,_ -> false in

        let inv_domain_loc loc1 loc2 =
          let open Constant in
          match loc1,loc2 with
          | A.Location_global (A.V.Val (Symbolic a1)),
            A.Location_global (A.V.Val (Symbolic a2))
            -> inv_domain_sym a1 a2
          | _,_ -> false in

        fun act1 act2 -> match act1,act2 with
        | (act,Inv (_,None))|(Inv (_, None),act)
          ->
            is_mem act &&
            begin match location_of act with
            | Some (A.Location_global _) ->  true
            | Some _|None -> false
            end
        | (e,Inv (_,Some loc1))|(Inv (_, Some loc1),e)
          ->
            is_mem e &&
            begin match location_of e with
            | Some loc2 -> inv_domain_loc loc1 loc2
            | None -> false
            end
        | _ -> false

      and alias_act =
        let get_oa =
          let open Constant in
          function
          | Some (A.V.Val (PteVal v)) -> Some v.PTEVal.oa
          | Some (A.V.Val (ConcreteVector _|Concrete _|Symbolic _|Label (_, _)|Tag _))
          | None
            -> None
          | Some (A.V.Var _) ->
              Warn.fatal "Cannot decide alias on variables"
        and is_amo = function
          | Amo _ -> true
          | _ -> false in

        fun act1 act2 ->
          (* RMW events are not compatible with this alias
          that relies on event values.
          Reason: RWM events have two values.. *)
          assert (not (is_amo act1 || is_amo act2)) ;
          is_pt act1 && is_pt act2 &&
          (match get_oa (value_of act1), get_oa (value_of act2) with
          | Some s1,Some s2 -> PTEVal.oa_eq s1 s2
          | _,_ -> false) in

      [("inv-domain",inv_domain_act); ("alias",alias_act);]
    else []

  let arch_dirty = (* To be deprecated *)
    if kvm then
      let open DirtyBit in
      let check_pred f d  =
        fun act ->
          is_pt act &&
          (match get_pteval act with
          | None -> false
          | Some pteval -> f d pteval) in

      let read_only =
        check_pred
          (fun t p ->
            let open PTEVal in
            (p.af=1 || p.af=0 && t.my_ha ()) &&
            (p.db=0 && (not (t.my_hd ()) || p.dbm=0)))

      and _read_write =
        check_pred
          (fun t p ->
            let open PTEVal in
            (p.af=1 || (p.af=0 && t.my_ha ())) &&
            (p.db=1 || (p.db=0 && p.dbm=1 && t.my_hd ()))) in
      ["ReadOnly",read_only;]
    else []

  let is_isync act = match act with
  | Barrier b -> A.is_isync b
  | _ -> false

  let pp_isync = A.pp_isync

(* Equations *)
  let undetermined_vars_in_action a =
    match a with
    | Access (_,l,v,_,_,_,_) ->
        V.ValueSet.union
          (A.undetermined_vars_in_loc l)
          (V.undetermined_vars v)
    | Amo (loc,v1,v2,_,_,_,_) ->
        V.ValueSet.union3
          (A.undetermined_vars_in_loc loc)
          (V.undetermined_vars v1)
          (V.undetermined_vars v2)
    | Arch a -> A.ArchAction.undetermined_vars a
    | Barrier _|Commit _|Fault _|TooFar|Inv _ | DC _ -> V.ValueSet.empty

  let simplify_vars_in_action soln a =
    match a with
    | Access (d,l,v,an,exp_an,sz,t) ->
        let l = A.simplify_vars_in_loc soln l in
        let v = V.simplify_var soln v in
        Access (d,l,v,an,exp_an,sz,t)
    | Amo (loc,v1,v2,an,exp_an,sz,t) ->
        let loc =  A.simplify_vars_in_loc soln loc in
        let v1 = V.simplify_var soln v1 in
        let v2 = V.simplify_var soln v2 in
        Amo (loc,v1,v2,an,exp_an,sz,t)
    | Fault (ii,loc,msg) ->
        let loc = A.simplify_vars_in_loc soln loc in
        Fault(ii,loc,msg)
    | Inv (op,oloc) ->
        let oloc = Misc.app_opt (A.simplify_vars_in_loc soln) oloc in
        Inv (op,oloc)
    | DC (op,oloc) ->
        let oloc = Misc.app_opt (A.simplify_vars_in_loc soln) oloc in
        DC (op,oloc)
    | Arch a -> Arch (A.ArchAction.simplify_vars soln a)
    | Barrier _ | Commit _|TooFar -> a

  let annot_in_list _str _ac = false

end
