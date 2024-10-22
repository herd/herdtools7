(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
open Printf

module Make (A : Arch_herd.S) : sig

  type lock_arg  =
    | LockC11 of bool         (* true <=> success *)
    | LockLinux of Dir.dirn   (* Linux locks represented as RMW *)

  type action =
    | Access of
        Dir.dirn * A.location * A.V.v * MemOrderOrAnnot.t
          * bool (* from RWM op *) * MachSize.sz
    | Fence of MemOrderOrAnnot.t
(* LM: ??? RMW (location, read, written, mo) *)
    | RMW of A.location * A.V.v * A.V.v * MemOrder.t  * MachSize.sz
(* Specific actions for locks *)
    | Lock of A.location * lock_arg
    | Unlock of A.location * CBase.mutex_kind
    | TryLock of A.location (* Failed trylock, returns 1 *)
          (* true -> from lock, false -> from unlokk *)
    | ReadLock of A.location * bool
(* SRCU : location, nature, and optional value (for lock/unlock) *)
    | SRCU of A.location * MemOrderOrAnnot.annot * A.V.v option
(* CutOff: unroll too much *)
    | CutOff of string

  include Action.S with type action := action and module A = A

end = struct
  module A = A
  module V = A.V
  open Dir
  open MemOrderOrAnnot

  type lock_arg =
    | LockC11 of bool         (* true <=> success *)
    | LockLinux of Dir.dirn   (* Linux locks represented as RMW *)

  type action =
    | Access of
        dirn * A.location * V.v * MemOrderOrAnnot.t * bool * MachSize.sz
    | Fence of MemOrderOrAnnot.t
    | RMW of A.location * V.v * V.v * MemOrder.t * MachSize.sz
    | Lock of A.location * lock_arg
    | Unlock of A.location  * CBase.mutex_kind
    | TryLock of A.location (* Failed trylock *)
    | ReadLock of A.location * bool
    | SRCU of A.location * annot * V.v option
    | CutOff of string

  let mk_init_write l sz v = Access (W,l,v,AN [],false,sz)

  let par f x = sprintf "(%s)" (f x)
  let bra f x = sprintf "[%s]" (f x)

  let pp_action a = match a with
  | Access (d,l,v,mo,at,_) ->
      sprintf "%s%s%s%s=%s"
        (pp_dirn d) (if at then "*" else "")
        (match mo with
        | MO mo -> par MemOrder.pp_mem_order_short mo
        | AN [] -> ""
        | AN a -> bra pp_annot a)
        (A.pp_location l)
        (V.pp_v v)
  | Fence mo ->
      sprintf "F%s"
        (match mo with
        | MO mo -> par MemOrder.pp_mem_order_short mo
        | AN a ->  bra pp_annot a)
  | RMW (l,v1,v2,mo,_) ->
      sprintf "RMW(%s)%s(%s>%s)"
        (MemOrder.pp_mem_order_short mo)
        (A.pp_location l)
        (V.pp_v v1) (V.pp_v v2)
  | Lock (l,LockC11 o) ->
      sprintf "L%s%s"
        (if o then "S" else "B")
        (A.pp_location l)
  | Unlock (l,CBase.MutexC11) ->
      sprintf "U%s"
        (A.pp_location l)
  | Lock (l,LockLinux d) ->
      sprintf "Lock(%s,%s)" (A.pp_location l) (pp_dirn d)
  | Unlock (l,CBase.MutexLinux) ->
      sprintf "Unlock(%s)"
        (A.pp_location l)
  | TryLock (l) ->
      sprintf "TryLock(%s,1)" (A.pp_location l)
  | ReadLock (l,ok) ->
      sprintf "ReadLock(%s,%c)"
        (A.pp_location l)
        (if ok then '1' else '0')
  | SRCU (l,an,None) ->
      sprintf "SRCU%s(%s)"
        (bra pp_annot an)
        (A.pp_location l)
  | SRCU (l,an,Some v) ->
      sprintf "SRCU%s(%s,%s)"
        (bra pp_annot an)
        (A.pp_location l)
        (V.pp_v v)
  | CutOff msg -> "CutOff:" ^ msg

(* Utility functions to pick out components *)

  let value_of a = match a with
  | Access (_,_ ,v,_,_,_)
  | SRCU (_,_,Some v)   -> Some v
  | _ -> None

  let read_of a = match a with
  | Access (R,_ , v,_,_,_)
  | RMW (_,v,_,_,_)
    -> Some v
  | _ -> None

  let written_of a = match a with
  | Access (W,_ , v,_,_,_)
  | RMW (_,_,v,_,_)
    -> Some v
  | _ -> None

  let location_of a = match a with
  | Access (_, loc, _,_,_,_)
  | Lock (loc,_)
  | Unlock (loc,_)
  | TryLock (loc)
  | ReadLock (loc,_)
  | RMW (loc,_,_,_,_)
  | SRCU (loc,_,_)
    -> Some loc
  | Fence _|CutOff _ -> None

(* relative to memory *)
  let is_mem_store a = match a with
  | Access (W,A.Location_global _,_,_,_,_)
  | RMW (A.Location_global _,_,_,_,_)
    -> true
  | _ -> false

  let is_mem_load a = match a with
  | Access (R,A.Location_global _,_,_,_,_)
  | RMW (A.Location_global _,_,_,_,_)
    -> true
  | _ -> false

  let is_additional_mem_load a = match a with
  | TryLock _|ReadLock _ -> true
  | _ -> false

  let is_mem a = match a with
  | Access (_,A.Location_global _,_,_,_,_)
  | RMW (A.Location_global _,_,_,_,_) -> true
  | _ -> false

  let is_ifetch _ = false

  let is_tag _ = false

  let is_additional_mem a = match a with
  | Lock _|Unlock _|TryLock _|ReadLock _ -> true
  | _ -> false

  (* Unimplemented *)
  let is_pte_access _ = assert false

  (* All accesses are explicit *)
  include Explicit.NoAction

  (* The following definition of is_atomic is quite arbitrary. *)

  let old_is_atomic a = match a with
  | Access (_,A.Location_global _,_,AN _,_,_) -> false
  | Access (_,A.Location_global _,_,MO _,_,_) -> true
  | RMW _ -> true
  | _ -> false

  (* LM: This one is for R and W issued by RWM *)
  let is_atomic = function
    | Access (_,A.Location_global _,_,_,at,_) -> at
    | _ -> false

  let is_fault _ = false

  let to_fault _ = None

  let get_mem_dir a = match a with
  | Access (d,A.Location_global _,_,_,_,_) -> d
  | _ -> assert false

  let get_mem_size a = match a with
  | Access (_,A.Location_global _,_,_,_,sz) -> sz
  | _ -> assert false

(* relative to the registers of the given proc *)
  let is_reg_store a (p:int) = match a with
  | Access (W,A.Location_reg (q,_),_,_,_,_) -> p = q
  | _ -> false

  let is_reg_load a (p:int) = match a with
  | Access (R,A.Location_reg (q,_),_,_,_,_) -> p = q
  | _ -> false

  let is_reg a (p:int) = match a with
  | Access (_,A.Location_reg (q,_),_,_,_,_) -> p = q
  | _ -> false

(* Store/Load anywhere *)
  let is_store a = match a with
  | Access (W,_,_,_,_,_)
  | RMW _
    -> true
  | _ -> false

  let is_load a = match a with
  | Access (R,_,_,_,_,_)
  | RMW _ -> true
  | _ -> false

  let is_reg_any a = match a with
  | Access (_,A.Location_reg _,_,_,_,_) -> true
  | _ -> false

  let is_reg_store_any a = match a with
  | Access (W,A.Location_reg _,_,_,_,_) -> true
  | _ -> false

  let is_reg_load_any a = match a with
  | Access (R,A.Location_reg _,_,_,_,_) -> true
  | _ -> false

  let compatible_accesses a1 a2 =
    (is_mem a1 && is_mem a2) || (is_reg_any a1 && is_reg_any a2)

(* Barriers *)
  let is_barrier = function
    | Fence _ -> true
    | _ -> false

  let barrier_of _ = assert false

  let same_barrier_id _ _ = assert false

(* (No) commits *)
  let is_bcc _ = false
  let is_pred ?cond:_ _ = false
  let is_commit _ = false

(* Unrolling control *)
  let cutoff msg = CutOff msg
  let is_cutoff = function
    | CutOff _ -> true
    | _ -> false
  let as_cutoff = function
    | CutOff msg -> Some msg
    | _ -> None

(* RMWs *)
  let is_rmw a = match a with
  | RMW _ -> true
  | _ -> false

(* Mutex operations *)
  let is_lock a = match a with
  | Lock _ -> true
  | _ -> false

  let is_lock_read a = match a with
  | Lock (_,LockLinux R) -> true
  | _ -> false

  let is_lock_write  a = match a with
  | Lock (_,LockLinux W) -> true
  | _ -> false

  let is_successful_lock a = match a with
  | Lock (_,LockC11 true) -> true
  | _ -> false

  let is_failed_lock a = match a with
  | Lock (_,LockC11 false)
  | TryLock (_) -> true
  | _ -> false

  let is_read_locked a = match a with
  | ReadLock (_,b) -> b
  | _ -> false

  let is_read_unlocked a = match a with
  | ReadLock (_,b) -> not b
  | _ -> false

  let is_unlock a = match a with
  | Unlock _ -> true
  | _ -> false

  let mo_matches target a = match a with
  | Access(_,_,_,MO mo,_,_)
  | RMW (_,_,_,mo,_)
  | Fence (MO mo) -> mo=target
  | _ -> false

(* Architecture-specific sets *)

  let arch_sets = [
    "RMW",(fun e -> is_rmw e || is_atomic e);
    "LK", is_lock; "LKR", is_lock_read; "LKW",is_lock_write;
    "LS", is_successful_lock;"LF", is_failed_lock;
    "UL", is_unlock;
    "RL",is_read_locked; "RU",is_read_unlocked;
    "ACQ", mo_matches MemOrder.Acq;
    "SC", mo_matches MemOrder.SC;
    "REL", mo_matches MemOrder.Rel;
    "ACQ_REL", mo_matches MemOrder.Acq_Rel;
    "RLX", mo_matches MemOrder.Rlx;
    "CON", mo_matches MemOrder.Con;
 (* For C11 RCU, Linux RCU implemented with bell file !! *)
    "A",old_is_atomic;
    "NA",(fun a -> not (old_is_atomic a));
    "annot", (fun a -> match a with
    | Access (_,_,_,AN a,_,_) | Fence (AN a) when a != [] -> true
    | _ -> false)
  ]

  let arch_rels = []
  and arch_dirty = []

  let is_isync _ = raise Misc.NoIsync
  let pp_isync = "???"

(* Equations *)

  let undetermined_vars_in_action a =
    match a with
    | Access (_,l,v,_,_,_)
    | SRCU (l,_,Some v) ->
        V.ValueSet.union
          (A.undetermined_vars_in_loc l)
          (V.undetermined_vars v)
    | RMW(l,v1,v2,_,_) ->
        V.ValueSet.union3
          (A.undetermined_vars_in_loc l)
          (V.undetermined_vars v1)
          (V.undetermined_vars v2)
    | TryLock (l)
    | Lock(l,_)
    | Unlock (l,_)
    | ReadLock (l,_)
    | SRCU(l,_,None) ->
        A.undetermined_vars_in_loc l
    | Fence _|CutOff _ -> V.ValueSet.empty

  let simplify_vars_in_action soln a =
    match a with
    | Access (d,l,v,mo,at,sz) ->
        let l' = A.simplify_vars_in_loc soln l in
        let v' = V.simplify_var soln v in
        Access (d,l',v',mo,at,sz)
    | RMW(l,v1,v2,mo,sz) ->
        let l' = A.simplify_vars_in_loc soln l in
        let v1' = V.simplify_var soln v1 in
        let v2' = V.simplify_var soln v2 in
        RMW(l',v1',v2',mo,sz)
    | Lock(l,a) ->
        let l' = A.simplify_vars_in_loc soln l in
        Lock(l',a)
    | Unlock (l,k)  ->
        let l' = A.simplify_vars_in_loc soln l in
        Unlock (l',k)
    | TryLock (l) ->
        let l' = A.simplify_vars_in_loc soln l in
        TryLock (l')
    | ReadLock (l,b) ->
        let l' = A.simplify_vars_in_loc soln l in
        ReadLock (l',b)
    | SRCU(l,a,vo) ->
        let l' =  A.simplify_vars_in_loc soln l in
        SRCU(l',a,Misc.app_opt (V.simplify_var soln) vo)
    | Fence _|CutOff _ -> a

(*************************************************************)
(* Add together event structures from different instructions *)
(*************************************************************)

  let annot_in_list str ac = match ac with
  | Access (_,_,_,AN a,_,_)
  | Fence (AN a)
  | SRCU(_,a,_)
    -> List.exists (fun a -> Misc.string_eq str a) a
  | Access (_, _, _, MO _,_,_)|Fence (MO _)|RMW (_, _, _, _,_)
  | Lock _|Unlock _|TryLock _|ReadLock _|CutOff _ -> false
end
