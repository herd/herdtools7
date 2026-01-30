(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module type Config = sig
  val verbose : int
  val cond : Config.cond
  val optcond : bool
  val hexa : bool
  val variant : Variant_gen.t -> bool
end

module Make : functor (O:Config) -> functor (C:ArchRun.S) ->
  sig

(* During compilation of cycle, final state is a
   pair eventmap * fenv, where
    + fenv associates locations to final values;
    + eventmap maps one event to the register
      written by the node. This is useful only
      for simulating execution in `-cond unicond` mode *)

    type v = I of C.C.Value.v | S of string
    val dump_val : v -> string

    type vset
    type fenv = (C.A.location * vset) list
    type eventmap = C.A.location C.C.EventMap.t

(* Add an observation to fenv *)
    val add_final_v :
        Code.proc -> C.A.arch_reg -> IntSet.t -> fenv -> fenv
    val add_final_pte :
        Code.proc -> C.A.arch_reg -> C.C.Value.pte -> fenv -> fenv
    val add_final_loc :
        Code.proc -> C.A.arch_reg -> string -> fenv -> fenv
    val cons_int :   C.A.location -> int -> fenv -> fenv
    val cons_vec : C.A.location -> int array -> fenv -> fenv
    val cons_pteval :   C.A.location -> C.C.Value.pte -> fenv -> fenv
    val cons_int_set :  (C.A.location * IntSet.t) -> fenv -> fenv
    val add_int_sets : fenv -> (C.A.location * IntSet.t) list -> fenv

(* Standard function to record an association from register
   to expected value:
   Call is `add_final get_friends proc reg node (map,fenv)`,
   where:
     + get_friends returns the "friends of register",
       friends are registers whose expected value is
       identical. Those may stem from instructions that
       write into several registers.
     + proc is a thread identifier.
     + reg is a register option, when None nothing happens.
     + node is the current node.
     + (map,env) is the old final structure.
*)
    val add_final :
      (C.A.arch_reg -> C.A.arch_reg list) ->
      Code.proc -> C.A.arch_reg option -> C.C.node ->
      eventmap * fenv -> eventmap * fenv

    include Fault.S with type loc_global := v and type fault_type := FaultType.No.t

    type faults = FaultAtomSet.t

    type prop = (C.A.location, v, FaultType.No.t) ConstrGen.prop
    type final = prop ConstrGen.constr
    val pp_prop_atom : (C.A.location, v, FaultType.No.t) ConstrGen.atom -> string

    type locations = (C.A.location, v, FaultType.No.t) LocationsItem.t

    val check : is_pos:bool -> fenv -> faults * faults -> final
    val exist_true : final
    val observe : fenv -> faults * faults -> locations list
    val run : C.C.event list list -> C.A.location C.C.EventMap.t -> faults * faults -> final

(* Complement init environemt *)
    val extract_ptes : fenv -> C.A.location list

  end = functor (O:Config) -> functor (C:ArchRun.S) ->
  struct

    type v = I of C.C.Value.v | S of string

    let looks_like_array = function
      | S s -> String.length s > 0 && s.[0] = '{'
      | _ -> false


    module VSet =
      MySet.Make
        (struct
          type t = v
          let compare = compare
        end)
    type vset = VSet.t
    type fenv = (C.A.location * vset) list
    type eventmap = C.A.location C.C.EventMap.t

    (* If the effect of a node `n` should be checked in final condition *)
    let show_in_cond n =
      if O.optcond then
        let valid_edge m =
          let e = m.C.C.edge in
          let open C.E in
          match e.edge with
          | Rf _ | Fr _ | Ws _ | Hat
          | Back _|Leave _ -> true
          | Rmw rmw -> RMW.show_rmw_reg rmw
          | Po _ | Fenced _ | Dp _ ->
            Code.is_same_loc @@ loc_sd e
          |Insert _|Store|Node _ -> false
          | Id -> assert false in
        let is_ord_event m =
            let open C.E in
            match m.C.C.evt.C.C.bank with
            | Code.Ord -> true
            | _ -> false in
        let check_value m = Option.value m.C.C.evt.C.C.check_value ~default:false in
        let p = C.C.find_non_pseudo_prev n.C.C.prev in
          (* TODO: why need to check the previous node `p` ? *)
          (not (is_ord_event n) || (check_value n)) && (valid_edge p || valid_edge n)
        else true

    let intset2vset is =
      IntSet.fold (fun v k -> VSet.add (I (C.C.Value.from_int v)) k) is VSet.empty

    let add_final_v p r v finals = (C.A.of_reg p r,intset2vset v)::finals

    let add_final_pte p r pte finals = (C.A.of_reg p r,VSet.singleton (I(C.C.Value.from_pte pte)))::finals

    let add_final_loc p r v finals =
      let loc = C.A.of_reg p r in
      (loc,VSet.singleton (S v))::finals

    let cons_int loc i fs = (loc,VSet.singleton (I (C.C.Value.from_int i)))::fs

    let cons_vec loc t fs =
      let vec = Code.add_vector O.hexa (Array.to_list t) in
      (loc,VSet.singleton (S vec))::fs

    let cons_pteval loc p fs = (loc,VSet.singleton (I(C.C.Value.from_pte p)))::fs

    let cons_int_set (l,is) fs = (l,intset2vset is)::fs

    let add_int_sets fs f =
      fs@List.map (fun (l,is) -> l,intset2vset is) f

    let prev_value = fun v -> v-1

    (* Add a final change into `final` based on the node `n` *)
    let add_final get_friends p o n finals = match o with
    | Some r ->
        let m,fs = finals in
        let evt = n.C.C.evt in
        let bank = evt.C.C.bank in
        (* variable `v` holds the observable value from the event `evt`.
          Note that different event might observe different type of value,
          e.g. a plain value for plain read event,
          but a pte value for a pte read event. *)
        let v = match evt.C.C.dir with
        | Some Code.R ->
            begin match bank with
            | Code.CapaTag
            | Code.CapaSeal
            | Code.Ord
            | Code.Pair
            | Code.Instr
            | Code.Pte
              ->
                Some (I evt.C.C.v)
            | Code.VecReg _->
               let v0 =
                 match evt.C.C.vecreg with
                 | [] -> assert false
                 | v0::_ -> v0 in
                let vec = v0
                 |> List.map C.C.Value.to_int
                 |> Code.add_vector O.hexa in
                Some (S vec)
            | Code.Tag ->
                Some (S (Code.add_tag (Code.as_data evt.C.C.loc) (C.C.Value.to_int evt.C.C.v)))
            end
        | Some Code.W ->
           (* Because written value is assigned incrementally,
              the value before this write event should be `-1`,
              as function `prev_value` computes *)
           assert (evt.C.C.bank = Code.Ord || evt.C.C.bank = Code.CapaSeal) ;
           Some (I ( evt.C.C.v |> C.C.Value.to_int |> prev_value |> C.C.Value.from_int ) )
        | None -> None in
        if show_in_cond n then match v with
        | Some v ->
           let add_to_fs r v fs =
             (C.A.of_reg p r,VSet.singleton v)::fs in
           let vs =
             match bank with
             | Code.VecReg _ ->
                begin match evt.C.C.vecreg with
                | _::vs ->
                   List.map (fun v -> S
                   ( v |> List.map C.C.Value.to_int
                     |> Code.add_vector O.hexa ) ) vs
                | _ -> assert false
                end
             | _ -> [] in
           let m = C.C.EventMap.add n.C.C.evt (C.A.of_reg p r) m in
           let fs =
             try
                (* TODO what is this ?? *)
               add_to_fs r v
                 (List.fold_right2 add_to_fs (get_friends r) vs fs)
             with Invalid_argument _ ->
               Printf.eprintf "Something wrong on %s\n"
                  (C.C.str_node n) ;
               assert false in
           m,fs
        | None -> finals
        else finals
    | None -> finals

    open Printf
    let dump_val = function
      | I i -> C.C.Value.pp_v ~hexa:O.hexa i
      | S s -> s

    module FaultArg = struct
      type arch_global = v
      let pp_global = dump_val
      let global_compare = compare

      let same_id_fault _ _ = assert false

      module FaultType = FaultType.No
      type fault_type = FaultType.t
      let pp_fault_type = FaultType.pp
      let fault_type_compare = FaultType.compare
    end

    include Fault.Make(FaultArg)

    type faults = FaultAtomSet.t

    let dump_tag = function
      | I i -> C.C.Value.to_int i
      | _ -> Warn.fatal "Tags can only be of type integer"

    let pp_prop_atom atom =
      let open ConstrGen in
      match atom with
      | LV (Loc loc, value)
      | LV (Deref (loc,_), value) ->
        let pp_loc =
          if looks_like_array value then C.A.pp_location
          else C.A.pp_location_brk in
        begin match Misc.tr_atag (C.A.pp_location loc) with
        | Some s -> sprintf "[tag(%s)]=%s" s (Code.add_tag "" (dump_tag value))
        | None ->
          sprintf "%s=%s" (pp_loc loc) (dump_val value)
        end
      | LL (loc, value_loc) -> sprintf "%s=%s" (C.A.pp_location_brk loc) (C.A.pp_location_brk value_loc)
      | FF fault -> pp_fatom fault

    open ConstrGen

    type prop = (C.A.location, v, FaultType.No.t) ConstrGen.prop
    type final = prop ConstrGen.constr

    type locations = (C.A.location, v, FaultType.No.t) LocationsItem.t

    module Run = Run_gen.Make(O)(C)

    let fault_atom_to_prop is_pos flt =
      let atom = Atom (FF flt) in
      if is_pos then atom else Not (atom)

    let fault_atoms_to_prop pos_flts neg_flts =
      let pos = FaultAtomSet.elements pos_flts
      |> List.map (fault_atom_to_prop true) in
      let neg = FaultAtomSet.elements neg_flts
      |> List.map (fault_atom_to_prop false) in
      And(pos @ neg)

    let fenv_to_prop f =
      List.map ( fun (loc, value_set) ->
        VSet.elements value_set
          |> List.map ( fun value -> Atom(LV(Loc loc, value)))
          |> fun prop -> Or( prop )
      ) f
      |> fun prop -> And( prop )

    let check ~is_pos f (pos_flts,neg_flts) =
      let value_prop = fenv_to_prop f in
      let fault_prop = fault_atoms_to_prop pos_flts neg_flts in
      let prop = match value_prop, fault_prop with
      | (Atom _ as l), (Atom _ as r) -> And [l; r;]
      | (Atom _ as l), And r -> And (l :: r)
      | And l, (Atom _ as r) -> And (l @ [r])
      | And l, And r -> And (l @ r)
      | _,_ -> Warn.fatal "error in connect fault and value propositions" in
      if is_pos then ExistsState ( prop ) else NotExistsState ( prop )

    let exist_true = ExistsState (And [])

    let observe f (pos_flts,neg_flts) =
      let value_location = List.map ( fun (loc, _) -> LocationsItem.Loc ((Loc loc),TestType.TyDef)) f in
      let fault_location = FaultAtomSet.union pos_flts neg_flts
        |> FaultAtomSet.elements
        |> List.map ( fun fault -> LocationsItem.Fault fault ) in
      value_location @ fault_location

    let rec run_cond_to_constr_gen_cond = function
      | Run.Atom (loc,v) -> Atom ( LV (Loc loc, I v))
      | Run.Or (cond) -> Or (List.map run_cond_to_constr_gen_cond cond)
      | Run.And (cond) -> And (List.map run_cond_to_constr_gen_cond cond)

    let run evts m (pos_flts,neg_flts) =
      let value_prop = Run.run evts m
      |> run_cond_to_constr_gen_cond in
      let fault_prop = fault_atoms_to_prop pos_flts neg_flts in
      ForallStates (Or [fault_prop; value_prop])

(* Extract ptes *)
    let extract_ptes =
      List.filter_map
        (fun (loc,vset) ->
          if VSet.exists (function
            | I value ->
              begin match value with C.C.Value.PteValue _ -> true | _ -> false end
            | _ -> false
          ) vset
          then Some loc
          else None)
  end
