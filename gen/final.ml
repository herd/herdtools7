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

    type vset
    type fenv = (C.A.location * vset) list
    type eventmap = C.A.location C.C.EventMap.t

(* Add an observation to fenv *)
    val add_final_v :
        Code.proc -> C.A.arch_reg -> IntSet.t -> fenv -> fenv
    val add_final_pte :
        Code.proc -> C.A.arch_reg -> C.A.Value.pte -> fenv -> fenv
    val add_final_loc :
        Code.proc -> C.A.arch_reg -> string -> fenv -> fenv
    val cons_int :   C.A.location -> int -> fenv -> fenv
    val cons_vec : C.A.location -> int array -> fenv -> fenv
    val cons_pteval :   C.A.location -> C.A.Value.pte -> fenv -> fenv
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

    include Fault.S with type loc_global := C.A.location and type fault_type := FaultType.No.t

    type faults = FaultSet.t

    type final

    val check : fenv -> faults * faults -> final
    val observe : fenv -> faults * faults -> final
    val run : C.C.event list list -> C.A.location C.C.EventMap.t -> faults * faults -> final

    val dump_final : out_channel -> final -> unit

(* Complement init environemt *)
    val extract_ptes : fenv -> C.A.location list

  end = functor (O:Config) -> functor (C:ArchRun.S) ->
  struct

    let do_kvm = Variant_gen.is_kvm O.variant

    (* TODO change the type? *)
    type v = I of C.C.Value.v | S of string | P of C.A.Value.pte
    let pte_def = P (C.A.Value.default_pte "*")
    let () = ignore pte_def

    let looks_like_array = function
      | S s -> String.length s > 0 && s.[0] = '{'
      | _ -> false


    module VSet =
      MySet.Make
        (struct
          type t = v

          let compare v1 v2 = match v1,v2 with
          | I i1,I i2 -> compare i1 i2
          | S s1,S s2 -> String.compare s1 s2
          | P p1,P p2 -> C.A.Value.pte_compare p1 p2
          | ((P _|S _),I _)
          | (P _,S _)
            -> -1
          | (I _,(S _|P _))
          | (S _,P _)
            -> +1
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
          | Rf _ | Fr _ | Ws _ | Coms _ | Hat
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

    let add_final_pte p r v finals = (C.A.of_reg p r,VSet.singleton (P v))::finals

    let add_final_loc p r v finals =
      let loc = C.A.of_reg p r in
      (loc,VSet.singleton (S v))::finals

    let cons_int loc i fs = (loc,VSet.singleton (I (C.C.Value.from_int i)))::fs

    let cons_vec loc t fs =
      let vec = Code.add_vector O.hexa (Array.to_list t) in
      (loc,VSet.singleton (S vec))::fs

    let cons_pteval loc p fs = (loc,VSet.singleton (P p))::fs

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
            | Code.Pte ->
                Some (P (C.C.Value.to_pte evt.C.C.v))
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

    module FaultArg = struct
      type arch_global = C.A.location
      let pp_global = C.A.pp_location
      let global_compare = C.A.location_compare

      let same_id_fault _ _ = assert false

      module FaultType = FaultType.No
      type fault_type = FaultType.t
      let pp_fault_type = FaultType.pp
      let fault_type_compare = FaultType.compare
    end

    include Fault.Make(FaultArg)

    (* Represent faults, i.e.,
       a /\ b /\ c,
       or the negation of faults, i.e.
       ~a /\ ~b /\ ~c *)
    type faults = FaultSet.t

    type cond_final =
      | Exists of fenv
      | Forall of (C.A.location * C.C.Value.v) list list
      | Locations of C.A.location list

    (* The two FaultSet.t carry
       positive and negetive checks respectively *)
    type final = cond_final * faults * faults

    module Run = Run_gen.Make(O)(C)

    let check f (pos_flts,neg_flts) = Exists f,pos_flts,neg_flts
    let observe f (pos_flts,neg_flts) = Locations (List.map fst f),pos_flts,neg_flts
    let run evts m (pos_flts,neg_flts) = Forall (Run.run evts m),pos_flts,neg_flts

(* Dumping *)
    open Printf


    let dump_val = function
      | I i ->
          let i = C.C.Value.to_int i in
          if O.hexa then sprintf "0x%x" i
          else sprintf "%i" i
      | S s -> s
      | P p -> C.A.Value.pp_pte p

    let dump_tag = function
      | I i -> C.C.Value.to_int i
      | _ -> Warn.fatal "Tags can only be of type integer"

    let dump_atom r v = match Misc.tr_atag (C.A.pp_location r) with
        | Some s -> sprintf "[tag(%s)]=%s" s (Code.add_tag "" (dump_tag v))
        | None ->
           let pp_loc =
             if looks_like_array v then C.A.pp_location
             else C.A.pp_location_brk in
           sprintf "%s=%s" (pp_loc r) (dump_val v)

    let dump_state_list fs =
      List.map ( fun (r,vs) ->
        match VSet.as_singleton vs with
        | Some v -> dump_atom r v
        | None ->
          let pp = VSet.pp_str " \\/ " (fun v -> dump_atom r v) vs in
          sprintf "(%s)" pp
      ) fs

    let dump_flts pos_flts neg_flts =
      (* print ~a /\ ~b /\ ~c *)
      ( FaultSet.map_list pp_fault neg_flts |> List.map ((^) "~") ) @
      (* print  a /\ b /\ c *)
      ( FaultSet.map_list pp_fault pos_flts )

    let dump_locations chan locs =
      fprintf chan "locations [%s]\n" (String.concat " " locs)

    let dump_final chan (f,pos_flts,neg_flts) =
      let flts = dump_flts pos_flts neg_flts in
      match f with
      | Exists fs ->
          let exist_list = (dump_state_list fs) @ flts in
          begin match exist_list with
            | [] -> Warn.fatal "There is no `exist` condition check."
            | _ ->
              let exist_string = String.concat " /\\ " exist_list in
              let if_neg = if !Config.neg then "~" else "" in
              fprintf chan "%sexists (%s)\n" if_neg exist_string
          end
      | Forall ffs ->
          fprintf chan "forall %s%s\n" (Run.dump_cond ffs)
            (match flts with
            | [] -> ""
            | pp -> " /\\ " ^ (String.concat " /\\ " pp))
      | Locations locs ->
          dump_locations chan
            (List.map
               (fun loc -> sprintf "%s;" (C.A.pp_location loc))
               locs) ;
          begin match flts with
          | [] -> ()
          | pp -> if not do_kvm then fprintf chan "forall %s\n"
                (String.concat " /\\ " pp)
          end

(* Extract ptes *)
    let extract_ptes =
      List.fold_left
        (fun k (loc,vset) ->
          if
            VSet.exists (function | P _ -> true | (I _|S _) -> false)
              vset then
            loc::k
          else k)
        []
  end
