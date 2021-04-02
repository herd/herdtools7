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
    type vset
    type fenv = (C.A.location * vset) list
    type eventmap = C.A.location C.C.EventMap.t

(* Add an observation to fenv *)
    val add_final_v :
        Code.proc -> C.A.arch_reg -> IntSet.t -> fenv -> fenv
    val add_final_pte :
        Code.proc -> C.A.arch_reg -> PTEVal.t -> fenv -> fenv
    val add_final_loc :
        Code.proc -> C.A.arch_reg -> string -> fenv -> fenv
    val cons_int :   C.A.location -> int -> fenv -> fenv
    val cons_pteval :   C.A.location -> PTEVal.t -> fenv -> fenv
    val cons_int_set :  (C.A.location * IntSet.t) -> fenv -> fenv
    val add_int_sets : fenv -> (C.A.location * IntSet.t) list -> fenv
    val add_final :
        Code.proc -> C.A.arch_reg option -> C.C.node ->
          eventmap * fenv -> eventmap * fenv

    type faults = (Proc.t * StringSet.t) list
    type final

    val check : fenv -> faults -> final
    val observe : fenv -> faults -> final
    val run : C.C.event list list -> C.A.location C.C.EventMap.t -> faults -> final

    val dump_final : out_channel ->  final -> unit

(* Complement init environemt *)
    val extract_ptes : fenv -> C.A.location list

  end = functor (O:Config) -> functor (C:ArchRun.S) ->
  struct

    let do_kvm = O.variant Variant_gen.KVM

    type v = I of int | S of string | P of PTEVal.t
    let pte_def = P (PTEVal.default "*")
    let () = ignore pte_def

    module VSet =
      MySet.Make
        (struct
          type t = v

          let compare v1 v2 = match v1,v2 with
          | I i1,I i2 -> compare i1 i2
          | S s1,S s2 -> String.compare s1 s2
          | P p1,P p2 -> PTEVal.compare p1 p2
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

    let show_in_cond =
      if O.optcond then
        let valid_edge m =
          let e = m.C.C.edge in
          let open C.E in
          match e.C.E.edge with
          | Rf _ | Fr _ | Ws _ | Hat
          | Back _|Leave _|Irf _|Ifr _ -> true
          | Rmw rmw -> C.A.show_rmw_reg rmw
          | Po _ | Fenced _ | Dp _ ->
              begin match C.E.loc_sd e with
              | Code.Same -> true
              | Code.Diff -> false
              end
          |Insert _|Node _ -> false
          | Id -> assert false in
        (fun n ->
          let p = C.C.find_non_pseudo_prev n.C.C.prev in
          valid_edge p || valid_edge n)
      else
        (fun _ -> true)

    let intset2vset is =
      IntSet.fold (fun v k -> VSet.add (I v) k) is VSet.empty

    let add_final_v p r v finals = (C.A.of_reg p r,intset2vset v)::finals

    let add_final_pte p r v finals = (C.A.of_reg p r,VSet.singleton (P v))::finals

    let add_final_loc p r v finals =
      (C.A.of_reg p r,VSet.singleton (S v))::finals

    let cons_int loc i fs = (loc,VSet.singleton (I i))::fs

    let cons_pteval loc p fs = (loc,VSet.singleton (P p))::fs

    let cons_int_set (l,is) fs = (l,intset2vset is)::fs

    let add_int_sets fs f =
      fs@List.map (fun (l,is) -> l,intset2vset is) f

    let prev_value = fun v -> v-1

    let add_final p o n finals = match o with
    | Some r ->
        let m,fs = finals in
        let evt = n.C.C.evt in
        let v = match evt.C.C.dir with
        | Some Code.R ->
            begin match evt.C.C.bank with
            | Code.CapaTag
            | Code.CapaSeal
            | Code.Ord ->
                Some (I evt.C.C.v)
            | Code.VecReg ->
                Some (S (Code.add_vector evt.C.C.vecreg))
            | Code.Tag ->
                Some (S (Code.add_tag (Code.as_data evt.C.C.loc) evt.C.C.v))
            | Code.Pte ->
                Some (P evt.C.C.pte)
            end
        | Some Code.W -> assert (evt.C.C.bank = Code.Ord || evt.C.C.bank = Code.CapaSeal || evt.C.C.bank == Code.VecReg) ; Some (I (prev_value evt.C.C.v))
        | None|Some Code.J -> None in
        if show_in_cond n then match v with
        | Some v ->
            C.C.EventMap.add n.C.C.evt (C.A.of_reg p r) m,
            (C.A.of_reg p r,VSet.singleton v)::fs
        | None -> finals
        else finals
    | None -> finals

    type faults = (Proc.t * StringSet.t) list

    type cond_final =
      | Exists of fenv
      | Forall of (C.A.location * Code.v) list list
      | Locations of C.A.location list

    type final = cond_final * faults

    module Run = Run_gen.Make(O)(C)

    let check f flts = Exists f,flts
    let observe f flts = Locations (List.map fst f),flts
    let run evts m flts = Forall (Run.run evts m),flts

(* Dumping *)
    open Printf

    let dump_val = function
      | I i ->
          if O.hexa then sprintf "0x%x" i
          else sprintf "%i" i
      | S s -> s
      | P p -> PTEVal.pp p

    let dump_atom r v = sprintf "%s=%s" (C.A.pp_location r) (dump_val v)

    let dump_state fs =
      String.concat " /\\ "
        (List.map
           (fun (r,vs) ->
             match VSet.as_singleton vs with
             | Some v ->
                 dump_atom r v
             | None ->
                 let pp =
                   VSet.pp_str " \\/ "
                     (fun v -> dump_atom r v)
                     vs in
                 sprintf "(%s)" pp)
           fs)

    let dump_one_flt p x =  sprintf "fault (%s,%s)" (Proc.pp p) x

    let dump_flt sep (p,xs) = StringSet.pp_str sep (dump_one_flt p) xs

    let dump_flts =
      if do_kvm then fun _ ->   ""
      else fun flts ->
        let pp = List.map (dump_flt " \\/ ") flts in
        let pp = String.concat " \\/ " pp in
        match flts with
        | [] -> ""
        | [_,xs] when StringSet.is_singleton xs -> "~" ^ pp
        | _ -> sprintf "~(%s)" pp

    let dump_locations chan = function
      | [] -> ()
      | locs -> fprintf chan "locations [%s]\n" (String.concat " " locs)

    let dump_final chan (f,flts) =
      let loc_flts =
        if do_kvm then
          List.fold_right
            (fun (p,xs) ->
              StringSet.fold
                (fun x k -> sprintf "%s;" (dump_one_flt p x)::k)
                xs)
            flts []
        else [] in
      match f with
      | Exists fs ->
          dump_locations chan loc_flts ;
          let ppfs = dump_state fs
          and ppflts = dump_flts flts in
          let cc = match ppfs,ppflts with
          | "","" -> ""
          | "",_ -> ppflts
          | _,"" -> sprintf "(%s)" ppfs
          | _,_ -> sprintf "(%s) /\\ %s" ppfs ppflts in
          if cc <> "" then
            fprintf chan "%sexists %s\n" (if !Config.neg then "~" else "") cc
      | Forall ffs ->
          dump_locations chan loc_flts ;
          fprintf chan "forall\n" ;
          fprintf chan "%s%s\n" (Run.dump_cond ffs)
            (match dump_flts flts with
            | "" -> ""
            | pp -> " /\\ "^pp)
      | Locations locs ->
          dump_locations chan
            (List.fold_right
               (fun loc k -> sprintf "%s;" (C.A.pp_location loc)::k)
               locs loc_flts) ;
          begin match dump_flts flts with
          | "" -> ()
          | pp -> if not do_kvm then fprintf chan "forall %s\n" pp
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
