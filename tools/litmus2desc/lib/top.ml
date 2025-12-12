(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Herd_core
module TestResult = Top_herd.TestResult

type error = string

exception Unsupported_model of string

let unsupported_model msg = raise (Unsupported_model msg)

module Located = struct
  type 'a t = { payload : 'a; ins : LitmusTest.instr }

  let make payload ins = { payload; ins }
  let get x = x.payload
  let equal_proc_poi x y = LitmusTest.equal_proc_poi x.ins y.ins
  let compare_by_proc_poi x y = LitmusTest.compare_proc_poi x.ins y.ins

  let group_by_proc_poi (l : 'a t list) : 'a list t list =
    l
    |> List.sort compare_by_proc_poi
    |> Util.List.group_by ~eq:equal_proc_poi
    |> List.map (fun x ->
        let x, xs = Util.NonEmpty.uncons x in
        let ins = x.ins in
        make (List.map get (x :: xs)) ins)
end

type dependency_kind = Data | Control | Address

(* A dependency is from a source instruction {!source_ins} and a target
      instruction {!target_ins}, and is established through intermediate
      instructions in {!path}. *)
type dependency = {
  kind : dependency_kind;
  path : LitmusTest.instr list;
  source_ins : LitmusTest.instr;
  target_ins : LitmusTest.instr;
  is_ordered : bool;
}

module Make (S : SemExtra.S) = struct
  module A = S.A
  module V = A.V
  module E = S.E
  module EU = EventUtils.Make (E)

  module Exec = struct
    type t = (S.concrete, S.set_pp, S.rel_pp) TestResult.execution

    let events t = (TestResult.concrete t).S.str.E.events
    let iico_data t = (TestResult.concrete t).S.str.E.intra_causality_data
    let po t = (TestResult.concrete t).S.po

    let relation name t =
      match List.assoc_opt name (TestResult.relations t) with
      | Some rel -> rel
      | None ->
          unsupported_model
            (Printf.sprintf "cat file does not define relation %S" name)

    (* Given a location in the final condition, find the store event
       responsible for writing the observed value *)
    let final_source_store (t : t) (location : S.location) =
      let location_equal l1 l2 = S.A.location_compare l1 l2 = 0 in
      let rfmap = (TestResult.concrete t).S.rfmap in
      rfmap |> S.RFMap.bindings
      |> Misc.List.find_map (function
        | S.Final l, store when location_equal l location -> Some store
        | _ -> None)
  end

  type exec = Exec.t

  let pp_value fmt (v : V.v) = Format.pp_print_string fmt (V.pp_v v)
  let pp_global_loc fmt (loc : S.global_loc) = pp_value fmt loc
  let global_loc_equal (l1 : S.global_loc) (l2 : S.global_loc) = V.equal l1 l2

  module Descriptum = struct
    (** This module defines the aspects of a litmus test (and its candidate
        executions) for which litmus2desc supports generating a prose
        description. *)

    (* Final condition's register value constraint, i.e. equations of the form
      `0:X1=3`. *)
    type reg_constr =
      | Reg_load_init of { proc : int; reg_pretty : string }
      | Reg_load_ev of {
          proc : int;
          reg_pretty : string;
          value : V.v;
          load_ev : E.event;
          source_store : E.event;
          ca_target : E.event option;
        }

    (* Final condition's memory location value constraint, i.e. equations of the
      form `[x]=1`. *)
    type mem_constr = {
      loc_pretty : string;
      value : V.v;
      store_ev : E.event;
      ca_source : E.event option;
    }

    type constr = Reg_constr of reg_constr | Mem_constr of mem_constr
  end

  type code = LitmusTest.instr list

  let find_instr_by_proc_poi ~proc ~poi : code -> LitmusTest.instr =
    List.find (fun (ins : LitmusTest.instr) ->
        Int.equal ins.proc proc && Int.equal ins.static_poi poi)

  let instr_of_event (code : code) ev =
    match (E.proc_of ev, E.static_poi ev) with
    | Some proc, Some poi -> find_instr_by_proc_poi ~proc ~poi code
    | _ -> invalid_arg "event with no instruction"

  (**********************************************************)
  (* Rendering discovered execution facts to english prose  *)
  (**********************************************************)

  type render_ctx = {
    code : LitmusTest.instr list;
    proc_count : int;
    exec : Exec.t;
    latex_compat : bool;
  }

  let render_instruction ctx ins =
    LitmusTest.render_instruction ~latex:ctx.latex_compat ins

  let opcode_name (instr : LitmusTest.instr) =
    match instr.instruction with
    | I_FENCE b -> AArch64Base.pp_barrier b
    | _ ->
        let str = LitmusTest.render_instruction ~latex:false instr in
        let splitted = String.split_on_char ' ' str in
        List.hd splitted

  let english_opcode_name instr =
    match opcode_name instr with
    | "LDR" -> Some "Load"
    | "STR" -> Some "Store"
    | "LDAR" -> Some "Load Acquire"
    | "STLR" -> Some "Store Release"
    | "EOR" -> Some "Exclusive Or"
    | "ADD" -> Some "Add"
    | "CBNZ" -> Some "Branch"
    | _ -> None

  let describe_instruction ctx (ins : LitmusTest.instr) : string =
    let proc = ins.proc in
    let ins_name = opcode_name ins in
    let ins_render_name =
      match english_opcode_name ins with Some name -> name | None -> ins_name
    in
    match ins.label with
    | Some label ->
        Format.sprintf "the %s instruction at label %s on P%d" ins_render_name
          (Label.pp label) proc
    | None ->
        let spoi = ins.static_poi in
        let similar_instructions =
          ctx.code
          |> List.filter (fun (other_ins : LitmusTest.instr) ->
              Int.equal other_ins.proc proc
              && (not (Int.equal spoi other_ins.static_poi))
              && String.equal (opcode_name other_ins) ins_name)
        in
        if List.length similar_instructions = 0 then
          Printf.sprintf "the %s instruction on P%d" ins_render_name proc
        else
          let ix =
            similar_instructions
            |> List.filter (fun (ins : LitmusTest.instr) ->
                ins.static_poi < spoi)
            |> List.length
          in
          let poi_description = Util.verbalize_index ix in
          Printf.sprintf "the %s %s instruction in program order on P%d"
            poi_description ins_render_name proc

  let describe_mem_access ~loc ~dir ~(value : V.v) =
    let eff_ty = match dir with Dir.R -> "Read" | W -> "Write" in
    let value_descr =
      if dir = W then Format.asprintf " with value %a" pp_value value else ""
    in
    Format.asprintf "an Explicit Memory %s Effect of Location %a%s" eff_ty
      pp_global_loc loc value_descr

  let describe_barrier_event (ins : LitmusTest.instr) =
    let eff =
      match ins.instruction with
      | I_FENCE b -> (
          let open AArch64Base in
          let pp_ty = function LD -> "LD" | ST -> "ST" | FULL -> "FULL" in
          match b with
          | DMB (_, ty) -> Printf.sprintf "DMB.%s" (pp_ty ty)
          | DSB (_, ty) -> Printf.sprintf "DSB.%s" (pp_ty ty)
          | ISB -> "ISB"
          | GCSB -> "GCSB")
      | _ ->
          invalid_arg
            (Printf.sprintf "Unsupported barrier event for instruction `%s`"
               (LitmusTest.render_instruction ~latex:false ins))
    in
    Format.sprintf "a %s Effect" eff

  (* Generate prose description of a dependency (addr, data, ctrl). *)
  let describe_dep ctx (dep : dependency) : string =
    let open Descriptum in
    let path = dep.path in
    let source_desc = describe_instruction ctx dep.source_ins in
    let target_desc = describe_instruction ctx dep.target_ins in
    let dependency_kind_article, dependency_kind_name =
      match dep.kind with
      | Data -> ("a", "data")
      | Control -> ("a", "control")
      | Address -> ("an", "address")
    in
    let source_ins_desc = render_instruction ctx dep.source_ins in
    let target_ins_desc = render_instruction ctx dep.target_ins in
    if Misc.List.is_empty dep.path then
      let match_phrase =
        if dep.is_ordered then dependency_kind_article else "no"
      in
      Printf.sprintf
        "There is %s %s dependency from %s, i.e. %s, to %s, i.e. %s"
        match_phrase dependency_kind_name source_desc source_ins_desc
        target_desc target_ins_desc
    else
      let describe_path_ev ins =
        let descr = describe_instruction ctx ins in
        let ins_full = render_instruction ctx ins in
        Printf.sprintf "%s, i.e. %s," descr ins_full
      in
      let path_descrs =
        path |> List.map describe_path_ev |> String.concat " and "
      in
      let plural = List.length path > 1 in
      if dep.is_ordered then
        let name_phrase =
          Printf.sprintf "%s %s dependency" dependency_kind_article
            (String.capitalize_ascii dependency_kind_name)
        in
        let match_phrase = if plural then "create" else "creates" in
        Printf.sprintf "%s %s %s from %s, i.e. %s, to %s, i.e. %s" path_descrs
          match_phrase name_phrase source_desc source_ins_desc target_desc
          target_ins_desc
      else
        let match_phrase =
          if plural then "do not create" else "does not create"
        in
        Printf.sprintf "%s %s any dependency to %s, i.e. %s" path_descrs
          match_phrase target_desc target_ins_desc

  (* Describe all selected events of the same instruction. *)
  let describe_located_events ctx (ins : LitmusTest.instr) evs =
    let instr_desc =
      match ins.instruction with
      | I_FENCE _ -> describe_instruction ctx ins
      | _ ->
          Printf.sprintf "%s, i.e. %s,"
            (describe_instruction ctx ins)
            (render_instruction ctx ins)
    in
    let descr =
      evs
      |> List.map (fun ev ->
          match EU.action_view ev with
          | Some (Access { loc = Location_global loc; dir; value; is_implicit })
            when not is_implicit ->
              describe_mem_access ~loc ~dir ~value
          | Some Barrier -> describe_barrier_event ins
          | _ -> failwith "unsupported event")
      |> Util.List.intersperse ", and "
      |> String.concat ""
    in
    Printf.sprintf "%s generates %s" instr_desc descr

  let describe_ev ctx (ev : E.event) : string =
    let should_print_proc = ctx.proc_count > 1 in
    match (ev.E.iiid, EU.action_view ev) with
    | IdInit, Some (Access { loc = Location_global loc; _ }) ->
        Format.asprintf "the initialization event of location %a" pp_global_loc
          loc
    | IdSome iiid, Some (Access ({ loc = Location_global loc; _ } as access))
      when not access.is_implicit ->
        let eff_dir = if access.dir = R then "Read" else "Write" in
        let similar_accesses =
          Exec.events ctx.exec
          |> E.EventSet.filter (fun ev ->
              match (ev.E.iiid, EU.as_access ev) with
              | IdSome { proc; _ }, Some { dir; loc = Location_global _; _ } ->
                  access.dir = dir && iiid.S.A.proc = proc
              | _ -> false)
        in
        let should_dump_instr = E.EventSet.cardinal similar_accesses > 1 in
        let proc_details =
          if should_print_proc then Printf.sprintf " on P%d" iiid.S.A.proc
          else ""
        in
        if should_dump_instr then
          let instr_descr =
            let ins =
              find_instr_by_proc_poi ctx.code ~proc:iiid.S.A.proc
                ~poi:iiid.S.A.static_poi
            in
            Printf.sprintf " generated by %s" (render_instruction ctx ins)
          in
          let loc_descr = Format.asprintf " of Location %a" pp_global_loc loc in
          Printf.sprintf "the Explicit Memory %s Effect%s%s%s" eff_dir loc_descr
            proc_details instr_descr
        else
          Printf.sprintf "the Explicit Memory %s Effect%s" eff_dir proc_details
    | IdSome iiid, Some (Access { loc = Location_reg _; dir; _ }) ->
        let ins =
          find_instr_by_proc_poi ctx.code ~proc:iiid.S.A.proc
            ~poi:iiid.S.A.static_poi
        in
        let proc_details =
          if should_print_proc then Printf.sprintf " on P%d" iiid.S.A.proc
          else ""
        in
        let eff_ty = if dir = R then "Read" else "Write" in
        Printf.sprintf
          "the Register %s Effect originating from the %s instruction%s" eff_ty
          (opcode_name ins) proc_details
    | IdInit, Some (Access { loc = Location_reg _; _ }) ->
        invalid_arg "initialization event cannot access a register"
    | _ -> failwith "event type not supported yet"

  let describe_mem_constr ctx (constr : Descriptum.mem_constr) : string =
    let extra_text =
      let ev = constr.store_ev in
      let extra_text =
        match constr.ca_source with
        | None -> (
            match ev.E.iiid with
            | IdInit ->
                Printf.sprintf "%s retains its initial value" constr.loc_pretty
            | IdSome iiid ->
                let ins =
                  find_instr_by_proc_poi ctx.code ~proc:iiid.S.A.proc
                    ~poi:iiid.S.A.static_poi
                in
                Format.sprintf "%s has updated the memory"
                  (describe_instruction ctx ins)
            | IdSpurious -> invalid_arg "spurious event in memory constraint")
        | Some ca_source ->
            let ca_source_descr = describe_ev ctx ca_source in
            Printf.sprintf "%s is Coherence-before %s" ca_source_descr
              (describe_ev ctx ev)
      in
      Printf.sprintf ", because %s" extra_text
    in
    Format.asprintf "The value of %s is %a in the end%s" constr.loc_pretty
      pp_value constr.value extra_text

  let describe_reg_constr ctx (constr : Descriptum.reg_constr) : string =
    match constr with
    | Reg_load_init { proc; reg_pretty } ->
        Printf.sprintf
          "The Register %s on P%d holds its initialization value in the end"
          reg_pretty proc
    | Reg_load_ev { proc; reg_pretty; load_ev; source_store; value; ca_target }
      ->
        let load_ev_descr = describe_ev ctx load_ev in
        let read_explanation =
          match EU.location_of_exn source_store with
          | Location_global _ ->
              let source_value_descr =
                if EU.is_initial source_store then "the initial state"
                else describe_ev ctx source_store
              in
              Printf.sprintf "%s Reads-from %s" load_ev_descr source_value_descr
          | Location_reg _ ->
              let source_store_descr = describe_ev ctx source_store in
              Format.asprintf "%s has read the value %a from %s" load_ev_descr
                pp_value value source_store_descr
        in
        let ca_explanation =
          match (source_store, ca_target) with
          | source_store, Some ca_target when EU.is_initial source_store ->
              let ca_target_descr = describe_ev ctx ca_target in
              Printf.sprintf ", and therefore is Coherence-before %s"
                ca_target_descr
          | _ -> ""
        in
        Format.asprintf
          "The Register %s on P%d holds the value %a in the end, because %s%s"
          reg_pretty proc pp_value value read_explanation ca_explanation

  let describe_constr ctx : Descriptum.constr -> string = function
    | Mem_constr mem_constr -> describe_mem_constr ctx mem_constr
    | Reg_constr reg_constr -> describe_reg_constr ctx reg_constr

  (**********************************************************)
  (*     Detecting constraints of the final condition       *)
  (**********************************************************)

  (* Infers the event that indirectly caused register [reg] to carry its final
     value in the execution [exec]. This is done by
     - finding the Wreg event that is immediately responsible for setting
       [reg]'s final value, and
     - following rf/rf-reg and iico_data edges until reaching the next write
       event up the chain

     This is either:
         Wreg  -- iico_data^-1 ->   R    -- rf^-1     ->  W
     or
         Wreg  -- iico_data^-1 ->  Rreg  -- rf-reg^-1 ->  Wreg
     *)
  let constr_of_reg (exec : exec) proc (reg : A.reg_arg) :
      Descriptum.constr option =
    let ( let* ) = Option.bind in
    let rf = Exec.relation "rf" exec in
    let rf_reg = Exec.relation "rf-reg" exec in
    let ca = Exec.relation "ca" exec in
    let ca = E.EventRel.remove_transitive_edges ca in
    let reg_pretty = A.pp_reg reg in
    let* source_store =
      Exec.final_source_store exec (Location_reg (proc, reg))
    in
    match source_store with
    | Init -> Some Descriptum.(Reg_constr (Reg_load_init { proc; reg_pretty }))
    | Store store_ev ->
        let value = EU.value_of_exn store_ev in
        let* load_ev =
          E.EventRel.inverse (Exec.iico_data exec)
          |> EU.rel_succs store_ev
          |> E.EventSet.find_opt E.is_load
        in
        let* source_store, ca_target =
          match EU.location_of_exn load_ev with
          | Location_global _ ->
              let source_store =
                E.EventRel.inverse rf |> EU.rel_find_unique_succ load_ev
              in
              let ca_target =
                EU.rel_succs load_ev ca
                |> E.EventSet.find_first_opt (fun _ -> true)
              in
              Some (source_store, ca_target)
          | Location_reg _ ->
              let source_store =
                E.EventRel.inverse rf_reg |> EU.rel_find_unique_succ load_ev
              in
              Some (source_store, None)
        in
        let reg_constr =
          Descriptum.Reg_load_ev
            { proc; reg_pretty; load_ev; source_store; ca_target; value }
        in
        Some (Descriptum.Reg_constr reg_constr)

  (* Infers the event that caused memory location [mem_loc] to carry its final
     value in the execution [exec]. This is done by querying the execution's
     {!rfmap} to find a matching write event [w].

     It also collects non-init writes that are Coherence-before [w],
     so the prose can explain why this write is the last visible one.
   *)
  let constr_of_global_loc (exec : exec) (mem_loc : S.global_loc) :
      Descriptum.constr option =
    let ( let* ) = Option.bind in
    let* source_store =
      Exec.final_source_store exec (S.A.Location_global mem_loc)
    in
    let* store_ev, value =
      match source_store with
      | Init ->
          let* ev =
            Exec.events exec
            |> E.EventSet.find_first_opt (fun ev ->
                match (ev.E.iiid, E.location_of ev) with
                | IdInit, Some (Location_global l) -> global_loc_equal l mem_loc
                | _ -> false)
          in
          let* value = E.value_of ev in
          Some (ev, value)
      | Store ev ->
          let* value = E.value_of ev in
          Some (ev, value)
    in
    let co = Exec.relation "co" exec in
    let co = E.EventRel.remove_transitive_edges co in
    let ca_source =
      E.EventRel.inverse co |> EU.rel_succs store_ev
      |> E.EventSet.find_first_opt (fun ca_source_ev ->
          match ca_source_ev.E.iiid with IdSome _ -> true | _ -> false)
    in
    let loc_pretty = V.pp_v mem_loc in
    Some Descriptum.(Mem_constr { loc_pretty; value; store_ev; ca_source })

  let constrs_of_test (test : S.test) (exec : exec) : Descriptum.constr list =
    ConstrGen.fold_constr
      (fun a l ->
        match a with
        | LV (Loc (S.A.Location_reg (proc, reg)), _) ->
            constr_of_reg exec proc reg :: l
        | LV (Loc (Location_global loc), _) ->
            constr_of_global_loc exec loc :: l
        | _ -> l)
      test.Test_herd.cond []
    |> Util.Option.keep_some

  (**********************************************************)
  (*              Detecting dependencies                    *)
  (**********************************************************)

  (* The purpose of the functions below is to infer the intermediate
     instructions that are involved in a dependency
     of kind [kind] between [source] and [target] endpoints, using
     architecture-specific and cat model-specific heuristics.

     In particular, this is done by following [dtrm] edges in the execution
     graph from [source] to [target], and collecting EOR and ADD instructions
     (for data/addr deps), or branches (for ctrl deps) along the path.

     We also rely on [dob] edges in the execution graph to determine whether
     a control dependency edge actually orders [source] and [target], so the
     prose can say whether the path creates the dependency. *)

  let find_dtrm_successors code exec source ~up_to_spoi predicate =
    let dtrm = Exec.relation "dtrm" exec in
    let proc = (instr_of_event code source).proc in
    E.EventRel.succs dtrm source
    |> E.EventSet.elements
    |> List.filter_map (fun ev ->
        match E.static_poi ev with
        | Some poi when poi < up_to_spoi ->
            let ins = find_instr_by_proc_poi code ~proc ~poi in
            if predicate ev ins then Some ins else None
        | _ -> None)
    |> Util.List.uniq ~eq:LitmusTest.equal_proc_poi

  let find_data_dep (code : code) (exec : exec) ~describe_dep_path ~source
      ~target : dependency Located.t =
    let source_ins = source |> instr_of_event code in
    let target_ins = target |> instr_of_event code in
    let path =
      if describe_dep_path then
        find_dtrm_successors code exec source ~up_to_spoi:target_ins.static_poi
          (fun ev ins ->
            E.is_reg ev ins.proc
            &&
            match ins.instruction with
            | AArch64Base.I_OP3 (_, (ADD | EOR), _, _, _) -> true
            | _ -> false)
      else []
    in
    let ins = match path with [] -> source_ins | ins :: _ -> ins in
    let dep =
      { kind = Data; path; source_ins; target_ins; is_ordered = true }
    in
    Located.make dep ins

  let find_data_deps ~describe_dep_path code exec (r : E.event_rel) =
    r |> EU.rel_to_list
    |> List.map (fun (source, target) ->
        find_data_dep code exec ~describe_dep_path ~source ~target)

  let find_addr_dep (code : code) (exec : exec) ~describe_dep_path ~source
      ~target : dependency Located.t =
    let source_ins = source |> instr_of_event code in
    let target_ins = target |> instr_of_event code in
    let path =
      if describe_dep_path then
        find_dtrm_successors code exec source ~up_to_spoi:target_ins.static_poi
          (fun ev ins ->
            E.is_reg ev ins.proc
            &&
            match ins.instruction with
            | AArch64Base.I_OP3 (_, EOR, _, _, _) -> true
            | _ -> false)
      else []
    in
    let ins = match path with [] -> source_ins | ins :: _ -> ins in
    let dep =
      { kind = Address; path; source_ins; target_ins; is_ordered = true }
    in
    Located.make dep ins

  let find_addr_deps ~describe_dep_path code exec (r : E.event_rel) =
    r |> EU.rel_to_list
    |> List.map (fun (source, target) ->
        find_addr_dep code exec ~describe_dep_path ~source ~target)

  let find_ctrl_dep (code : code) (exec : exec) ~describe_dep_path ~source
      ~target : dependency Located.t =
    let source_ins = source |> instr_of_event code in
    let target_ins = target |> instr_of_event code in
    let path =
      if describe_dep_path then
        find_dtrm_successors code exec source ~up_to_spoi:target_ins.static_poi
          (fun ev _ -> E.is_commit ev)
      else []
    in
    let ins = match path with [] -> source_ins | ins :: _ -> ins in
    let dob = Exec.relation "dob" exec in
    let is_ordered =
      E.EventRel.succs dob source
      |> E.EventSet.exists (fun ev -> E.event_equal ev target)
    in
    let dep = { kind = Control; path; source_ins; target_ins; is_ordered } in
    Located.make dep ins

  let find_ctrl_deps ~describe_dep_path code exec (r : E.event_rel) =
    r |> EU.rel_to_list
    |> List.map (fun (source, target) ->
        find_ctrl_dep code exec ~describe_dep_path ~source ~target)

  (**********************************************************)
  (*               Explaining full executions               *)
  (**********************************************************)

  let describe_execution ~latex_compat ~describe_dep_path ~proc_count
      (code : code) (test : S.test) (exec : exec) : string list =
    let find_instr_by_iiid iiid =
      find_instr_by_proc_poi ~proc:iiid.S.A.proc ~poi:iiid.S.A.static_poi code
    in
    let events_to_describe =
      Exec.events exec |> E.EventSet.elements
      |> List.filter_map (fun ev ->
          match (ev.E.iiid, EU.action_view ev) with
          | ( IdSome iiid,
              Some (Access { loc = Location_global _; is_implicit; _ }) )
            when not is_implicit ->
              Some (Located.make ev (find_instr_by_iiid iiid))
          | IdSome iiid, Some Barrier ->
              Some (Located.make ev (find_instr_by_iiid iiid))
          | _ -> None)
      |> Located.group_by_proc_poi
    in
    let po = Exec.po exec in
    let data = Exec.relation "data" exec in
    (* Compute "minimal" addr dependency as addr \ ((addr | data); po) *)
    let minimal_addr =
      let addr = Exec.relation "addr" exec in
      E.EventRel.(diff addr (sequence (union addr data) po))
    in
    (* Compute "minimal" ctrl dependency as ctrl \ (ctrl; po | po; ctrl) *)
    let minimal_ctrl =
      let ctrl = Exec.relation "ctrl" exec in
      let ctrl =
        E.EventRel.restrict_domains EU.is_global_access EU.is_global_access ctrl
      in
      E.EventRel.(diff ctrl (union (sequence ctrl po) (sequence po ctrl)))
    in
    let deps_to_describe =
      find_addr_deps ~describe_dep_path code exec minimal_addr
      @ find_data_deps ~describe_dep_path code exec data
      @ find_ctrl_deps ~describe_dep_path code exec minimal_ctrl
    in
    let render_ctx = { code; proc_count; exec; latex_compat } in
    let described_events =
      events_to_describe
      |> List.map (fun Located.{ payload; ins } ->
          Located.make (describe_located_events render_ctx ins payload) ins)
    in
    let described_deps =
      deps_to_describe
      |> List.map (fun Located.{ payload; ins } ->
          Located.make (describe_dep render_ctx payload) ins)
    in
    let located_descriptions =
      described_events @ described_deps
      |> List.sort Located.compare_by_proc_poi
      |> List.map Located.get
    in
    let constrs = constrs_of_test test exec in
    let constrs_descriptions = List.map (describe_constr render_ctx) constrs in
    located_descriptions @ constrs_descriptions
end

let format_line_endings items =
  let items_count = List.length items in
  items
  |> List.mapi (fun ix s ->
      let line_end = if ix = items_count - 1 then "." else ";" in
      Printf.sprintf "%s%s" s line_end)

let format_bullets ~latex_compat items =
  let bullet = if latex_compat then "  \\item" else "*" in
  let formatted_items =
    items
    |> List.map (fun item ->
        Printf.sprintf "%s %s" bullet (String.capitalize_ascii item))
    |> String.concat "\n"
  in
  if latex_compat then
    String.concat "\n" [ "\\begin{itemize}"; formatted_items; "\\end{itemize}" ]
  else formatted_items

let format_description ~latex_compat items =
  items |> format_line_endings |> format_bullets ~latex_compat

let explain_test ?libdir ?(latex_compat = false) ?(describe_dep_path = false)
    contents =
  let open Misc.Result.Syntax in
  let* parsed =
    try Ok (LitmusTest.from_string contents)
    with Invalid_argument msg -> Error msg
  in
  let info = parsed.MiscParser.info in
  let* () =
    match List.assoc_opt MiscParser.variant_key info with
    | Some variant -> Error (Printf.sprintf "variant `%s`" variant)
    | None -> Ok ()
  in
  let proc_count = List.length parsed.MiscParser.prog in
  let code = LitmusTest.collect_instructions parsed in
  let outcome = HerdDriver.top ~libdir contents in
  let module R = (val outcome : RunTest.Outcome) in
  let module Expl = Make (R.M.S) in
  let descriptions_rev, _ =
    HerdDriver.fold_execs
      (fun exec acc ->
        let descr =
          Expl.describe_execution ~latex_compat ~describe_dep_path ~proc_count
            code R.test exec
          |> format_description ~latex_compat
        in
        descr :: acc)
      [] R.result
  in
  Ok (descriptions_rev |> List.rev |> Util.List.uniq ~eq:String.equal)
