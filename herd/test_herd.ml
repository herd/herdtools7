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

type proc_info = (string * int list) list

type
  ('prog,'nice_prog,'start,'ret,'entry,'state,
   'size_env, 'type_env,
   'prop,'loc,'locset,'fset) t =
    {
     arch : Archs.t ;
     name : Name.t ;
     info : MiscParser.info ;
     program : 'prog ;
     nice_prog : 'nice_prog ;
     start_points : 'start ;
     code_segment : 'ret ;
     entry_points : 'entry;
     init_state : 'state ;
     size_env : 'size_env ; type_env : 'type_env ;
     filter : 'prop option ;
     cond : 'prop ConstrGen.constr ;
     flocs : 'loc ConstrGen.rloc list ;
     ffaults: 'fset;
     observed : 'locset ;
     displayed : 'locset ;
     extra_data : MiscParser.extra_data ;
     access_size : MachSize.sz list ;
     proc_info : proc_info ;
    }

(* Name and nothing else *)
let simple_name test = test.name.Name.name

(* human-readable test name/filename combination *)
let readable_name test =  test.name.Name.name

(* and just the first part of that, for use in latex index *)
let very_readable_name test =  test.name.Name.name

(* Name from filename *)
let basename test =
  let base = Filename.basename test.name.Name.file in
  try Filename.chop_extension base
  with Invalid_argument _ -> base

module Make(A:Arch_herd.S) =
  struct

    type result =
      (A.program, A.nice_prog, A.start_points, A.code_segment, A.entry_points, A.state,
       A.size_env, A.type_env,
       A.prop, A.location, A.RLocSet.t,A.FaultAtomSet.t) t


(* Symb register allocation is external, since litmus needs it *)
    module ArchAlloc = struct
      include A

      (* Here values and global (addresses) are identical,
         NB: this is not the case for litmus! *)
      let maybevToV = V.maybevToV
      type global = A.V.v
      let maybevToGlobal = V.maybevToV

      module FaultType = A.I.FaultType
    end

   module Alloc = SymbReg.Make(ArchAlloc)

(* Code loader is external, since litmus tests need it too *)
    module Load = Loader.Make(A)

    let collect_atom a r =
      let open ConstrGen in
      match a with
      | LV (loc,_v) -> A.RLocSet.add loc r
      | LL (l1,l2) -> A.RLocSet.add (Loc l1) (A.RLocSet.add (Loc l2) r)
      | FF _ ->  r

    let collect_atom_fault a r =
      let open ConstrGen in
      match a with
      | (LV _|LL _) -> r
      | FF f -> f::r


(*******************)
(* Mem size access *)
(*******************)

(* From init *)
    let mem_access_size_init init =
      let szs =
        List.fold_left
          (fun k (loc,(t,_)) ->
            if A.is_global loc then A.mem_access_size_of_t t::k
            else k)
          [] init in
      MachSize.Set.of_list szs

(* From code *)
    let mem_access_size_of_code sz code =
      List.fold_left
        (A.pseudo_fold
           (fun sz0 ins -> match A.mem_access_size ins with
           | Some sz -> MachSize.Set.add sz sz0
           | None -> sz0))
        sz
        code

    let mem_access_size_prog p =
      List.fold_left
        (fun sz (_,code) -> mem_access_size_of_code sz code)
        MachSize.Set.empty p

      let mem_access_size init prog =
        if A.is_mixed then (* Useful for mixed-size only *)
          let szs =
            MachSize.Set.union
              (mem_access_size_init init)
              (mem_access_size_prog prog) in
          MachSize.Set.elements szs
        else []

(***************)
(* Entry point *)
(***************)

    let build name t =
      let t = Alloc.allocate_regs t in
      let
          {MiscParser.init = init ;
           info = info ;
           prog = nice_prog ;
           filter = filter ;
           condition = final ;
           locations = locs ;
           extra_data = extra_data ;
         } = t in

      let prog,starts,rets = Load.load nice_prog in
      (* ensure labels in the init list are present in the body of the test*)
      List.iter (fun (_,(_,v)) ->
        let open Constant in
        match v with
        | A.V.Val (Symbolic (Virtual {name=Symbol.Label(p,s); _}))
          -> begin
          if not (Label.Map.mem s prog) then 
            Warn.user_error
              "Label %s not found on P%d, yet it is used in the initialization list" s p end
        | _ -> ()) init ;
      let entry_points =
        let instr2labels =
          let one_label lbl addr res =
            let ins_lbls = IntMap.safe_find Label.Set.empty addr res in
            IntMap.add addr (Label.Set.add lbl ins_lbls) res in
          Label.Map.fold one_label prog IntMap.empty in
        fun addr -> IntMap.safe_find Label.Set.empty addr instr2labels in
      let type_env = A.build_type_env init in
      let init_state = A.build_state type_env init in
      let flocs,ffaults = LocationsItem.locs_and_faults locs in
      let displayed =
        let flocs = A.RLocSet.of_list flocs in
        ConstrGen.fold_constr collect_atom final flocs in
      let observed = match filter with
      | None -> displayed
      | Some filter ->
          ConstrGen.fold_prop collect_atom filter displayed in
      let ffaults = A.FaultAtomSet.of_list
          (ConstrGen.fold_constr collect_atom_fault final ffaults) in
      let proc_info =
        let m =
          List.fold_left
            (fun m ((p,ao,_),_) -> match ao with
            | None -> m
            | Some ans ->
                List.fold_left
                  (fun m an ->
                    let old = StringMap.safe_find [] an m in
                    StringMap.add an (p::old) m)
                  m ans)
            StringMap.empty nice_prog in
        StringMap.bindings m in
      {
       arch = A.arch ;
       name = name ;
       info = info ;
       program = prog ;
       nice_prog = nice_prog ;
       start_points = starts ;
       code_segment = rets ;
       entry_points = entry_points;
       init_state = init_state ;
       filter = filter ;
       cond = final ;
       flocs = flocs ; ffaults;
       observed = observed ;
       displayed = displayed ;
       extra_data = extra_data ;
       size_env = A.build_size_env init ;
       type_env;
       access_size = mem_access_size init nice_prog ;
       proc_info;
     }

    let empty_test =
      let empty_name =
        {
          Name.name = "";
          Name.file = "";
          Name.texname = "";
          Name.doc = "";
        }
      in
    let fake_constr =
      ConstrGen.ExistsState (ConstrGen.And [])
      in
      {
       arch = A.arch ;
       name = empty_name ;
       info = [] ;
       program = Label.Map.empty ;
       nice_prog = [] ;
       start_points = [] ;
       code_segment = IntMap.empty ;
       entry_points = (fun _ -> Label.Set.empty) ;
       init_state = A.state_empty;
       size_env = A.size_env_empty ;
       type_env = A.type_env_empty ;
       filter = None ;
       cond = fake_constr ;
       flocs = [] ; ffaults = A.FaultAtomSet.empty;
       observed = A.RLocSet.empty;
       displayed = A.RLocSet.empty;
       extra_data = MiscParser.empty_extra;
       access_size = [];
       proc_info = [];
     }

    let find_our_constraint test = test.cond

end
