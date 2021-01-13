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

type ('prog,'nice_prog,'start,'state,'size_env, 'type_env, 'prop,'loc,'locset) t =
    {
     arch : Archs.t ;
     name : Name.t ;
     info : MiscParser.info ;
     program : 'prog ;
     nice_prog : 'nice_prog ;
     start_points : 'start ;
     init_state : 'state ;
     size_env : 'size_env ; type_env : 'type_env ;
     filter : 'prop option ;
     cond : 'prop ConstrGen.constr ;
     flocs : 'loc ConstrGen.rloc list ;
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
        (A.program, A.nice_prog, A.start_points,
         A.state, A.size_env, A.type_env, A.prop, A.location, A.RLocSet.t) t

(* Symb register allocation is external, since litmus needs it *)
    module ArchAlloc = struct
      include A

      (* Here values and global (addresses) are identical,
         NB: this is not the case for litmus! *)
      let maybevToV = V.maybevToV
      type global = A.V.v
      let maybevToGlobal = V.maybevToV
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

(* Mem size access *)
    let mem_access_size_of_code sz code =
      List.fold_left
        (A.pseudo_fold
           (fun sz0 ins -> match A.mem_access_size ins with
           | Some sz -> MachSize.Set.add sz sz0
           | None -> sz0))
        sz
        code

    let mem_access_size_prog p =
      let s =
        List.fold_left
          (fun sz (_,code) -> mem_access_size_of_code sz code)
          MachSize.Set.empty p in
      MachSize.Set.elements s

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

      let prog,starts = Load.load nice_prog in
      let flocs = List.map fst locs in
      let init_state = A.build_state init in
      (* init_state contains vector size metadata, add it to final constrs*)
      (* Needed so we can align a scaled access according to size of the type *)
      let type_env = A.build_type_env init in
      let displayed = (* Luc: Doubt purpose of displayed ? *)
        let flocs = A.RLocSet.of_list flocs in
        ConstrGen.fold_constr collect_atom final flocs in
      let observed = match filter with
      | None -> displayed
      | Some filter ->
          ConstrGen.fold_prop collect_atom filter displayed in
      let proc_info =
        let m =
          List.fold_left
            (fun m ((p,ao),_) -> match ao with
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
       init_state = init_state ;
       filter = filter ;
       cond = final ;
       flocs = flocs ;
       observed = observed ;
       displayed = displayed ;
       extra_data = extra_data ;
       size_env = A.build_size_env init ;
       type_env;
       access_size = mem_access_size_prog nice_prog ;
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
       program = A.LabelMap.empty ;
       nice_prog = [] ;
       start_points = [] ;
       init_state = A.state_empty;
       size_env = A.size_env_empty ;
       type_env = A.type_env_empty ;
       filter = None ;
       cond = fake_constr ;
       flocs = [] ;
       observed = A.RLocSet.empty; displayed = A.RLocSet.empty;
       extra_data = MiscParser.empty_extra;
       access_size = [];
       proc_info = [];
     }

    let find_our_constraint test = test.cond

end
