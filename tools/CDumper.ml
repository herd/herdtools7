(****************************************************************************)
(*                           the diy7 toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* C dumper *)

module type Out = sig
  type t
  val fprintf : t -> ('a, out_channel, unit) format -> 'a
end

module Make(Out:Out) : sig
  val dump :
      Out.t -> Name.t -> CBase.pseudo MiscParser.t -> unit
  val dump_withhash :
      Out.t -> Name.t -> CBase.pseudo MiscParser.t -> unit
end = struct

  open Printf
  open MiscParser

(* state *)
  let dump_loc = MiscParser.dump_location

  let dump_atom_state a =
    MiscParser.dump_state_atom
      MiscParser.is_global dump_loc ParsedConstant.pp_v a

  let dump_state st =
    let st =
      List.filter
        (function
          | Location_global _,_ -> true
          | Location_reg _,_ -> false
          | Location_sreg _,_ ->
              Warn.fatal "wrong location in init")
        st in
    String.concat " "
      (List.map
         (fun a -> sprintf "%s;" (dump_atom_state a))
         st)

(* Propositions *)
  let dump_atom a =
    ConstrGen.dump_atom dump_loc MiscParser.dump_location_brk
      ParsedConstant.pp_v a

  let dump_prop = ConstrGen.prop_to_string dump_atom
  let dump_constr = ConstrGen.constraints_to_string dump_atom

(* Parameters *)
  let dump_param p =
    let open CAst in
    sprintf "%s %s" (CType.dump p.param_ty) p.param_name

  let dump_params ps = String.concat "," (List.map dump_param ps)

  let do_dump with_hash chan doc t =
    Out.fprintf chan "%s %s\n" (Archs.pp CBase.arch) doc.Name.name ;
    List.iter
      (fun (k,i) ->
        if with_hash || k <> MiscParser.hash_key then
          Out.fprintf chan "%s=%s\n" k i)
      t.info ;
    Out.fprintf chan "\n{%s}\n\n" (dump_state  t.init) ;
    begin match t.extra_data with
    | CExtra pss ->
        List.iter2
          (fun ((i,_,_),code) ps ->
            Out.fprintf chan "\nP%i(%s) {\n" i (dump_params ps) ;
            List.iter
              (fun pseudo ->
                CBase.pseudo_iter
                  (fun i ->
                    Out.fprintf chan "%s\n" (CBase.do_dump_instruction "  " i))
                  pseudo)
              code ;
            Out.fprintf chan "}\n")
          t.prog pss
    | _ -> ()
    end ;
    let locs = DumpUtils.dump_locations dump_loc ParsedConstant.pp_v t.locations in
    if locs <> "" then Out.fprintf chan "%s\n" locs ;
    begin match t.filter with
    | None -> ()
    | Some p -> Out.fprintf chan "filter %s" (dump_prop p)
    end ;
    Out.fprintf chan "\n%s\n" (dump_constr t.condition) ;
    ()

    let dump = do_dump false
    let dump_withhash = do_dump true

end
