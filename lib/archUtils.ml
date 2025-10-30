(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Utilities from ArchBase useful for herd and litmus *)

module Make(A:ArchBase.S) =
  struct

    let get_exported_labels_code prog =
      let lbls =
        List.fold_left
          (fun k (p,code) ->
            A.fold_pseudo_code
              (fun k i ->
                let open BranchTarget in
                match A.get_exported_label i with
                | None -> k
                | Some (Lbl lbl) ->
                   (MiscParser.proc_num p,lbl)::k
                | Some (Offset _) ->
                   Warn.user_error "Replace offset by label in instruction %s"
                     (A.dump_instruction i))
              k code)
          [] prog in
      Label.Full.Set.of_list lbls

end
