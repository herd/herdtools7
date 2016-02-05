(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(****************)
(* Command line *)
(****************)

let parse_select select = 
  "-select",
  Arg.String (fun s ->  select := !select @ [s]),
  "<name> specify test or test index  file, can be repeated"

let parse_names names =
  "-names",
  Arg.String (fun s ->  names := !names @ [s]),
  "<name> specify file of names, can be repeated"

let parse_rename rename =
  "-rename", Arg.String (fun s -> rename := !rename @ [s]),     
  "<name> specify a rename mapping, hashes are checked"

let parse_excl excl =
  "-excl", Arg.String (fun s -> excl := !excl @ [s]),     
  "<name> specify file of names to be excluded, can be repeated"

module
  Make
    (I:sig
      val verbose : int
      val rename : string list
      val select : string list
      val names : string list
      val excl : string list
    end) =
  struct
(******************)
(* Rename mapping *)
(******************)

    module LR = LexRename.Make(I)

    let rename_table = LR.read_from_files I.rename (fun s -> Some s)

    let rename name = 
      try TblRename.find_value rename_table name
      with Not_found -> name

    let rename_opt name = TblRename.find_value_opt rename_table name

(******************)
(* Name selection *)
(******************)
    let names1 = match I.select with
    | [] -> None
    | args ->
        let names = Names.from_fnames (Misc.expand_argv args) in
        let names = List.rev_map rename names in
        let set = StringSet.of_list names in
        Some set

    let names2 = match I.names with
    | [] -> None
    | args ->
        let names =
          List.fold_left
            (fun r name -> ReadNames.from_file (rename name) Misc.cons r)
            [] args in
        let set = StringSet.of_list names in
        Some set


    let names_excl = match I.excl with
    | [] -> None
    | args ->
        let names =
          List.fold_left
            (fun r name -> ReadNames.from_file (rename name) Misc.cons r)
            [] args in
        let set = StringSet.of_list names in
        Some set

    let names3 = match names1,names2 with
    | (None,ns)|(ns,None) -> ns
    | Some ns1,Some ns2 -> Some (StringSet.union ns1 ns2)

    let names = match names3 with
    | None -> None
    | Some ns -> match names_excl with
      | None -> names3
      | Some e -> Some (StringSet.diff ns e)

    let ok = match names with
    | None ->
        begin match names_excl with
        | None ->fun _ -> true
        | Some e -> fun n -> not (StringSet.mem n e)
        end
    | Some ns -> fun n -> StringSet.mem n ns
  end
