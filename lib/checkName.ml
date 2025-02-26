(****************************************************************************)
(*                           The Diy Toolsuite                              *)
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

let parse_oknames  oknames =
  ArgUtils.parse_stringset "-oknames" oknames
    "<name,...,name> names of tests to be selected"

let parse_rename rename =
  "-rename", Arg.String (fun s -> rename := !rename @ [s]),
  "<name> specify a rename mapping, hashes are checked"

let parse_excl excl =
  "-excl", Arg.String (fun s -> excl := !excl @ [s]),
  "<name> specify file of names to be excluded, can be repeated"

let parse_nonames  nonames =
  ArgUtils.parse_stringset "-nonames" nonames
    "<name,...,name> names of tests to be excluded"

let parse_hexa hexa =
  "-hexa", Arg.Bool (fun b -> hexa := b),
  (Printf.sprintf "<bool> specify hexadecimal output, default %b" !hexa)

let parse_int32 int32 =
  "-int32", Arg.Bool (fun b -> int32 := b),
  (Printf.sprintf "<bool> integer in logs are 32 bits wide, default %b" !int32)

let parse_faulttype ft =
   ("-faulttype", Arg.Bool (fun b -> ft := b),
    Printf.sprintf
      "<bool> consider fault types, default %b" !ft);

module
  Make
    (I:sig
      val verbose : int
      val rename : string list
      val select : string list
      val names : string list
      val oknames : StringSet.t
      val excl : string list
      val nonames : StringSet.t
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
    | [] ->
        if StringSet.is_empty I.oknames then None
        else Some I.oknames
    | args ->
        let names = Names.from_fnames (Misc.expand_argv args) in
        let names = List.rev_map rename names in
        let set = StringSet.of_list names in
        Some (StringSet.union set I.oknames)

    let names2 = match I.names with
    | [] -> None
    | args ->
        let names =
          List.fold_left
            (fun r name -> ReadNames.from_file (rename name) Misc.cons r)
            [] args in
        let set = StringSet.of_list names in
        Some set


    let names_excl_files = match I.excl with
    | [] -> StringSet.empty
    | args ->
        let names =
          List.fold_left
            (fun r name -> ReadNames.from_file (rename name) Misc.cons r)
            [] args in
        let set = StringSet.of_list names in
        if I.verbose > 1 then
          Printf.eprintf "Excl {%s}\n" (StringSet.pp_str "," (fun s -> s) set) ;
        set

    let names_excl = StringSet.union names_excl_files I.nonames

    let names3 = match names1,names2 with
    | (None,ns)|(ns,None) -> ns
    | Some ns1,Some ns2 -> Some (StringSet.union ns1 ns2)

    let names = match names3 with
    | None -> None
    | Some ns -> Some (StringSet.diff ns names_excl)

    let ok =
      match names with
      | None ->
          let is_ok n = not (StringSet.mem n names_excl) in
          if I.verbose > 1 then
            fun n ->
              let b = is_ok n in
              Printf.eprintf "Check %s -> %b\n" n b ;
              b
          else is_ok
      | Some ns -> fun n -> StringSet.mem n ns
  end
