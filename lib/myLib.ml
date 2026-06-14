(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Find my files *)

module type Config = sig
  val includes : string list
  val env : string option
  val libdir : string
  val debug : bool
end

let pp_debug name = Printf.eprintf "Found: '%s'\n%!" name

module Make =
  functor (C:Config) -> struct

    let debug = C.debug

    (* TODO: could refine this to check whether the file is a regular file or symlink *)
    let nondir_file_exists path =
      Sys.file_exists path && not (Sys.is_directory path)

    let try_find name dir =
      let rname = Filename.concat dir name in
      if nondir_file_exists rname then begin
        if debug then pp_debug rname ;
        Some rname
      end else None

    let envlib = match C.env with
    | None -> None
    | Some v ->
        try Some (Sys.getenv v) with Not_found -> None

    let includes =
      List.map
        (fun d ->
          let len = String.length d in
          if len > 0 && d.[0] = '+' then
            Filename.concat C.libdir (String.sub d 1 (len-1))
          else d)
        C.includes

    let all_locations =
      List.concat [ ["."] ; includes ; Option.to_list envlib; [ C.libdir ] ]

    let find_lib name =
      match List.find_map (try_find name) all_locations with
      | Some path -> path
      | None -> Warn.fatal "Cannot find file %s" name

    let find path =
      if Filename.is_implicit path then find_lib path else path
  end
