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

(* Open my files *)

module type Config = sig
  val includes : string list
  val env : string option
  val libdir : string
  val debug : bool
end

let pp_debug name = Printf.eprintf "Found and opened: '%s'\n" name

module Make =
  functor (C:Config) -> struct
    
    let try_open dir name =
      let rname = Filename.concat dir name in
      try
        let r = rname,open_in rname in
        if C.debug then pp_debug rname ;
        r
      with _ -> raise Exit

    let rec try_opens dirs name = match dirs with
    | [] -> raise Exit
    | dir::dirs ->
        try try_open dir name
        with Exit -> try_opens dirs name

    let envlib = match C.env with
    | None -> None
    | Some v ->
        try Some (Sys.getenv v) with Not_found -> None


    let open_lib name =
      try try_opens ("."::C.includes) name
      with Exit -> try match envlib with
      | Some lib -> try_open lib name
      | None -> raise Exit
      with Exit -> try try_open C.libdir name
      with Exit -> Warn.fatal "Cannot find file %s" name
  
    let do_find name =
      let r,chan = open_lib name in
      begin try close_in chan with _ -> () end ;
      r

    let find path =
      if Filename.is_implicit path then do_find path
      else path
  end
