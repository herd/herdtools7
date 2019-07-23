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

open Printf

let dbg = false

let webpath = "/jherd"
let bell_fname = ref "error"
let cat_fname = ref "error"
let cfg_fname = ref "error"
let litmus_fname = ref "error"
let bell_str = ref "error"
let cat_str = ref "error"
let cfg_str = ref "error"
let litmus_str = ref "error"


let hash contents = Digest.to_hex (Digest.string contents)

let set_str suffix str_ref contents  =
  let h = hash contents in
  let name =  sprintf "web-%s%s" h suffix in
  if dbg then eprintf "JHERD: set_str name=%s\n%!" name ;
  str_ref := name ;
  name

let set_bell_str contents =
  bell_str := contents ;
  set_str ".bell" bell_fname contents


let set_cat_str contents =
  cat_str := contents ;
  set_str ".cat" cat_fname contents

let set_cfg_str contents =
  cfg_str := contents ;
  set_str ".cfg" cfg_fname contents

let set_litmus_str contents =
  litmus_str := contents ;
  set_str ".litmus" litmus_fname contents

let pp path cts =
  if dbg then begin match cts with
  | Some cts ->
      Printf.eprintf "** Found %s in pseudo file system\n" path ;
      Printf.eprintf "%s\n"  cts
  | None -> ()
  end ;
  cts

let autoloader ~prefix ~path =
  let fname_to_str = [
    (!bell_fname, !bell_str) ;
    (!cat_fname, !cat_str) ;
    (!cfg_fname, !cfg_str) ;
    (!litmus_fname, !litmus_str)] in
  try Some (List.assoc path fname_to_str)
  with Not_found ->
    try pp path (CatIncludes.autoloader ~prefix:prefix ~path:path)
    with Not_found ->  None

let register_autoloader () =
  Js_of_ocaml.Sys_js.unmount ~path:webpath ;
  Js_of_ocaml.Sys_js.mount ~path:webpath autoloader
