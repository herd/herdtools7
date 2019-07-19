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

let webpath = "/jherd"
let bell_fname = ref "error"
let cat_fname = ref "error"
let cfg_fname = ref "error"
let litmus_fname = ref "error"

let bell_str = ref "error"
let cat_str = ref "error"
let cfg_str = ref "error"
let litmus_str = ref "error"

(* TODO: doesn't work... passing hash from JavaScript instead for now *)
let hash str =
   let hash = ref 5381 in
   let each_char c =
     hash := ((!hash lsl 5) + !hash) + (Char.code c); (* hash * 33 + c *) in
   String.iter each_char str;
   !hash

let set_str suffix str_ref _contents hash =
  let name =  sprintf "web-%x%s"  hash suffix in
  eprintf "STR: %s\n" name ;
  str_ref := name

let set_bell_str contents hash =
  set_str ".bell" bell_fname contents hash;
  bell_str := contents

let set_cat_str contents hash =
  set_str ".cat" cat_fname contents hash;
  cat_str := contents

let set_cfg_str contents hash =
  set_str ".cfg" cfg_fname contents hash;
  cfg_str := contents

let set_litmus_str contents hash =
  set_str ".litmus" litmus_fname contents hash;
  litmus_str := contents

let autoloader ~prefix ~path =
  eprintf "Autoload: prefix=%s, path=%s\n%!" prefix path ;
  let fname_to_str = [
    (!bell_fname, !bell_str) ;
    (!cat_fname, !cat_str) ;
    (!cfg_fname, !cfg_str) ;
    (!litmus_fname, !litmus_str)] in
  let r =
    try Some (List.assoc path fname_to_str)
    with Not_found ->
      try CatIncludes.autoloader ~prefix:prefix ~path:path
      with Not_found ->
        None in
  match r with
  | None -> r
  | Some ct ->
      Printf.eprintf "Contents of %s:\n" path ;
      Printf.eprintf "%s" ct ;
      r

let register_autoloader () =
  Js_of_ocaml.Sys_js.unmount ~path:webpath ;
  Js_of_ocaml.Sys_js.mount ~path:webpath autoloader
