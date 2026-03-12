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
open Js_of_ocaml

let dbg = false

let webpath = "/jherd"
let webpath_basename = Filename.basename webpath
let fetch_dir = "weblib"
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

let log_js_error e =
  try
    Js.Unsafe.fun_call
      (Js.Unsafe.variable "console.error")
      [| Js.Unsafe.inject e |]
  with
  | Js.Error _ -> ()

let log_error s =
 log_js_error (Js.string s)

let autoloader ~prefix ~path =
  let fname_to_str = [
    (!bell_fname, !bell_str) ;
    (!cat_fname, !cat_str) ;
    (!cfg_fname, !cfg_str) ;
    (!litmus_fname, !litmus_str)] in
  try Some (List.assoc path fname_to_str)
  with Not_found ->
    let dir = Filename.basename prefix in
    let fname = if dir = webpath_basename then path
    else Filename.concat dir path in
    let result =
      try
        let url = Filename.concat fetch_dir fname in
        let req = XmlHttpRequest.create () in
        (* The false argument causes the request to be made synchronously.
          Required since herd has no way to delay resolving dependencies. *)
        req##_open (Js.string "GET") (Js.string url) (Js.bool false);
        req##send Js.null;
        if req##.status = 200 then
          match Js.Opt.to_option req##.responseText with
          | Some txt -> Some (Js.to_string txt)
          | None ->
              log_error
                (Printf.sprintf "autoloader: GET %s succeeded but returned \
                  empty response text (status=%d)" url req##.status);
              None
        else begin
          let status_text = Js.to_string req##.statusText in
          log_error
            (Printf.sprintf "autoloader: GET %s returned status %d (%s)"
              url req##.status status_text);
          None
        end
      with
      | Js.Error e ->
          log_js_error e;
          None in
    if dbg then
      Option.iter (fun cts ->
        Printf.eprintf "** Found %s in pseudo file system\n" path ;
        Printf.eprintf "%s\n" cts
      ) result;
    result

let get_env_webpath path =
  Filename.concat webpath path

let register_autoloader env =
  let paths = webpath :: Option.fold ~some:(fun env ->
    [get_env_webpath env]
  ) ~none:[] env in
  List.iter (fun webpath ->
    Js_of_ocaml.Sys_js.unmount ~path:webpath ;
    Js_of_ocaml.Sys_js.mount ~path:webpath autoloader
  ) paths
