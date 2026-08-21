(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** A tool that runs herd, redirecting stderr and stdout *)

let Args.{args; com; wrapped} = Args.split_wrapper_args Sys.argv

type flags = {verbose:bool}
let noflags = {verbose=false}

let {verbose}, litmus =
  let get_litmus = function
    | Some litmus -> litmus
    | None ->
         Printf.eprintf "%s: Could not find litmus among arguments: [%s]\n%!"
           Sys.argv.(0) (String.concat "; " args) ;
         exit 1 in
  let rec gather_args (flags, litmus) args =
    match args with
    | [] -> flags, get_litmus litmus
    | "-verbose" :: args ->
        gather_args ({verbose=true}, litmus) args
    | _ :: args ->
        gather_args (flags, litmus) args
  in
  gather_args (noflags, None) args

let out_name = TestHerd.outname litmus
and err_name = TestHerd.errname litmus

let cat p out_chan line =
  if verbose && TestHerd.check_tags line then prerr_endline line ;
  if p line then Printf.fprintf out_chan "%s\n" line


let run out err =
  let stdout = cat TestHerd.is_stable out
  and stderr = cat (fun _ -> true) err
  and stdin = Base.Iter.of_list [litmus] in
  ignore
    (Command.NonBlock.run_status ~stdin ~stdout ~stderr com wrapped)

let rm_if_empty name =
  let st = Unix.stat name in
  if st.Unix.st_size = 0 then Sys.remove name

let () =
  Base.Fun.open_out_protect
    (fun out ->
      Base.Fun.open_out_protect (run out) err_name)
    out_name ;
  rm_if_empty out_name ;
  rm_if_empty err_name ;
  ()
