(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** A tool that runs herd, redirecting stderr and stdout *)

let litmus = Sys.argv.(Array.length Sys.argv -1)

let rec to_list k =
  if k+1 >= Array.length Sys.argv then []
  else Sys.argv.(k)::to_list (k+1)

let com = Sys.argv.(1)
let args = to_list 2

let out_name = TestHerd.outname litmus
and err_name = TestHerd.errname litmus

let cat p out_chan line =
  if p line then Printf.fprintf out_chan "%s\n" line


let run out err =
  let stdout = cat TestHerd.is_stable out
  and stderr = cat (fun _ -> true) err
  and stdin = Base.Iter.of_list [litmus] in
  ignore
    (Command.NonBlock.run_status ~stdin ~stdout ~stderr com args)

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
