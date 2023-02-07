(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2016-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(** Output litmus files, with index file, Tar *)

module Make(O:Tar.Option) = struct
  module T = Tar.Make(O)

  type t = out_channel
  let do_open name = open_out (T.outname name)
  let open_all () = do_open "@all"
  let open_file name = do_open name
  let close chan = close_out chan
  let remove name = MySys.remove (T.outname name)
  let put_char = output_char
  let fprintf chan fmt = Printf.fprintf chan fmt
  let tar = T.tar
  let chan t = t
end
