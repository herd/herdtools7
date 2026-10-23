(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2026-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Apply test generator *)

module type Config = sig
  type state
  val verbose : int
  val check_name : string -> bool
end

module Make
    (Opt:Config)
    (Copy:functor(Out:OutTests.S) ->
        CopyTest.S with type out = Out.t and type state = Opt.state) = struct

  module MakeInside(Out:OutTests.S) =  struct

    module GenTest = Copy(Out)

    let do_split st idx_out fname in_chan =
      let splitted = GenTest.split fname in_chan in
      let { Splitter.name; _ } = splitted in
      let name = name.Name.name in
      if Opt.check_name name then
        GenTest.from_splitted st idx_out splitted in_chan
      else st

    let from_file idx_out fname st =
      try
        Misc.input_protect
          (do_split st idx_out fname)
          fname
      with Misc.Exit -> st
         | Misc.Fatal msg|Misc.UserError msg ->
             Printf.eprintf "Fatal error is not fatal, %s\n" msg ;
             st

    let from_tokens tokens st =
      let idx_out = Out.open_all () in
      let _ =
        Misc.output_protect_close Out.close
          (fun idx_out ->
             Misc.fold_argv_or_stdin (from_file idx_out) tokens st)
        idx_out in
      Out.tar ()
  end

  let from_tokens tokens st0 tar =
    match tar with
    | None ->
        let module X = MakeInside(OutStd) in
        X.from_tokens tokens st0
    | Some _ as t ->
        let module T =
          OutTar.Make
            (struct
              let verbose = Opt.verbose
              let outname = t
            end) in
        let module X = MakeInside(T) in
        X.from_tokens tokens st0

end
