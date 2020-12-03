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

(** Tests for the Compare module. *)

let tests = [
  "Compare.chain", (fun () ->
    let tests = [
      [], 0 ;
      [0; 0], 0 ;
      [1; 0], 1 ;
      [-1; 1; 0], -1 ;
      [0; -1; 1; 0], -1 ;
    ] in

    List.iter
      (fun (cs, expected) ->
        let actual = Compare.chain cs in
        if actual <> expected then
          Test.fail (Printf.sprintf "expected %i, got %i" expected actual)
      )
      tests
  );
]

let () = Test.run tests
