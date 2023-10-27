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

(** Tests for the Log module. *)

module Fun = Base.Fun

module LogList = struct
  let compare = Base.List.compare Log.compare
  let to_ocaml_string = Base.List.to_ocaml_string Log.to_ocaml_string
end

let tests = [
  "Log.of_string_list", (fun () ->
    let tests = [
      [
        "Test A4 Allowed" ;
        "States 1" ;
        "0:X0=0;" ;
        "Ok" ;
        "Witnesses" ;
        "Positive: 1 Negative: 0" ;
        "Condition exists (0:X0=0)" ;
        "Observation A4 Always 1 0" ;
        "Time A4 0.00" ;
        "Hash=b0a348f458429140b16552cc100cdc7c" ;
      ], [
        {
          Log.name = "A4" ;
          Log.kind = Some ConstrGen.Allow ;
        } ;
      ] ;

      [
        "Test A4 Allowed" ;
        "States 1" ;
        "0:X0=0;" ;
        "Ok" ;
        "Witnesses" ;
        "Positive: 1 Negative: 0" ;
        "Condition exists (0:X0=0)" ;
        "Observation A4 Always 1 0" ;
        "Time A4 0.00" ;
        "Hash=b0a348f458429140b16552cc100cdc7c" ;
        "" ;
        "Test A8 Forbid" ;
        "States 1" ;
        "0:X0=0; 0:X1=x+44;" ;
        "Ok" ;
        "Witnesses" ;
        "Positive: 1 Negative: 0" ;
        "Condition exists (0:X0=0 /\ not (0:X1=x))" ;
        "Observation A8 Always 1 0" ;
        "Time A8 0.00" ;
        "Hash=d1591baf0ba882a97685e1fa37c64e74" ;
      ], [
        {
          Log.name = "A4" ;
          Log.kind = Some ConstrGen.Allow ;
        } ;
        {
          Log.name = "A8" ;
          Log.kind = Some ConstrGen.Allow ;
        } ;
      ] ;
    ] in

    List.iteri (fun i (log, expected) ->
      let actual = Log.of_string_list log in

      if LogList.compare actual expected <> 0 then
        Test.fail (Printf.sprintf "[%i] expected %s, got %s" i (LogList.to_ocaml_string expected) (LogList.to_ocaml_string actual))
    ) tests
  );
]

let () = Test.run tests
