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

(** Tests for the ExtendedBaseModules module. *)

let tests = [
  "ExtendedBaseModules.Int.to_ocaml_string", (fun () ->
    let tests = [
      0, "0" ;
      1, "1" ;
      -1, "-1" ;
    ] in

    List.iter
      (fun (i, expected) ->
        let actual = ExtendedBaseModules.Int.to_ocaml_string i in
        if String.compare actual expected <> 0 then
          Test.fail (Printf.sprintf "Expected %s, got %s" expected actual)
      )
      tests
  );

  "ExtendedBaseModules.String.to_ocaml_string", (fun () ->
    let to_ocaml_string = ExtendedBaseModules.String.to_ocaml_string in
    let tests = [
      "", "\"\"" ;
      "a", "\"a\"" ;
      "a\"", "\"a\\\"\"" ;
    ] in

    List.iter
      (fun (x, expected) ->
        let actual = to_ocaml_string x in
        if String.compare actual expected <> 0 then
          Test.fail (Printf.sprintf "Expected %s, got %s" expected actual)
      )
      tests
  );

  "ExtendedBaseModules.List.to_ocaml_string(String)", (fun () ->
    let to_ocaml_string = ExtendedBaseModules.List.to_ocaml_string ExtendedBaseModules.String.to_ocaml_string in
    let tests = [
      [], "[]" ;
      ["a"], "[\"a\"]" ;
      ["a"; "b"], "[\"a\"; \"b\"]" ;
    ] in

    List.iter
      (fun (xs, expected) ->
        let actual = to_ocaml_string xs in
        if String.compare actual expected <> 0 then
          Test.fail (Printf.sprintf "Expected %s, got %s" expected actual)
      )
      tests
  );

  "ExtendedBaseModules.List.compare(Int)", (fun () ->
    let compare = ExtendedBaseModules.List.compare ExtendedBaseModules.Int.compare in
    let tests = [
      [], [], 0 ;
      [1], [], 1 ;
      [], [1], -1 ;
      [1], [1], 0 ;
      [1], [2], -1 ;
      [1; 2], [12; 2], -1 ;
    ] in

    List.iter
      (fun (xs, ys, expected) ->
        let actual = compare xs ys in
        if actual <> expected then
          Test.fail (Printf.sprintf "Expected %i, got %i" expected actual)
      )
      tests
  );

  "ExtendedBaseModules.List.compare(String)", (fun () ->
    let compare = ExtendedBaseModules.List.compare String.compare in
    let tests = [
      [], [], 0 ;
      ["a"], [], 1 ;
      [], ["a"], -1 ;
      ["a"], ["a"], 0 ;
      ["a"], ["b"], -1 ;
      ["a"; "b"], ["ab"; "b"], -1 ;
    ] in

    List.iter
      (fun (xs, ys, expected) ->
        let actual = compare xs ys in
        if actual <> expected then
          Test.fail (Printf.sprintf "Expected %i, got %i" expected actual)
      )
      tests
  );
]

let () = Test.run tests
