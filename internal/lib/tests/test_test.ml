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

(** Tests for the Test module. *)

let pp_int_list = Test.pp_int_list
let pp_string_list = Test.pp_string_list

let tests = [
  "Test.pp_int_list", (fun () ->
    let tests = [
      [], "[]" ;
      [1], "[1]" ;
      [1; 2; 3], "[1; 2; 3]" ;
    ] in

    List.iter
      (fun (xs, expected) ->
        let actual = Test.pp_int_list xs in
        if String.compare actual expected <> 0 then
          Test.fail (Printf.sprintf "Expected %s, got %s" expected actual)
      )
      tests
  );

  "Test.pp_string_list", (fun () ->
    let tests = [
      [], "[]" ;
      ["a"], "[\"a\"]" ;
      ["a"; "b"], "[\"a\"; \"b\"]" ;
    ] in

    List.iter
      (fun (xs, expected) ->
        let actual = Test.pp_string_list xs in
        if String.compare actual expected <> 0 then
          Test.fail (Printf.sprintf "Expected %s, got %s" expected actual)
      )
      tests
  );

  "Test.int_list_compare", (fun () ->
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
        let actual = Test.int_list_compare xs ys in
        if actual <> expected then
          Test.fail (Printf.sprintf "Expected %i, got %i" expected actual)
      )
      tests
  );

  "Test.string_list_compare", (fun () ->
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
        let actual = Test.string_list_compare xs ys in
        if actual <> expected then
          Test.fail (Printf.sprintf "Expected %i, got %i" expected actual)
      )
      tests
  );
]

let () = Test.run tests
