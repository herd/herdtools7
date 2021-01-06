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

(** Tests for the Base modules. *)

let tests = [
  "Base.Fun.protect calls both f and finally", (fun () ->
    let called_f = ref false in
    let called_finally = ref false in

    Base.Fun.protect
      ~finally:(fun () -> called_finally := true)
      (fun () -> called_f := true) ;

    if not !called_f then
      Test.fail "did not call f" ;

    if not !called_finally then
      Test.fail "did not call finally"
  );
  "Base.Fun.protect calls finally before re-raising exception", (fun () ->
    let called_finally = ref false in

    let raised_exception =
      try
        Base.Fun.protect
          ~finally:(fun () -> called_finally := true)
          (fun () -> if true then raise Not_found ; ()) ;
        false
      with Not_found -> true
    in

    if not raised_exception then
      Test.fail "did not re-raise exception" ;

    if not !called_finally then
      Test.fail "did not call finally"
  );
  "Base.Fun.protect wraps exceptions raised by finally", (fun () ->
    let raised_exception =
      try
        Base.Fun.protect
          ~finally:(fun () -> raise Not_found)
          (fun () -> ()) ;
        false
      with Base.Fun.Finally_raised Not_found -> true
    in

    if not raised_exception then
      Test.fail "did not wrap & re-raise exception" ;
  );

  "Base.List.compare", (fun () ->
    let tests = [
      [], [], 0 ;
      ["a"], [], 1 ;
      [], ["a"], -1 ;
      ["a"], ["a"], 0 ;
      ["a"], ["b"], -1 ;
    ] in

    List.iteri
      (fun i (xs, ys, expected) ->
        let actual = Base.List.compare Base.String.compare xs ys in
        if actual <> expected then
          Test.fail (Printf.sprintf "[%i] expected %i, got %i" i expected actual)
      )
      tests
  );

  "Base.List.to_ocaml_string", (fun () ->
    let tests = [
      [], "[]" ;
      ["a"], "[\"a\"]" ;
      ["a"; "b"], "[\"a\"; \"b\"]" ;
    ] in

    List.iter
      (fun (xs, expected) ->
        let actual = Base.List.to_ocaml_string Base.String.to_ocaml_string xs in
        if String.compare actual expected <> 0 then
          Test.fail (Printf.sprintf "expected %s, got %s" expected actual)
      )
      tests
  );

  "Base.Option.compare", (fun () ->
    let tests = [
      None, None, 0 ;
      Some "a", None, 1 ;
      None, Some "a", -1 ;
      Some "a", Some "a", 0 ;
      Some "a", Some "b", -1 ;
    ] in

    List.iteri
      (fun i (xs, ys, expected) ->
        let actual = Base.Option.compare Base.String.compare xs ys in
        if actual <> expected then
          Test.fail (Printf.sprintf "[%i] expected %i, got %i" i expected actual)
      )
      tests
  );

  "Base.Option.to_ocaml_string", (fun () ->
    let tests = [
      None, "None" ;
      Some "a", "Some (\"a\")" ;
    ] in

    List.iter
      (fun (xs, expected) ->
        let actual = Base.Option.to_ocaml_string Base.String.to_ocaml_string xs in
        if String.compare actual expected <> 0 then
          Test.fail (Printf.sprintf "expected %s, got %s" expected actual)
      )
      tests
  );
]

let () = Test.run tests

