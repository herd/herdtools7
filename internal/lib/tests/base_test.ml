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

(** Tests for the Base modules. *)

let tests = [
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

  "Base.List.split_when", (fun () ->
    let tests = [
      (([], (fun _ -> true)), ([], [])) ;
      ((['a'; 'b'; 'c'], (Char.equal 'a')), ([], ['a'; 'b'; 'c'])) ;
      ((['a'; 'b'; 'c'], (Char.equal 'b')), (['a'], ['b'; 'c'])) ;
      ((['a'; 'b'; 'c'], (Char.equal 'c')), (['a'; 'b'], ['c'])) ;
      ((['a'; 'b'; 'c'], (Char.equal 'd')), (['a'; 'b'; 'c'], [])) ;
    ] in

    let tuple_to_string a_str b_str (a, b) =
      Printf.sprintf "(%s, %s)" (a_str a) (b_str b)
    in

    let charlist_to_string = Base.List.to_ocaml_string (String.make 1) in
    let result_to_string =
      tuple_to_string charlist_to_string charlist_to_string
    in

    List.iter
      (fun ((xs, p), expected) ->
        let actual = Base.List.split_when p xs in
        if not (actual = expected)  then
          let expected = result_to_string expected in
          let actual = result_to_string actual in
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
