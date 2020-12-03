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

let tests = [
  "Sexp.of_dune_file", (fun () ->
    let dune o =
      Printf.fprintf o "(tests" ;
      Printf.fprintf o "  (names a_test b_test)" ;
      Printf.fprintf o "  (libraries lib))" ;
      close_out o
    in

    let expected = let open Sexp in
      List [
        List [Atom "tests" ;
          List [Atom "names"; Atom "a_test"; Atom "b_test"] ;
          List [Atom "libraries"; Atom "lib"] ;
        ]
      ]
    in

    let tmp_file = Filename.temp_file "" "" in
    Filesystem.write_file tmp_file dune ;

    let actual = Sexp.of_dune_file tmp_file in
    Sys.remove tmp_file ;

    if Sexp.compare expected actual <> 0 then
      Test.fail (Printf.sprintf "expected %s, got %s"
        (Sexp.to_string expected)
        (Sexp.to_string actual)
      )
  );

  "Sexp_of_dune_file raises on bad input", (fun () ->
    let bad_dunes = [
      (fun o ->
        Printf.fprintf o "(tests" ;
        close_out o
      );
      (fun o ->
        Printf.fprintf o "tests)" ;
        close_out o
      );
    ] in

    List.iteri (fun i bad_dune ->
      let tmp_file = Filename.temp_file "" "" in
      Filesystem.write_file tmp_file bad_dune ;

      let raised_exception =
        try
          ignore (Sexp.of_dune_file tmp_file); false
        with Sexp.ParseError _ -> true
      in

      Sys.remove tmp_file ;
      if not raised_exception then
        Test.fail (Printf.sprintf "[%i] expected exception, did not raise" i)
    ) bad_dunes
  );
]

let () = Test.run tests
