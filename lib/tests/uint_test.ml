(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Tests for Uint module. *)

let tests = [
  (* Tests for Uint64. *)

  "Uint.Uint64.leading_zeros", (fun () ->
    let open Uint in
    let tests = [
      "0b1000000000000000000000000000000000000000000000000000000000000000", 0 ;
      "0b0010000000000000000000000000000000000000000000000000000000000000", 2 ;
      "0b0000000100000000000000000000000000000000000000000000000000000000", 7 ;
      "0b0000000000000000000000000000000000000000000000000000000000000000", 64 ;
    ] in

    List.iter (fun (bits, expected) ->
      let got = Uint64.leading_zeros (Uint64.of_string bits) in
      if got <> expected then
        Test.fail (Printf.sprintf "%s: expected %i, got %i"
          bits
          expected
          got)
    ) tests
  );

  "Uint.Uint64.add", (fun () ->
    let open Uint in
    let tests = [
      Uint64.zero, Uint64.zero, Uint64.zero ;
      Uint64.zero, Uint64.one, Uint64.one ;
      Uint64.one, Uint64.zero, Uint64.one ;
      Uint64.max_int, Uint64.one, Uint64.zero ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint64.add a b in
      if Uint64.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Uint64.to_string a)
          (Uint64.to_string b)
          (Uint64.to_string expected)
          (Uint64.to_string got))
    ) tests
  );

  "Uint.Uint64.sub", (fun () ->
    let open Uint in
    let tests = [
      Uint64.zero, Uint64.zero, Uint64.zero ;
      Uint64.zero, Uint64.one, Uint64.max_int ;
      Uint64.one, Uint64.zero, Uint64.one ;
      Uint64.max_int, Uint64.one, Uint64.of_string "0xFFFFFFFFFFFFFFFE" ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint64.sub a b in
      if Uint64.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Uint64.to_string a)
          (Uint64.to_string b)
          (Uint64.to_string expected)
          (Uint64.to_string got))
    ) tests
  );

  "Uint.Uint64.mul", (fun () ->
    let open Uint in
    let tests = [
      Uint64.one, Uint64.one, Uint64.one ;
      Uint64.one, Uint64.zero, Uint64.zero ;
      Uint64.of_int 123456789, Uint64.of_int 847, Uint64.of_int 104567900283 ;
      Uint64.of_int 123456789, Uint64.of_int 847, Uint64.of_int 104567900283 ;
      Uint64.of_int 3, Uint64.of_string "6148914691236517205", Uint64.max_int ;
      Uint64.of_int 2, Uint64.of_string "9223372036854775807", Uint64.sub Uint64.max_int Uint64.one ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint64.mul a b in
      if Uint64.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Uint64.to_string a)
          (Uint64.to_string b)
          (Uint64.to_string expected)
          (Uint64.to_string got))
    ) tests
  );

  "Uint.Uint64.div", (fun () ->
    let open Uint in
    let tests = [
      Uint64.one, Uint64.one, Uint64.one ;
      Uint64.of_int 123456789, Uint64.of_int 847, Uint64.of_int 145757 ;
      Uint64.of_int 123456789, Uint64.max_int, Uint64.zero ;
      Uint64.max_int, Uint64.max_int, Uint64.one ;
      Uint64.max_int, Uint64.of_int 3, Uint64.of_string "6148914691236517205" ;
      Uint64.max_int, Uint64.of_int 2, Uint64.of_string "9223372036854775807" ;
      Uint64.of_int 0x123456789ABCEDF, Uint64.of_int 10 , Uint64.of_int 8198552921648713 ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint64.div a b in
      if Uint64.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Uint64.to_string a)
          (Uint64.to_string b)
          (Uint64.to_string expected)
          (Uint64.to_string got))
    ) tests
  );

  "Uint.Uint64.rem", (fun () ->
    let open Uint in
    let tests = [
      Uint64.one, Uint64.one, Uint64.zero ;
      Uint64.of_int 123456789, Uint64.of_int 847, Uint64.of_int 610 ;
      Uint64.of_int 123456789, Uint64.max_int, Uint64.of_int 123456789 ;
      Uint64.max_int, Uint64.max_int, Uint64.zero ;
      Uint64.max_int, Uint64.of_int 3, Uint64.zero ;
      Uint64.max_int, Uint64.of_int 2, Uint64.of_int 1 ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint64.rem a b in
      if Uint64.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Uint64.to_string a)
          (Uint64.to_string b)
          (Uint64.to_string expected)
          (Uint64.to_string got))
    ) tests
  );

  "Uint.Uint64.to_string", (fun () ->
    let open Uint in
    let tests = [
      Uint64.zero, "0" ;
      Uint64.one , "1" ;
      Uint64.max_int, "18446744073709551615" ;
    ] in

    List.iter (fun (a, expected) ->
      let got = Uint64.to_string a in
      if String.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          (Uint64.to_string_hex a)
          expected
          got)
    ) tests
  );

  "Uint.Uint64.to_string_hex", (fun () ->
    let open Uint in
    let tests = [
      Uint64.zero, "0x0" ;
      Uint64.one , "0x1" ;
      Uint64.max_int, "0xffffffffffffffff" ;
    ] in

    List.iter (fun (a, expected) ->
      let got = Uint64.to_string_hex a in
      if String.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          (Uint64.to_string a)
          expected
          got)
    ) tests
  );

  "Uint.Uint64.of_string", (fun () ->
    let open Uint in
    let tests = [
      "0", Uint64.zero ;
      "1", Uint64.one ;
      "0xFFFFFFFFFFFFFFFF", Uint64.max_int ;
      "18446744073709551615", Uint64.max_int ;
    ] in

    List.iter (fun (raw, expected) ->
      let got = Uint64.of_string raw in
      if Uint64.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          raw
          (Uint64.to_string expected)
          (Uint64.to_string got))
    ) tests
  );

  "Uint.Uint64.of_string bad input", (fun () ->
    let open Uint in
    let tests = [
      "a" ;
      "-1" ;
      "0xFFFFFFFFFFFFFFFFF" ;
      "184467440737095516158" ;
    ] in

    List.iter (fun raw ->
      let raised =
        try
          let _ = Uint64.of_string raw in false
        with _ -> true
      in
      if not raised then
        Test.fail (Printf.sprintf "%s: did not raise" raw)
    ) tests
  );

  "Uint.Uint64.compare", (fun () ->
    let open Uint in
    let tests = [
      Uint64.zero, Uint64.zero, 0 ;
      Uint64.zero, Uint64.one, -1 ;
      Uint64.one, Uint64.zero, 1 ;
      Uint64.max_int, Uint64.one, 1 ;
      Uint64.max_int, Uint64.zero, 1 ;
      Uint64.max_int, Uint64.of_string "18446744073709551613", 1 ;
      Uint64.max_int, Uint64.of_string "0xFFFFFFFFFFFFFFAA", 1 ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint64.compare a b in
      if got <> expected then
        Test.fail (Printf.sprintf "%s %s: expected %i, got %i"
          (Uint64.to_string a)
          (Uint64.to_string b)
          expected
          got)
    ) tests
  );
]

let () = Test.run tests
