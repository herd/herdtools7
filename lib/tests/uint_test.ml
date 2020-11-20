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

  (* Tests for Uint128. *)

  "Uint.Uint128.leading_zeros", (fun () ->
    let open Uint in
    let tests = [
      "0b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 0 ;
      "0b00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 2 ;
      "0b00000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000", 71 ;
      "0b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 128 ;
    ] in

    List.iter (fun (bits, expected) ->
      let got = Uint128.leading_zeros (Uint128.of_string bits) in
      if got <> expected then
        Test.fail (Printf.sprintf "%s: expected %i, got %i"
          bits
          expected
          got)
    ) tests
  );

  "Uint.Uint128.shift_left", (fun () ->
    let open Uint in
    let tests = [
      "0x0", 3, "0x0" ;
      "0xF", 3, "0x78" ;
      "0x123456789ABCDEF", 12, "0x123456789ABCDEF000" ;
      "0xF", 0, "0xF" ;
      "0x123456789ABCDEF", 0, "0x123456789ABCDEF" ;
    ] in

    List.iter (fun (bits, shift, expected_bits) ->
      let got = Uint128.shift_left (Uint128.of_string bits) shift in
      let expected = Uint128.of_string expected_bits in
      if Uint128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          bits
          (Uint128.to_string_hex expected)
          (Uint128.to_string_hex got))
    ) tests
  );

  "Uint.Uint128.shift_right_logical", (fun () ->
    let open Uint in
    let tests = [
      "0x0", 3, "0x0" ;
      "0x78", 3, "0xF" ;
      "0x123456789ABCDEF000", 12, "0x123456789ABCDEF" ;
    ] in

    List.iter (fun (bits, shift, expected_bits) ->
      let got = Uint128.shift_right_logical (Uint128.of_string bits) shift in
      let expected = Uint128.of_string expected_bits in
      if Uint128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          bits
          (Uint128.to_string_hex expected)
          (Uint128.to_string_hex got))
    ) tests
  );

  "Uint.Uint128.add", (fun () ->
    let open Uint in
    let tests = [
      Uint128.zero, Uint128.zero, Uint128.zero ;
      Uint128.zero, Uint128.one, Uint128.one ;
      Uint128.one, Uint128.zero, Uint128.one ;
      Uint128.max_int, Uint128.one, Uint128.zero ;
      Uint128.of_string "0xFFFFFFFFFFFFFFFF", Uint128.one, Uint128.of_string "0x10000000000000000" ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint128.add a b in
      if Uint128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Uint128.to_string_hex a)
          (Uint128.to_string_hex b)
          (Uint128.to_string_hex expected)
          (Uint128.to_string_hex got))
    ) tests
  );

  "Uint.Uint128.sub", (fun () ->
    let open Uint in
    let tests = [
      Uint128.zero, Uint128.zero, Uint128.zero ;
      Uint128.zero, Uint128.one, Uint128.max_int ;
      Uint128.one, Uint128.zero, Uint128.one ;
      Uint128.of_string "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Uint128.of_string "0x123456789ABDCDEF", Uint128.of_string "0x7fffffffffffffffedcba98765423210" ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint128.sub a b in
      if Uint128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Uint128.to_string_hex a)
          (Uint128.to_string_hex b)
          (Uint128.to_string_hex expected)
          (Uint128.to_string_hex got))
    ) tests
  );

  "Uint.Uint128.div", (fun () ->
    let open Uint in
    let tests = [
      Uint128.one, Uint128.one, Uint128.one ;
      Uint128.of_int 15, Uint128.of_int 10, Uint128.of_int 1 ;
      Uint128.of_int 123456789, Uint128.max_int, Uint128.zero ;
      Uint128.of_int 123456789, Uint128.of_int 847, Uint128.of_int 145757 ;
      Uint128.of_string "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Uint128.of_string "0xAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Uint128.zero ;
      Uint128.of_string "0xAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Uint128.of_string "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Uint128.one ;
      Uint128.max_int, Uint128.max_int, Uint128.one ;
      Uint128.of_string "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Uint128.of_string "0x123456789ABCEDF", Uint128.of_string "0x707fffffffffa3b770" ;
      Uint128.max_int, Uint128.of_int 2, Uint128.of_string "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" ;
      Uint128.max_int, Uint128.of_int 3, Uint128.of_string "0x55555555555555555555555555555555" ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint128.div a b in
      if Uint128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Uint128.to_string_hex a)
          (Uint128.to_string_hex b)
          (Uint128.to_string_hex expected)
          (Uint128.to_string_hex got))
    ) tests
  );

  "Uint.Uint128.rem", (fun () ->
    let open Uint in
    let tests = [
      Uint128.one, Uint128.one, Uint128.zero ;
      Uint128.of_int 123456789, Uint128.of_int 847, Uint128.of_int 610 ;
      Uint128.of_int 123456789, Uint128.max_int, Uint128.of_int 123456789 ;
      Uint128.max_int, Uint128.max_int, Uint128.zero ;
      Uint128.one, Uint128.max_int, Uint128.one ;
      Uint128.max_int, Uint128.of_string "0x3", Uint128.zero ;
      Uint128.max_int, Uint128.of_string "0x2", Uint128.one ;
      Uint128.of_int 15, Uint128.of_int 10, Uint128.of_int 5 ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint128.rem a b in
      if Uint128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Uint128.to_string_hex a)
          (Uint128.to_string_hex b)
          (Uint128.to_string_hex expected)
          (Uint128.to_string_hex got))
    ) tests
  );

  "Uint.Uint128.compare", (fun () ->
    let open Uint in
    let tests = [
      Uint128.zero, Uint128.zero, 0 ;
      Uint128.zero, Uint128.one, -1 ;
      Uint128.one, Uint128.zero, 1 ;
      Uint128.max_int, Uint128.one, 1 ;
      Uint128.max_int, Uint128.zero, 1 ;
      Uint128.max_int, Uint128.of_string "18446744073709551613", 1 ;
      Uint128.max_int, Uint128.of_string "0xFFFFFFFFFFFFFFAA", 1 ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Uint128.compare a b in
      if got <> expected then
        Test.fail (Printf.sprintf "%s %s: expected %i, got %i"
          (Uint128.to_string_hex a)
          (Uint128.to_string_hex b)
          expected
          got)
    ) tests
  );

  "Uint.Uint128.to_int", (fun () ->
    let open Uint in
    let tests = [
      Uint128.zero, 0 ;
      Uint128.one, 1 ;
      Uint128.of_string "0x75BCD15", 123456789 ;
      Uint128.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", -1 ;
      Uint128.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFF8A432EB", -123456789 ;
      Uint128.of_string "0x0FFFFFFFFFFFFFFFFFFFFFFFF8A432EB", -123456789 ;
    ] in

    List.iter (fun (a, expected) ->
      let got = Uint128.to_int a in
      if got <> expected then
        Test.fail (Printf.sprintf "%s: expected %i, got %i"
          (Uint128.to_string_hex a)
          expected
          got)
    ) tests
  );

  "Uint.Uint128.of_int", (fun () ->
    let open Uint in
    let tests = [
      0, Uint128.of_string "0x0" ;
      1, Uint128.of_string "0x1" ;
      123456789, Uint128.of_string "0x75BCD15" ;
      -1, Uint128.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" ;
      -123456789, Uint128.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFF8A432EB" ;
    ] in

    List.iter (fun (i, expected) ->
      let got = Uint128.of_int i in
      if Uint128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%i: expected %s, got %s"
          i
          (Uint128.to_string_hex expected)
          (Uint128.to_string_hex got))
    ) tests
  );

  "Uint.Uint128.to_string", (fun () ->
    let open Uint in
    let tests = [
      Uint128.zero, "0" ;
      Uint128.one , "1" ;
      Uint128.of_int 10, "10" ;
      Uint128.of_int 15, "15" ;
      Uint128.of_string "0x8" , "8" ;
      Uint128.of_string "0xF" , "15" ;
      Uint128.of_string "0xFF" , "255" ;
      Uint128.of_string "0xFAFF" , "64255" ;
      Uint128.of_string "0xABCDFAFF" , "2882403071" ;
      Uint128.of_string "0x123456789ABCEDF" , "81985529216487135" ;
      Uint128.max_int, "340282366920938463463374607431768211455" ;
    ] in

    List.iter (fun (a, expected) ->
      let got = Uint128.to_string a in
      if String.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          (Uint128.to_string_hex a)
          expected
          got)
    ) tests
  );

  "Uint.Uint128.to_string_hex", (fun () ->
    let open Uint in
    let tests = [
      Uint128.zero, "0x0" ;
      Uint128.one , "0x1" ;
      Uint128.max_int, "0xffffffffffffffffffffffffffffffff" ;
      Uint128.of_string "0xfffafffbfffffffffffffffff", "0xfffafffbfffffffffffffffff" ;
      Uint128.of_string "0xfffafff0000000000ffffffff", "0xfffafff0000000000ffffffff" ;
    ] in

    List.iter (fun (a, expected) ->
      let got = Uint128.to_string_hex a in
      if String.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          (Uint128.to_string_hex a)
          expected
          got)
    ) tests
  );

  "Uint.Uint128.of_string", (fun () ->
    let open Uint in
    let tests = [
      "0", Uint128.zero ;
      "1", Uint128.one ;
      "123456789123456789", Uint128.of_int 123456789123456789 ;
      "123456789123456789123456789", Uint128.of_string "0x661efdf2e3b19f7c045f15" ;
      "12345", Uint128.of_int 12345 ;
      "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Uint128.max_int ;
      "340282366920938463463374607431768211455", Uint128.max_int ;
    ] in

    List.iter (fun (raw, expected) ->
      let got = Uint128.of_string raw in
      if Uint128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          raw
          (Uint128.to_string_hex expected)
          (Uint128.to_string_hex got))
    ) tests
  );

  "Uint.Uint128.of_string bad input", (fun () ->
    let open Uint in
    let tests = [
      "a" ;
      "-1" ;
      "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" ;
      (* "3402823669209384634633746074317682114551" ; *)
      "hello" ;
    ] in

    List.iter (fun raw ->
      let raised =
        try
          let _ = Uint128.of_string raw in false
        with _ -> true
      in
      if not raised then
        Test.fail (Printf.sprintf "%s: did not raise" raw)
    ) tests
  );
]

let () = Test.run tests
