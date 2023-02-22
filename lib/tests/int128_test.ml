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

(** Tests for Int128 module. *)
open! Int128
let tests = [
  (* Tests for Int128. *)

  "Int128.leading_zeros", (fun () ->
    let tests = [
      "0b10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 0 ;
      "0b00100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 2 ;
      "0b00000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000", 71 ;
      "0b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", 128 ;
    ] in

    List.iter (fun (bits, expected) ->
      let got = Int128.leading_zeros (Int128.of_string bits) in
      if got <> expected then
        Test.fail (Printf.sprintf "%s: expected %i, got %i"
          bits
          expected
          got)
    ) tests
  );

  "Int128.shift_left", (fun () ->
    let tests = [
      "0x0", 3, "0x0" ;
      "0xF", 3, "0x78" ;
      "0x123456789ABCDEF", 12, "0x123456789ABCDEF000" ;
      "0xF", 0, "0xF" ;
      "0x123456789ABCDEF", 0, "0x123456789ABCDEF" ;
    ] in

    List.iter (fun (bits, shift, expected_bits) ->
      let got = Int128.shift_left (Int128.of_string bits) shift in
      let expected = Int128.of_string expected_bits in
      if Int128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          bits
          (Int128.to_string_hex expected)
          (Int128.to_string_hex got))
    ) tests
  );

  "Int128.shift_right_logical", (fun () ->
    let tests = [
      "0x0", 3, "0x0" ;
      "0x78", 3, "0xF" ;
      "0x123456789ABCDEF000", 12, "0x123456789ABCDEF" ;
    ] in

    List.iter (fun (bits, shift, expected_bits) ->
      let got = Int128.shift_right_logical (Int128.of_string bits) shift in
      let expected = Int128.of_string expected_bits in
      if Int128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          bits
          (Int128.to_string_hex expected)
          (Int128.to_string_hex got))
    ) tests
  );

  "Int128.shift_right", (fun () ->
    let tests = [
      "0x0", 3, "0x0" ;
      "0x78", 3, "0xF" ;
      "0x123456789ABCDEF000", 12, "0x123456789ABCDEF" ;
      "0xA0000000000000123456789ABCDEF000", 12, "0xFFFA0000000000000123456789ABCDEF" ;
    ] in

    List.iter (fun (bits, shift, expected_bits) ->
      let got = Int128.shift_right (Int128.of_string bits) shift in
      let expected = Int128.of_string expected_bits in
      if Int128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          bits
          (Int128.to_string_hex expected)
          (Int128.to_string_hex got))
    ) tests
  );

  "Int128.add", (fun () ->
    let tests = [
      Int128.zero, Int128.zero, Int128.zero ;
      Int128.zero, Int128.one, Int128.one ;
      Int128.one, Int128.zero, Int128.one ;
      Int128.max_int, Int128.one, (Int128.shift_left Int128.one 127);
      Int128.of_string "0xFFFFFFFFFFFFFFFF", Int128.one, Int128.of_string "0x10000000000000000" ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Int128.add a b in
      if Int128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Int128.to_string_hex a)
          (Int128.to_string_hex b)
          (Int128.to_string_hex expected)
          (Int128.to_string_hex got))
    ) tests
  );

  "Int128.sub", (fun () ->
    let tests = [
      Int128.zero, Int128.zero, Int128.zero ;
      Int128.zero, Int128.one, Int128.minus_one ;
      Int128.one, Int128.zero, Int128.one ;
      Int128.of_string "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Int128.of_string "0x123456789ABDCDEF", Int128.of_string "0x7fffffffffffffffedcba98765423210" ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Int128.sub a b in
      if Int128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Int128.to_string_hex a)
          (Int128.to_string_hex b)
          (Int128.to_string_hex expected)
          (Int128.to_string_hex got))
    ) tests
  );

  "Int128.mul", (fun () ->
    let tests = [
      Int128.one, Int128.one, Int128.one ;
      Int128.of_int64 15L, Int128.of_int64 10L, Int128.of_int64 150L ;
      Int128.of_int64 123456789L, Int128.zero, Int128.zero ;
      Int128.of_int64 123456789L, Int128.of_int64 847L, Int128.of_int64 0x1858bb887bL ;
      Int128.of_string "0x7FFFFFFFFFF000000000000FFFFFFFFF", Int128.of_string "0x123456789ABDCDEF123456789ABCEDF1", Int128.of_string "0xb21edca9a977999a999a88976543120f" ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Int128.mul a b in
      if Int128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Int128.to_string_hex a)
          (Int128.to_string_hex b)
          (Int128.to_string_hex expected)
          (Int128.to_string_hex got))
    ) tests
  );

  "Int128.div", (fun () ->
    let tests = [
      Int128.one, Int128.one, Int128.one ;
      Int128.of_int64 15L, Int128.of_int64 10L, Int128.of_int64 1L ;
      Int128.of_int64 123456789L, Int128.max_int, Int128.zero ;
      Int128.of_int64 123456789L, Int128.of_int64 847L, Int128.of_int64 145757L ;
      Int128.of_string "0xAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Int128.of_string "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Int128.minus_one ;
      Int128.max_int, Int128.max_int, Int128.one ;
      Int128.of_string "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Int128.of_string "0x123456789ABCEDF", Int128.of_string "0x707fffffffffa3b770" ;
      Int128.max_int, Int128.of_int64 2L, Int128.of_string "0x3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" ;
      Int128.max_int, Int128.of_int64 3L, Int128.of_string "0x2aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" ;
      Int128.of_int64 (-11L), Int128.of_int64 2L, Int128.of_int64 (-6L) ;
      Int128.of_int64 (-11L), Int128.of_int64 3L, Int128.of_int64 (-4L) ;
    ] in

    List.iter (fun (a, b, expected) ->
     try
       let got = Int128.div a b in
       if Int128.compare got expected <> 0 then
         Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
           (Int128.to_string a)
           (Int128.to_string b)
           (Int128.to_string expected)
           (Int128.to_string got))
     with Failure msg ->
       Test.fail (Printf.sprintf "%s %s: expected %s, got Failure %s"
           (Int128.to_string_hex a)
           (Int128.to_string_hex b)
           (Int128.to_string_hex expected)
           msg)
    ) tests
  );

  "Int128.rem", (fun () ->
    let tests = [
      Int128.one, Int128.one, Int128.zero ;
      Int128.of_int64 123456789L, Int128.of_int64 847L, Int128.of_int64 610L ;
      Int128.of_int64 123456789L, Int128.max_int, Int128.of_int64 123456789L ;
      Int128.max_int, Int128.max_int, Int128.zero ;
      Int128.one, Int128.max_int, Int128.one ;
      Int128.max_int, Int128.of_string "0x3", Int128.one ;
      Int128.max_int, Int128.of_string "0x2", Int128.one ;
      Int128.of_int64 15L, Int128.of_int64 10L, Int128.of_int64 5L ;
      Int128.of_int64 (-11L), Int128.of_int64 2L, Int128.of_int64 1L ;
      Int128.of_int64 (-10L), Int128.of_int64 3L, Int128.of_int64 2L ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Int128.rem a b in
      if Int128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s, got %s"
          (Int128.to_string_hex a)
          (Int128.to_string_hex b)
          (Int128.to_string_hex expected)
          (Int128.to_string_hex got))
    ) tests
  );

  "Int128.compare", (fun () ->
    let tests = [
      Int128.zero, Int128.zero, 0 ;
      Int128.zero, Int128.one, -1 ;
      Int128.one, Int128.zero, 1 ;
      Int128.one,Int128.minus_one, 1;
      Int128.minus_one,Int128.one, -1;
      Int128.max_int, Int128.one, 1 ;
      Int128.max_int, Int128.zero, 1 ;
      Int128.max_int, Int128.of_string "18446744073709551613", 1 ;
      Int128.max_int, Int128.of_string "0xFFFFFFFFFFFFFFAA", 1 ;
    ] in

    List.iter (fun (a, b, expected) ->
      let got = Int128.compare a b in
      if got <> expected then
        Test.fail (Printf.sprintf "%s %s: expected %i, got %i"
          (Int128.to_string_hex a)
          (Int128.to_string_hex b)
          expected
          got)
    ) tests
  );

  "Int128.to_int", (fun () ->
    let tests = [
      Int128.zero, 0 ;
      Int128.one, 1 ;
      Int128.of_string "0x75BCD15", 123456789 ;
      Int128.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", -1 ;
      Int128.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFF8A432EB", -123456789 ;
      Int128.of_string "0x0FFFFFFFFFFFFFFFFFFFFFFFF8A432EB", -123456789 ;
    ] in

    List.iter (fun (a, expected) ->
      let got = Int128.to_int a in
      if got <> expected then
        Test.fail (Printf.sprintf "%s: expected %i, got %i"
          (Int128.to_string_hex a)
          expected
          got)
    ) tests
  );

  "Int128.of_int", (fun () ->
    let tests = [
      0L, Int128.of_string "0x0" ;
      1L, Int128.of_string "0x1" ;
      123456789L, Int128.of_string "0x75BCD15" ;
      -1L, Int128.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" ;
      -123456789L, Int128.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFF8A432EB" ;
    ] in

    List.iter (fun (i, expected) ->
      let got = Int128.of_int64 i in
      if Int128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%Li: expected %s, got %s"
          i
          (Int128.to_string_hex expected)
          (Int128.to_string_hex got))
    ) tests
  );

  "Int128.to_string", (fun () ->
    let tests = [
      Int128.zero, "0" ;
      Int128.one , "1" ;
      Int128.minus_one, "-1";
      Int128.of_int64 10L, "10" ;
      Int128.of_int64 15L, "15" ;
      Int128.of_string "0x8" , "8" ;
      Int128.of_string "0xF" , "15" ;
      Int128.of_string "0xFF" , "255" ;
      Int128.of_string "0xFAFF" , "64255" ;
      Int128.of_string "0xABCDFAFF" , "2882403071" ;
      Int128.of_string "0x123456789ABCEDF" , "81985529216487135" ;
      Int128.max_int, "170141183460469231731687303715884105727" ;
    ] in

    List.iter (fun (a, expected) ->
      let got = Int128.to_string a in
      if String.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          (Int128.to_string_hex a)
          expected
          got)
    ) tests
  );

  "Int128.to_string_hex", (fun () ->
    let tests = [
      Int128.zero, "0x0" ;
      Int128.one , "0x1" ;
      Int128.max_int, "0x7fffffffffffffffffffffffffffffff" ;
      Int128.of_string "0xfffafffbfffffffffffffffff", "0xfffafffbfffffffffffffffff" ;
      Int128.of_string "0xfffafff0000000000ffffffff", "0xfffafff0000000000ffffffff" ;
    ] in

    List.iter (fun (a, expected) ->
      let got = Int128.to_string_hex a in
      if String.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          (Int128.to_string_hex a)
          expected
          got)
    ) tests
  );

  "Int128.of_string", (fun () ->
    let tests = [
      "0", Int128.zero ;
      "1", Int128.one ;
      "123456789123456789", Int128.of_int64 123456789123456789L ;
      "123456789123456789123456789", Int128.of_string "0x661efdf2e3b19f7c045f15" ;
      "12345", Int128.of_int64 12345L ;
      "0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", Int128.max_int ;
      "170141183460469231731687303715884105727", Int128.max_int ;
    ] in

    List.iter (fun (raw, expected) ->
      let got = Int128.of_string raw in
      if Int128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          raw
          (Int128.to_string_hex expected)
          (Int128.to_string_hex got))
    ) tests
  );

  "Int128.of_string bad input", (fun () ->
    let tests = [
      "a" ;
      "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" ;
      (* "3402823669209384634633746074317682114551" ; *)
      "hello" ;
    ] in

    List.iter (fun raw ->
      let raised =
        try
          let _ = Int128.of_string raw in false
        with _ -> true
      in
      if not raised then
        Test.fail (Printf.sprintf "%s: did not raise" raw)
    ) tests
  );

  "Int128.is_neg", (fun () ->
    let tests = [
      Int128.one, false;
      Int128.zero, false;
      Int128.max_int, false;
      Int128.minus_one, true;
      Int128.add Int128.minus_one Int128.zero, true;
      Int128.sub Int128.minus_one Int128.one, true;
    ] in

    List.iter (fun (raw, expected) ->
      let got = Int128.is_neg raw in
      if got <> expected then
        Test.fail (Printf.sprintf "%s: expected %B, got %B"
          (Int128.to_string raw)
          expected
          got)
    ) tests
  );

  "Int128.chsgn", (fun () ->
    let tests = [
      Int128.one, Int128.minus_one;
      Int128.minus_one, Int128.one;
      Int128.zero, Int128.zero;
      Int128.max_int, Int128.of_string "-0x7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF";
      Int128.add Int128.minus_one Int128.zero, Int128.one;
    ] in

    List.iter (fun (raw, expected) ->
      let got = Int128.chsgn raw in
      if Int128.compare got expected <> 0 then
        Test.fail (Printf.sprintf "%s: expected %s, got %s"
          (Int128.to_string raw)
          (Int128.to_string expected)
          (Int128.to_string got))
    ) tests
  );

  "Int128.div_and_rem", (fun () ->
    let tests = [
      (Int128.one, Int128.one), (Int128.one, Int128.zero);
      (Int128.of_string "0x2", Int128.one), (Int128.of_string "0x2", Int128.zero);
    ] in

    List.iter (fun (raw, expected) ->
      let got = Int128.div_and_rem (fst raw) (snd raw) in
      if Int128.compare (fst got) (fst expected) <> 0 ||
         Int128.compare (snd got) (snd expected) <> 0 then
        Test.fail (Printf.sprintf "%s %s: expected %s %s, got %s %s"
          (Int128.to_string (fst raw))
          (Int128.to_string (snd raw))
          (Int128.to_string (fst expected))
          (Int128.to_string (snd expected))
          (Int128.to_string (fst got))
          (Int128.to_string (snd got)))
    ) tests
  );

  "Int128.div_and_rem bad input", (fun () ->
    let tests = [
      (Int128.one, Int128.zero);
    ] in

    List.iter (fun raw ->
      let raised =
        try
          let _ = Int128.div_and_rem (fst raw) (snd raw) in false
        with _ -> true
      in
      if not raised then
        Test.fail (Printf.sprintf "%s %s: did not raise"
          (Int128.to_string (fst raw))
          (Int128.to_string (snd raw)))
    ) tests
  );
]

let () = Test.run tests
