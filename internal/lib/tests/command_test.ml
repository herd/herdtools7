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

(** Tests for the Command module. *)

let pp_string_list = Test.pp_string_list

let tests = [
  "Command.command without args", (fun () ->
    let expected = "'foo'" in
    let actual = Command.command "foo" [] in
    if String.compare actual expected <> 0 then
      failwith (Printf.sprintf "Expected %s, got %s" expected actual)
  );
  "Command.command with args", (fun () ->
    let expected = "'foo' '-bar' 'baz'" in
    let actual = Command.command "foo" ["-bar"; "baz"] in
    if String.compare actual expected <> 0 then
      failwith (Printf.sprintf "Expected %s, got %s" expected actual)
  );
  "Command.command with escaping args", (fun () ->
    let expected = "'/bin/do foo' '-a flag' '~/foo'\\''s files/'" in
    let actual = Command.command "/bin/do foo" ["-a flag"; "~/foo's files/"] in
    if String.compare actual expected <> 0 then
      failwith (Printf.sprintf "Expected %s, got %s" expected actual)
  );

  "Command.run_with_stdout captures stdout", (fun () ->
    let tests = [
      ("true", [], []);
      ("echo", ["foo"], ["foo"]);
      ("echo", ["foo\nbar"], ["foo"; "bar"]);
    ] in

    List.iter
      (fun (cmd, args, expected) ->
        let actual = Command.run_with_stdout cmd args Channel.read_lines in
        if Test.string_list_compare actual expected <> 0 then
          failwith (Printf.sprintf "Expected %s, got %s" (pp_string_list expected) (pp_string_list actual)))
      tests
  );
  "Command.run_with_stdout_lines raises on error", (fun () ->
    (* Deliberately fail. *)
    let raised_exception = try
      let _ = Command.run_with_stdout "false" [] (fun _ -> ()) in false
    with
      Command.Error _ -> true
    in

    if not raised_exception then
      failwith "Expected exception, did not raise";
  );
]

let () = Test.run tests
