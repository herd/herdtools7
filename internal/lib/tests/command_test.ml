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

module Option = Base.Option

module StringList = struct
  let compare = Base.List.compare String.compare
  let to_ocaml_string = Base.List.to_ocaml_string Base.String.to_ocaml_string
end

let tests = [
  "Command.command without args", (fun () ->
    let expected = "'foo'" in
    let actual = Command.command "foo" [] in
    if String.compare actual expected <> 0 then
      Test.fail (Printf.sprintf "Expected %s, got %s" expected actual)
  );
  "Command.command with args", (fun () ->
    let expected = "'foo' '-bar' 'baz'" in
    let actual = Command.command "foo" ["-bar"; "baz"] in
    if String.compare actual expected <> 0 then
      Test.fail (Printf.sprintf "Expected %s, got %s" expected actual)
  );
  "Command.command with escaping args", (fun () ->
    let expected = "'/bin/do foo' '-a flag' '~/foo'\\''s files/'" in
    let actual = Command.command "/bin/do foo" ["-a flag"; "~/foo's files/"] in
    if String.compare actual expected <> 0 then
      Test.fail (Printf.sprintf "Expected %s, got %s" expected actual)
  );

  "Command.run runs cleanly", (fun () ->
    (* This test uses `touch`, because it is a command that produces a
     * side-effect, and so can be verified to have run. *)

    (* Create a random file path, by creating a random file and deleting it. *)
    let path = Filename.temp_file "" "" in
    Sys.remove path ;

    (* Recreate it with `touch`. *)
    Command.run "touch" [path] ;

    if not (Sys.file_exists path) then
      Test.fail "File doesn't exist after `touch`"
    else
      (* Cleanup. *)
      Sys.remove path
  );

  "Command.run ~stdout", (fun () ->
    let tests = [
      ("true", [], []);
      ("echo", ["foo"], ["foo"]);
      ("echo", ["foo\nbar"], ["foo"; "bar"]);
    ] in

    List.iter
      (fun (bin, args, expected) ->
        let actual = ref None in
        let read_lines i = actual := Some (Channel.read_lines i) in
        Command.run ~stdout:read_lines bin args ;
        let actual = Option.get !actual in

        if StringList.compare actual expected <> 0 then
          Test.fail (Printf.sprintf "Expected %s, got %s"
            (StringList.to_ocaml_string expected)
            (StringList.to_ocaml_string actual)
          )
      )
      tests
  );
  "Command.run ~stdin ~stdout", (fun () ->
    let echo o =
      Printf.fprintf o "I am a line\n" ;
      Printf.fprintf o "so am I\n" ;
      close_out o
    in
    let expected = [
      "I am a line" ;
      "so am I" ;
    ] in

    let actual = ref None in
    let read_lines i = actual := Some (Channel.read_lines i) in
    Command.run ~stdin:echo ~stdout:read_lines "cat" [] ;
    let actual = Option.get !actual in

    if StringList.compare actual expected <> 0 then
      Test.fail (Printf.sprintf "expected %s, got %s"
        (StringList.to_ocaml_string expected)
        (StringList.to_ocaml_string actual)
      )
  );
  "Command.run ~stdout ~stderr", (fun () ->
    let bin = Lazy.force Shelf.python in
    let args = [
      "-c" ;
      "import sys; sys.stdout.write('mew\\n'); sys.stderr.write('purr\\n')" ;
    ] in

    let expected_stdout = [ "mew" ] in
    let expected_stderr = [ "purr" ] in

    let actual_stdout = ref None in
    let actual_stderr = ref None in
    let read r i = r := Some (Channel.read_lines i) in
    Command.run
      ~stdout:(read actual_stdout)
      ~stderr:(read actual_stderr)
      bin args ;
    let actual_stdout = Option.get !actual_stdout in
    let actual_stderr = Option.get !actual_stderr in

    if StringList.compare actual_stdout expected_stdout <> 0 then
      Test.fail (Printf.sprintf "stdout: expected %s, got %s"
        (StringList.to_ocaml_string expected_stdout)
        (StringList.to_ocaml_string actual_stdout)
      ) ;

    if StringList.compare actual_stderr expected_stderr <> 0 then
      Test.fail (Printf.sprintf "stderr: expected %s, got %s"
        (StringList.to_ocaml_string expected_stderr)
        (StringList.to_ocaml_string actual_stderr)
      )
  );
]

let () = Test.run tests
