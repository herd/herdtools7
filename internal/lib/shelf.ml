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

(** An OCaml representation of a shelf.py file. *)

module StringList = struct
  let compare = Base.List.compare String.compare
  let to_ocaml_string = Base.List.to_ocaml_string Base.String.to_ocaml_string
end
module StringListOption = struct
  let compare = Base.Option.compare StringList.compare
  let to_ocaml_string = Base.Option.to_ocaml_string StringList.to_ocaml_string
end

exception ParseError of string

type t = {
  record : string ;

  cats : string list ;
  configs : string list ;
  tests : string list ;

  bells : string list option ;
  compatibilities : string list option ;
}

let compare a b = Compare.chain [
  String.compare a.record b.record ;
  StringList.compare a.cats b.cats ;
  StringList.compare a.configs b.configs ;
  StringList.compare a.tests b.tests ;
  StringListOption.compare a.bells b.bells ;
  StringListOption.compare a.compatibilities b.compatibilities ;
]

let to_ocaml_string shelf = OcamlString.record [
  "record",              Base.String.to_ocaml_string       shelf.record ;
  "cats",                StringList.to_ocaml_string        shelf.cats ;
  "configs",             StringList.to_ocaml_string        shelf.configs ;
  "tests",               StringList.to_ocaml_string        shelf.tests ;
  "bells",               StringListOption.to_ocaml_string  shelf.bells ;
  "compatibilities",     StringListOption.to_ocaml_string  shelf.compatibilities ;
]

let python = lazy
  begin
    let exists p =
      let dev_null ch = ignore (Channel.read_lines ch) in
      try
        Command.run ~stdout:dev_null ~stderr:dev_null p ["--version"] ;
        true
      with
        | Unix.Unix_error _ -> false
        | Command.Error _ -> false
    in
    match List.find_opt exists ["python"; "python3"] with
    | Some p -> p
    | None -> failwith "Could not find either python or python3"
  end

let do_list_of_file sorted path key =
  (* Shelf files are executable Python code, so this Python script imports the
   * shelf.py file, then prints the given global variables. *)
  let script chan =
    Printf.fprintf chan "import os\n" ;
    Printf.fprintf chan "import sys\n" ;
    Printf.fprintf chan "os.chdir(%s)\n" (Filename.quote (Filename.dirname path)) ;
    Printf.fprintf chan "m = {}\n" ;
    Printf.fprintf chan "if sys.version_info >= (3, 0):\n" ;
    Printf.fprintf chan "  exec(open(%s).read(), m)\n" (Filename.quote (Filename.basename path)) ;
    Printf.fprintf chan "else:\n" ;
    Printf.fprintf chan "  execfile(%s, m)\n" (Filename.quote (Filename.basename path)) ;
    Printf.fprintf chan "try:\n" ;
    Printf.fprintf chan "  v = m[%s]\n" (Filename.quote key) ;
    Printf.fprintf chan "  if isinstance(v, str):\n" ;
    Printf.fprintf chan "    print(v)\n" ;
    Printf.fprintf chan "  else:\n" ;
    Printf.fprintf chan "    for x in v:\n" ;
    Printf.fprintf chan "      print(x)\n" ;
    Printf.fprintf chan "except:\n" ;
    Printf.fprintf chan "  pass\n" ;
    close_out chan
  in
  let lines = ref [] in
  let read_lines c = lines := Channel.read_lines c in
  begin try
    Command.run ~stdin:script ~stdout:read_lines (Lazy.force python) []
  with
    Command.Error e -> failwith (Command.string_of_error e)
  end ;
  if sorted then
    List.sort String.compare !lines
  else
    !lines

let list_of_file = do_list_of_file false
and list_of_file_sorted = do_list_of_file true

let string_of_file path key =
  match list_of_file path key with
  | x :: [] -> x
  | [] -> raise (ParseError (Printf.sprintf "Missing value for key %s" key))
  | xs -> raise (ParseError (Printf.sprintf "Expected single value for key %s, found %i" key (List.length xs)))

let optional_list_of_file path key =
  match list_of_file path key with
  | [] -> None
  | xs -> Some xs

let of_file path =
  {
    record = string_of_file path "record" ;

    cats               = list_of_file path "cats" ;
    configs            = list_of_file path "cfgs" ;
    tests =
      (match list_of_file_sorted path "illustrative_tests" with
      | [] -> list_of_file_sorted path "tests"
      | xs -> xs);
    bells           = optional_list_of_file path "bells" ;
    compatibilities = optional_list_of_file path "compatibilities" ;
  }
