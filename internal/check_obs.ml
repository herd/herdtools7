(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** A tool to check presence of Observation fields in an output file *)

let verbose = ref false
let fnames = ref []

let usage = String.concat "\n" [
    Printf.sprintf
      "Usage: %s [options] [fnames]*" (Filename.basename Sys.argv.(0)) ;
    "" ;
    "Check that the input contains one \"Observation tname...\" line";
    " per test name listed in files [fnames]*" ;
    "" ;
  "Options:" ;
]

let opts = [Args.verbose verbose;]

let anon s =
  if not (Args.test_file s) then begin
    raise
      (Arg.Bad (Printf.sprintf "Invalid filenameargument %s" s))
  end ;
  fnames := s :: !fnames

let () = Arg.parse opts anon usage

let verbose = !verbose
and fnames = !fnames

let exit_with_error msg =
  Printf.printf "%s: %s.\n" Sys.argv.(0) msg ;
  Arg.usage opts usage ;
  exit 2


let names =
  try
    List.map
      (fun fname ->
         Misc.input_protect
           (Channel.fold_lines
              (fun k line -> StringSet.add line k)
              StringSet.empty)
           fname) fnames
    |> StringSet.unions
  with Sys_error msg -> exit_with_error msg

let observed =
  Channel.fold_lines
    (fun k line ->
       if verbose && TestHerd.check_tags line then print_endline line ;
       let ws =
         try String.split_on_char ' ' line
         with Not_found -> [line] in
       match ws with
       | "Observation"::name::_ -> StringSet.add name k
       | _ -> k)
    StringSet.empty
    stdin

let not_observed =
  let set = StringSet.diff names observed in
  not (StringSet.is_empty set) &&
  (Printf.eprintf
     "Test%s not observed: %a\n%!"
     (if StringSet.is_singleton set then "" else "s")
     (fun chan -> StringSet.pp chan "," output_string) set;
   true)

let () = if not_observed then exit 1
