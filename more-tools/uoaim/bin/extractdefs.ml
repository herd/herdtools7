(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Extract definitions *)

let prog =
  if Array.length Sys.argv > 0 then Filename.basename Sys.argv.(0)
  else "extractdefs7"

let parse_options () =
  let outdir = ref None in
  let arg = ref None in
  let options =
    [
      ( "-version",
        Arg.Unit
          (fun () ->
            Printf.printf "%s, Rev: %s\n" Version.version Version.rev;
            exit 0),
        " show version number and exit" );
      ( "-o",
        Arg.String (fun s -> outdir := Some s),
        "<dir> output definitions in directory <dir>" );
    ]
  in
  Arg.parse options
    (fun s -> arg := Some s)
    (Printf.sprintf "Usage: %s [option] [file]" prog);
  (!outdir, !arg)

module Regex = struct
  open Re

  let hspace = set " \t"
  let whitespace = rep (set " \t\n")
  let not_nl = diff any (char '\n')
  let not_delim = diff any (set ".:")

  let phrase =
    seq
      [
        str "if ";
        opt (str "and only if ");
        alt [ str "one"; str "any"; str "all" ];
        str " of the following ";
        alt [ str "applies"; str "apply" ];
        char ':';
      ]

  let header_line = seq [ bol; rg 'A' 'Z'; rep not_nl; phrase; whitespace ]
  let bullet_marker = alt [ char 'o'; rep1 (char '-'); str "then" ]

  let bullet_line =
    seq
      [
        bol;
        rep hspace;
        bullet_marker;
        rep1 hspace;
        rep not_delim;
        alt [ char '.'; char ':' ];
        whitespace;
      ]

  let paragraph = seq [ header_line; rep1 bullet_line ]
end

let extract_paragraphs contents =
  Re.all (Re.compile Regex.paragraph) contents
  |> List.map (fun re_match -> Re.Group.get re_match 0)

let output_paragraphs ~outdir paragraphs =
  match outdir with
  | None ->
      paragraphs
      |> List.iteri (fun ix paragraph ->
          if ix > 0 then output_char stdout '\n';
          output_string stdout paragraph;
          output_char stdout '\n')
  | Some dir ->
      paragraphs
      |> List.iteri (fun ix paragraph ->
          let fname = Filename.concat dir (Printf.sprintf "def%02d.txt" ix) in
          Misc.output_protect (fun chan -> output_string chan paragraph) fname)

let run ~outdir (ch : in_channel) =
  let contents = Uoaim.Util.read_all ch in
  let paragraphs = extract_paragraphs contents in
  output_paragraphs ~outdir paragraphs

let () =
  let outdir, arg = parse_options () in
  match arg with
  | None -> run ~outdir stdin
  | Some name -> Misc.input_protect (run ~outdir) name
