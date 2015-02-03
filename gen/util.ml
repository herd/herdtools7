(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf


type spec = string * Arg.spec * string

let parse_tag opt set tags msg =
  opt,
  Arg.String
    (fun tag -> match set tag with
    | false ->
        raise
          (Arg.Bad
             (sprintf "bad tags for %s, allowed tag are %s"
                opt (String.concat "," tags)))
    | true -> ()),
  sprintf "<%s> %s" (String.concat "|" tags) msg

let arch_opt arch =
  let d = !arch in
   parse_tag
    "-arch"
    (fun tag -> match Archs.parse tag with
    | None -> false
    | Some a -> arch := a ; true)
    Archs.tags (sprintf "specify architecture, default %s" (Archs.pp d))

let parse_cmdline options get_cmd_arg =
  Arg.parse options
    get_cmd_arg
    (sprintf "Usage %s [options] [arg]*\noptions are:" Sys.argv.(0))

