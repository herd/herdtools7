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

{
open Parser
exception Eof
(* Track whether the current scope has already seen an operand.
   Whitespace after an operand is treated as a sequence separator. *)
let push t stack = stack := (t :: !stack)
let pop stack = match !stack with
      | [] -> Warn.fatal "error in has_previous_relaxation in lexer.\n"
      | hd :: tail -> stack := tail; hd
let peak stack = match !stack with
      | [] -> Warn.fatal "error in has_previous_relaxation in lexer.\n"
      | hd :: _ -> hd
let modify t stack = match !stack with
      | [] -> Warn.fatal "error in has_previous_relaxation in lexer.\n"
      | _ :: tail -> stack := t :: tail
}

let blank = [' ''\t''\n''\r']
let relexation = ['A'-'Z' 'a'-'z' '0'-'9' '-' '.' '*']+

(* The lexer consumes optional blanks around explicit syntax such as `[`, `]`,
   `,`, `|`, and `?`, because, for backward compatibility, standalone blanks
   are interpreted as `COMMA` after an concreate relax. *)
rule token has_previous_relaxation = parse
| eof { EOF }
| '[' blank* { push false has_previous_relaxation; LEFT_SQUARE }
| blank* ']' {
  ignore (pop has_previous_relaxation);
  modify true has_previous_relaxation;
  RIGHT_SQUARE
}
| blank* '?' { OPTION }
| blank* ',' blank* eof { EOF }
| blank* ',' blank* { COMMA }
| blank* '|' blank* { CHOICE_BAR }
| (relexation as lxm) {
  modify true has_previous_relaxation;
  RELAXATION lxm
}
| blank+ {
  if peak has_previous_relaxation then COMMA
  else token has_previous_relaxation lexbuf
}
{

(* The lexer keeps per-parse scope state to decide when whitespace should be
   treated as a separator. Keep that state local to each parse so nested or
   repeated parses do not interfere. *)
let parse parser lexbuf =
  let has_previous_relaxation = ref [false] in
  parser (token has_previous_relaxation) lexbuf

}
