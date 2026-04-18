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

(* The stack records, for each scope, whether the lexer has consumed a
   relaxation string. Scopes are either top-level or inside square brackets.
   This is because whitespace after a relaxation in the same scope is
   treated as a sequence separator. *)
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
let is_singleton stack = List.length !stack = 1
}

let blank = [' ''\t''\n''\r']
let relexation = ['A'-'Z' 'a'-'z' '0'-'9' '-' '.' '*']+

(* The lexer consumes optional blanks around explicit syntax such as `[`, `]`,
   `,`, `|`, and `?`, because, for backward compatibility, standalone blanks
   are interpreted as `COMMA` after an concreate relax. *)
rule token is_backward_compatible has_previous_relaxation = parse
| eof { EOF }
| '[' blank* { push false has_previous_relaxation; LEFT_SQUARE }
| blank* ']' {
  ignore (pop has_previous_relaxation);
  modify true has_previous_relaxation;
  RIGHT_SQUARE
}
| blank* '?' { OPTION }
| blank* ',' blank* eof { EOF }
| blank* ',' blank* {
  if is_backward_compatible && is_singleton has_previous_relaxation
  then CHOICE_BAR else COMMA
}
| blank* '|' blank* { CHOICE_BAR }
| (relexation as lxm) {
  modify true has_previous_relaxation;
  RELAXATION lxm
}
| blank+ {
  if peak has_previous_relaxation then begin
    if is_backward_compatible && is_singleton has_previous_relaxation
    then CHOICE_BAR else COMMA
  end
  else token is_backward_compatible has_previous_relaxation lexbuf
}
{

(* The lexer keeps per-parse scope state to decide when whitespace should be
   treated as a separator. Keep that state local to each parse so nested or
   repeated parses do not interfere. *)
let parse ?(is_backward_compatible=true) parser lexbuf =
  let has_previous_relaxation = ref [false] in
  parser (token is_backward_compatible has_previous_relaxation) lexbuf
  |> Ast.normalise

}
