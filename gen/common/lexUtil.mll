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
(* if there is a relexation before,
   the blank will be treated as comma, hence sequence *)
let has_previous_relaxation = ref [false]
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
let predicate = ['A'-'Z' 'a'-'z' '0'-'9' '_']+

rule token = parse
| eof { EOF }
(* - operands consumes necessary blanks
   - `[....]` create a seperate scope
     hence a separate `has_previous_relaxation`. *)
| '[' blank* { push false has_previous_relaxation; LEFT_SQUIRE }
| blank* ']' {
  ignore (pop has_previous_relaxation);
  modify true has_previous_relaxation;
  RIGHT_SQUIRE
}
| blank* '?' { OPTION }
| blank* ',' blank* { COMMA }
| blank* '|' blank* { CHOICE_BAR }
| (relexation as lxm) {
  modify true has_previous_relaxation;
  RELAXATION lxm
}
| '@' (predicate as pred) blank* {
  PREDICATE pred
}
| '(' blank* {
  LEFT_BRACKET
}
| blank* ')' {
  RIGHT_BRACKET
}
| blank+ {
  if peak has_previous_relaxation then COMMA
  else token lexbuf
}
