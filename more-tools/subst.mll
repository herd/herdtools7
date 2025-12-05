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

(** Substitute in strings *)

rule subst buff args = parse
| '{' (['0'-'9']+ as n) '}'
  {
    let n = int_of_string n in
    Buffer.add_string buff args.(n) ;
    subst buff args lexbuf
  }
| _ as c
  {
    Buffer.add_char buff c ;
    subst buff args lexbuf
  }
| eof { Buffer.contents buff }

and reverse = parse
| '{' (['0''1'] as id) '}' { id::reverse lexbuf }
| _     { reverse lexbuf }
| eof   { [] }

and event = parse
| 'E' ['0'-'9']+ eof { true }
| "" { false }

{
  let is_event s = event (Lexing.from_string s)

  let rec uniq rs = function
  | [] -> []
  | c::cs ->
      if List.exists (Char.equal c) rs then uniq rs cs
      else c::uniq (c::rs) cs

  let is_reverse body =
    let ids =  Lexing.from_string body |> reverse in
    match uniq [] ids with
    | ['1'; '0'; ] -> true
    | ['0'; '1'; ] -> false
    | _ -> assert false

  let subst body args =
    subst (Buffer.create 16) args (Lexing.from_string body)
}
