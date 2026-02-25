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

open Angstrom

let word =
  let is_word_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | _ -> false
  in
  take_while1 is_word_char

let is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false
let whitespace = skip_while is_whitespace

let number =
  let* str = take_while1 (function '0' .. '9' -> true | _ -> false) in
  return (int_of_string str)

let token p = p <* whitespace

let remaining ~max =
  let* remaining = available in
  let remaining = min remaining max in
  if remaining = 0 then return "EOF" else peek_string remaining

let on_fail ~expected ?(max = 10) p =
  let q =
    let* rem = remaining ~max in
    fail (Format.sprintf "expected %s but got \"%s...\"" expected rem)
  in
  p <|> q

let word_except l =
  let* s = word in
  if List.mem s l then fail (Format.sprintf "unexpected word %S" s)
  else return s

let char c =
  let on_fail =
    let* rem = remaining ~max:1 in
    fail (Format.sprintf "expected character '%c' but got '%s'" c rem)
  in
  char c <|> on_fail

let dot = token (char '.') *> return ()
let colon = token (char ':')
let comma = token (char ',') *> return ()
let lparen = token (char '(')
let rparen = token (char ')')
let string s = on_fail ~expected:(Format.sprintf "string %S" s) (string s)
let string_ci s = on_fail ~expected:(Format.sprintf "string %S" s) (string_ci s)
let string_token s = token (string s)
let string_token_ci s = token (string_ci s)

let end_of_input =
  at_end_of_input >>= function
  | true -> return ()
  | false ->
      let* rem = remaining ~max:10 in
      fail (Format.sprintf "expected end of input but got \"%s...\"" rem)

let words s =
  let aux = String.split_on_char ' ' s in
  list (List.map (fun s -> token (string s)) aux)

let words_ci s =
  let aux = String.split_on_char ' ' s in
  list (List.map (fun s -> token (string_ci s)) aux)

let ( *> ) = ( *> )
let ( <* ) = ( <* )
let ( <|> ) = ( <|> )
let ( <?> ) = ( <?> )
let ( let* ) = ( let* )
let ( >>= ) = ( >>= )

(** [try_otherwise ps q] tries all parsers from [ps]. If all fail, use parser
    [q]. Unlike [choice ( ps @ [q] )], this combinator is biased towards [q], in
    the sense that [try_otherwise ps q] will produce [q]'s error message on
    failure. *)
let try_otherwise ps q =
  let* x = option None (map ~f:Option.some (choice ps)) in
  match x with Some x -> return x | None -> q

let parse_string p s =
  let p = p <* end_of_input in
  let open Buffered in
  let st = parse p in
  let st = feed st (`String s) in
  let st = feed st `Eof in
  match st with
  | Partial _ -> assert false
  | Done (_, x) -> x
  | Fail (unconsumed, trail, msg) ->
      let pos = unconsumed.off in
      let rec make_msg msg = function
        | [] -> [ msg ]
        | x :: xs -> Format.sprintf "when parsing %s:" x :: make_msg msg xs
      in
      let msg = String.concat " " (make_msg msg trail) in
      raise (Util.Parse_error { msg; pos })
