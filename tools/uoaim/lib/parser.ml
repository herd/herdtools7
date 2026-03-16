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

module S = Structure
module A = Angstrom
open ParserUtil

let subclause =
  let comma = comma *> A.return "," in
  let* ws = lparen *> A.many1 (token word <|> comma) <* rparen in
  A.return (Util.String.from_words ([ "(" ] @ ws @ [ ")" ]))

let constraint_chunk = token word <|> subclause

let constr ptail =
  let* chks = A.many1 (token constraint_chunk) in
  let c = Util.String.from_words chks in
  let* _ = ptail <?> "constraint termination symbol" in
  A.return (S.Constr c)

(* let star_count = token (A.many1 (char '*')) |> A.map ~f:List.length *)

let bullet_point expected_level =
  let char_point =
    match expected_level with
    | 0 -> A.return ()
    | 1 -> token (char 'o') *> A.return ()
    | 2 -> token (char '-') *> A.return ()
    | 3 -> token (A.string "++") *> A.return ()
    | _ -> A.fail "unsupported level"
  in
  let star_point =
    let* s = token (A.many1 (char '*')) in
    if List.length s = expected_level then A.return ()
    else A.fail "wrong star count"
  in
  let expected = Format.sprintf "bullet point of level %d" expected_level in
  on_fail ~expected (char_point <|> star_point)

let rec connection_point parent_level =
  let* ws = A.many_till (token word) (A.char ':') <* A.commit in
  let phrase = String.lowercase_ascii (Util.String.from_words ws) in
  if String.equal phrase "all of the following apply" then
    let* ii = whitespace *> items (parent_level + 1) in
    A.return (S.And ii)
  else if String.equal phrase "one of the following applies" then
    let* ii = whitespace *> items (parent_level + 1) in
    A.return (S.Or ii)
  else
    let phrase = Util.String.from_words ws in
    let msg = Format.sprintf "invalid connective phrase %S" phrase in
    A.fail msg

and items expected_level = A.many1 (item expected_level)

and item expected_level =
  (* let* level = star_count in *)
  (* if level <> expected_level then *)
  (*   A.fail (Printf.sprintf "expected list item with indentation level %d, but got a level %d item" expected_level level) *)
  (* else *)
  let* _ = bullet_point expected_level in
  let* _ = A.commit in
  try_otherwise [ connection_point expected_level ] (constr dot)

let structure ptail = try_otherwise [ connection_point 0 ] (constr ptail)

module Definition = struct
  type t = {
    head_phrase : string;
    endpoints : Eff.t list;
    structure : string Structure.t;
  }

  let pp fmt t =
    let open Format in
    fprintf fmt "Head phrase: %S@," t.head_phrase;
    fprintf fmt "Endpoints: %a@," (Util.pp_list_semicolon Eff.pp) t.endpoints;
    fprintf fmt "@[<v 2>Structure:@,%a@]"
      (Structure.pp Format.pp_print_string)
      t.structure

  let effect_p : Eff.t A.t =
    let* _ =
      token (A.string "another" <|> A.string "an")
      *> token (A.string "Effect")
      *> A.char 'E'
    in
    let* n = token number in
    A.return (Eff.make n)

  let preamble : (string * Eff.t list) A.t =
    let module Either = Util.Either in
    let preamble_chunk =
      A.map effect_p ~f:Either.left <|> A.map (token word) ~f:Either.right
    in
    let if_phrase = token (A.string "if and only if" <|> A.string "if") in
    let* ws = A.many_till preamble_chunk if_phrase in
    let preamble =
      ws
      |> List.map (Either.fold ~left:Eff.to_string ~right:Fun.id)
      |> String.concat " "
    in
    let mentioned =
      List.filter_map
        (function Either.Left n -> Some n | Either.Right _ -> None)
        ws
    in
    A.return (preamble, mentioned)

  let parser : t A.t =
    let* head_phrase, endpoints = preamble in
    let* structure = structure dot in
    A.return { head_phrase; endpoints; structure }

  let parse str : t =
    let str = String.uncapitalize_ascii str in
    ParserUtil.parse_string parser str
end

module HwReq = struct
  type t = { pre : string Structure.t; post : string }

  let pp fmt t =
    let open Format in
    fprintf fmt "@[<v 2>Pre-condition:@,%a@]@,"
      (Structure.pp Format.pp_print_string)
      t.pre;
    fprintf fmt "Post-condition: %S@," t.post

  let conclusion : string A.t =
    let concl_a =
      let* _ = words "the hardware is not architecturally Allowed to let" in
      let* ws = A.many1 (token word) in
      A.return (Util.String.from_words ws)
    in
    let concl_b =
      let* es = A.many_till (token word) (token (A.string "is")) in
      let* _ = words "not architecturally Allowed to" in
      let* ws = A.many1 (token word) in
      A.return (Util.String.from_words (es @ ws))
    in
    A.choice ~failure_msg:"hw req conclusion" [ concl_a; concl_b ]
    <?> "hw req conclusion"

  let pre_post : (string Structure.t * string) A.t =
    let* _ =
      A.option ()
        (words_ci "this places a requirement on the hardware that"
        *> A.return ())
    in
    let* _ = token (string_ci "if") in
    let* pre = structure comma in
    let* _ = token (string_ci "then") in
    let* post = conclusion in
    let* _ = dot *> A.end_of_input in
    A.return (pre, post)

  let parse str : t =
    let str = String.uncapitalize_ascii str in
    let pre, post = ParserUtil.parse_string pre_post str in
    { pre; post }
end
