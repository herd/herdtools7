(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

(*
   Translate an ocamllex .ml file to a set of bnfc tokens and comment descriptors
 *)

open BNFC

(* A utility function to convert a token name into a valid bnfc identifier *)
let slug_str =
  String.map (function
    | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_') as c -> c
    | _ -> '_')

(* Extract token and comemnt data from an ocamllex -ml file. *)
module Convert (GRAMMAR : MenhirSdk.Cmly_api.GRAMMAR) : sig
  val comments : comment list
  val tokens : token list
  val reserved : decl list
end = struct

  let comments = [ Comment [ "//" ]; Comment [ "/*"; "*/" ] ]

  let tokens =
    let open GRAMMAR in
    let open Regex in
    let digit : Regex.part = MatchDigit in
    let alpha : Regex.part = MatchLetter in
    let int_lit : Regex.t =
      [ digit; zero_or_more @@ choice [ digit ; Char '_' ] ]
    in
    let hex_alpha : Regex.part = OneOf "abcdefABCDEF" in
    let hex_lit : Regex.t =
      [
        Str "0x";
        choice [ digit; hex_alpha ];
        zero_or_more @@ choice [ Char '_'; digit; hex_alpha ];
      ]
    in
    let mk_token terminal acc =
      let t_name = Terminal.name terminal in
      let name = slug_str t_name in
      let re regex = Token { name; regex } in
      let is_regular =
        match Terminal.kind terminal with `REGULAR -> true | _ -> false
      in
      let is_external_term term =
        let attrs = Terminal.attributes term in
        List.for_all (fun a -> not @@ Attribute.has_label "internal" a) attrs
      in
      let is_external = is_external_term terminal in
      if (not is_external) || (not is_regular) || String.equal t_name "EOF" then acc
      else
        let tok =
          match t_name with
          | "STRING_LIT" ->
              re
                [
                  Char '"';
                  zero_or_more @@
                      Choice
                        [
                          [ Except (MatchAll, OneOf "\"\\") ];
                          [ Char '\\'; OneOf "nt\"\\" ];
                        ];
                  Char '"';
                ]
          | "INT_LIT" -> re [ Choice [ int_lit; hex_lit ] ]
          | "REAL_LIT" -> re @@ int_lit @ [ Char '.' ] @ int_lit
          | "BITVECTOR_LIT" ->
              re
                [
                  Char '\'';
                  zero_or_more @@ OneOf "01 ";
                  Char '\'';
                ]
          | "MASK_LIT" ->
              re
                [
                  Char '\'';
                  zero_or_more @@ OneOf "01x ";
                  Char '\'';
                ]
          | "IDENTIFIER" ->
              re
                [
                  choice [ alpha; Char '_' ];
                  zero_or_more @@ choice [ alpha; digit; Char '_' ]
                ]
          | "BOOL_LIT" -> re [ choice [ Str "TRUE"; Str "FALSE" ] ]
          | _ ->
              let asl_tok =
                match Asllib.Lexer.token_of_string t_name with
                | None ->
                    raise
                    @@ Failure
                         (Printf.sprintf "No token with name %s known." t_name)
                | Some t -> t
              in
              let sym = Asllib.Lexer.token_to_symbol asl_tok in
              Token { name; regex = [ Str sym ] }
        in
        tok :: acc
    in
    let reserved_id =
      let name = "RESERVED_IDENTIFIER" in
      let regex : Regex.t =
        [
          Str "__";
          ZeroOrMore
            [ Choice [ [ MatchLetter ]; [ MatchDigit ]; [ Char '_' ] ] ];
        ]
      in
      Token { name; regex }
    in
    reserved_id :: GRAMMAR.Terminal.fold mk_token []

  let reserved =
    let mk_decl kw =
      let name = "ReservedKeyword" in
      let ast_name = name ^ "_" ^ slug_str kw in
      let terms = [ Literal kw ] in
      Decl { ast_name; name; terms }
    in
    List.map mk_decl Asllib.Lexer.reserved_keywords
end
