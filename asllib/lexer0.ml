(****************************************************************
 * ASL lexer support
 *
 * Copyright Arm Limited (c) 2017-2019
 * SPDX-Licence-Identifier: BSD-3-Clause
 ****************************************************************)

(** ASL lexer support *)

open Lexing
open Parser0

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let starters : Parser0.token list = [ LPAREN; LBRACK; LBRACE; IF; ELSIF; WHILE ]
let enders : Parser0.token list = [ RPAREN; RBRACK; RBRACE; THEN; DO ]

type offside_state = {
  mutable stack : int list; (* indentation history *)
  mutable parens : int; (* number of outstanding openers *)
  mutable newline : bool; (* processing newline *)
  mutable next : Parser0.token; (* next token *)
}

let token : Lexing.lexbuf -> Parser0.token =
  let state = { stack = [ 0 ]; parens = 0; newline = false; next = EOL } in

  let pushStack (col : int) : Parser0.token =
    state.stack <- col :: state.stack;
    INDENT
  in

  let getToken (buf : Lexing.lexbuf) : Parser0.token =
    let useToken _ : Parser0.token =
      let tok : Parser0.token = state.next in
      if List.mem tok starters then state.parens <- state.parens + 1
      else if state.parens > 0 && List.mem tok enders then
        state.parens <- state.parens - 1;
      state.next <- SimpleLexer0.token buf;
      (* Printf.eprintf "Got token: %s\n" (SimpleLexer0.string_of_token tok); *)
      tok
    in

    let res =
      if state.parens > 0 then (
        (* In parentheses: ignore EOL tokens *)
        while state.next = EOL do
          ignore (useToken ())
        done;
        useToken ())
      else if state.next = EOF then (
        (* End of file: emit outstanding DEDENT tokens *)
        match state.stack with
        | [] | [ _ ] -> EOF
        | _d :: ds ->
            state.stack <- ds;
            DEDENT)
      else if state.next = EOL then (
        while state.next = EOL do
          state.newline <- true;
          ignore (useToken ())
        done;
        EOL)
      else if state.newline then
        let prev_col = List.hd state.stack in
        let pos = lexeme_start_p buf in
        let new_column = pos.pos_cnum - pos.pos_bol in
        if new_column > prev_col then (
          state.newline <- false;
          pushStack new_column)
        else if new_column = prev_col then (
          state.newline <- false;
          useToken ())
        else (
          state.stack <- List.tl state.stack;
          let target_column = List.hd state.stack in
          (* state.newline <- false; *)
          state.newline <- new_column <> target_column;
          (* This gives spurious warnings when indentation is
           * decremented in two steps.
           *
           * if new_column < target_column then begin
           *     Printf.printf "Warning: incorrect indentation %d: %d %d\n"
           *         buf.lex_curr_p.pos_lnum
           *         new_column target_column
           * end;
           *)
          DEDENT)
      else useToken ()
    in
    (*
      let () = Printf.eprintf "Producing token %s\n" (SimpleLexer0.string_of_token res) in
     *)
    res
  in
  getToken

(****************************************************************
 * End
 ****************************************************************)
