(*
   This Module implements some sort of a GLR parser for ASLv0.
   Such work is needed because ASLv0 is not a LR language, as the following
   statements show:

      if A < B && C >   then ...
      if A < B && C > D then ...

      (some_type, some_type, some_type, some_type) result = ...
      (some_var_, some_var_, some_var_, some_var_)        = ...

   The following replacements have been found, and implemented:
      - LT --> RBRACK, LT
      - GT --> LBRACK, GT
   The following replacements have been found, but not implemented
      - LPAREN --> LPAREN, <TO BE NAMED>
      - RPAREN --> RPAREN, <TO BE NAMED>

   It tries to work as follows:
      - for each possible ambiguous token, we try the possible tokens in order
        of most used path.
      - If an error is found, we try the next possible token
      - If it is accepted, we accept the constructed ast.
   For backtracking to be possible, the lexer should be repeatable. This has
   been implemented in the lexer_state module, that uses a mutable record to store
   both the lexer past tokens.

   Assumptions:
    - Finding if a replacement works or not should be quick (i.e. if it does
      not work, it fails after a few steps), as it should at most go to the end
      of the current statement.
    - The parser rules should be completely functionnal (or at least repeating
      them would not break anything).
    - Lexing should not fail eagerly.
 *)

open Parser0
open Lexing
module I = MenhirInterpreter
module RL = RepeatableLexer

type token_pos = token * position * position
type lexer_state = token RL.state
type 'a result = 'a Error.result

let _dbg = false
let toks = SimpleLexer0.string_of_token
let is_eof = function EOF -> true | _ -> false
let _max_lines = Hashtbl.create ~random:false 3

let _min_err e1 e2 =
  let open Error in
  let open AST in
  match (e1.desc, e2.desc) with
  | CannotParse, CannotParse ->
      if e1.pos_start.pos_lnum > e2.pos_start.pos_lnum then e1 else e2
  | _ -> e2

let list_first (f : 'a -> 'b result) : 'a list -> 'b result =
  let rec aux e1 : 'a list -> 'b result = function
    | [] -> Error e1
    | x :: t -> (
        match f x with Ok res -> Ok res | Error e2 -> aux (_min_err e1 e2) t)
  in
  function
  | [] -> assert false
  | x :: t -> ( match f x with Ok res -> Ok res | Error e1 -> aux e1 t)

let try_in_order (process : lexer_state -> token -> AST.t) lexer_state :
    token list -> AST.t =
  let one_possibility tok =
    if _dbg then Format.eprintf "@[<hv 4>Trying %s :@ @[<hov>" (toks tok);
    try
      let res = process (RL.copy lexer_state) tok in
      if _dbg then Format.eprintf "@]";
      Ok res
    with Error.ASLException e ->
      if _dbg then Format.eprintf "@]@ Did not work (%S).@]@ " (toks tok);
      Error e
  in
  fun tokens ->
    let () = if _dbg then Format.eprintf "@]@ " in
    match list_first one_possibility tokens with
    | Ok res -> res
    | Error e -> Error.fatal e

(** Main loop of the interpreter. Inspired by menhir documentation. *)
let rec loop lexer_state (p1, p2) : 'a I.checkpoint -> AST.t = function
  | I.InputNeeded _ as cpt -> (
      let tok, p1, p2 = RL.get lexer_state in
      let () =
        if false then
          Format.eprintf "@[<3>Reading token %s@ at position@ @[<h>%a@]@]@."
            (SimpleLexer0.string_of_token tok)
            PP.pp_pos
            (ASTUtils.annotated () p1 p2)
      in
      let continue = continue cpt (p1, p2) in
      match tok with
      | LT -> try_in_order continue lexer_state [ LBRACK; LT ]
      | GT -> try_in_order continue lexer_state [ RBRACK; GT ]
      | RETURN ->
          let () =
            if false then
              Format.eprintf "Seen pos %a@ " PP.pp_pos
                (ASTUtils.annotated () p1 p2)
          in
          continue lexer_state tok
      | tok -> continue lexer_state tok)
  | (I.Shifting _ | I.AboutToReduce _) as checkpoint ->
      loop lexer_state (p1, p2) (I.resume checkpoint)
  | I.HandlingError _ ->
      let () =
        if true then
          match Hashtbl.find_opt _max_lines p1.pos_fname with
          | None -> Hashtbl.add _max_lines p1.pos_fname p1
          | Some p ->
              if false && p.pos_lnum < p1.pos_lnum then (
                Hashtbl.add _max_lines p1.pos_fname p1;
                Format.eprintf "@[%a:@ Found error.@]@." PP.pp_pos
                  (ASTUtils.annotated () p1 p2))
      in
      Error.fatal_here p1 p2 Error.CannotParse
  | I.Accepted ast -> ast
  | I.Rejected -> assert false

(** Continuation for [try_in_order] *)
and continue cpt (p1, p2) lexer_state tok =
  let () = if false && _dbg then Format.eprintf "%s@ " (toks tok) in
  loop lexer_state (p1, p2) @@ I.offer cpt (tok, p1, p2)

(** Alternative entry-point for this module. This one take directly a repeatableLexer. *)
let parse_repeatable parse lexer_state lexbuf : AST.t =
  if _dbg then Format.eprintf "@[<v 4>Starting parsing...@ @[<hov 4>";
  let first_checkpoint = parse lexbuf.lex_curr_p in
  let res =
    loop lexer_state (lexbuf.lex_start_p, lexbuf.lex_curr_p) first_checkpoint
  in
  let () = if _dbg then Format.eprintf "@]@." in
  res

let ast_chunk lexbuf =
  let lexer = Lexer0.token () in
  let lexer_state = RL.of_lexer_lexbuf is_eof lexer lexbuf in
  let r = parse_repeatable Parser0.Incremental.ast lexer_state lexbuf in
  if false then Printf.eprintf "Chunk of size %d\n" (List.length r);
  r

(** The main entry-point for this module. Should be usable as a drop-in
    replacement for [Parser0.ast]. *)

(* Set [as_chunks] to false parsing ASL files as a whole *)

let as_chunks = true

let ast (lexer : lexbuf -> token) (lexbuf : lexbuf) : AST.t =
  if as_chunks then
    let asts =
      Seq.fold_left
        (fun k chunk ->
          let ast = ast_chunk (Lexing.from_string chunk) in
          ast :: k)
        [] (Splitasl.split lexbuf)
    in
    List.concat (List.rev asts)
  else
    let lexer_state = RL.of_lexer_lexbuf is_eof lexer lexbuf in
    parse_repeatable Parser0.Incremental.ast lexer_state lexbuf

let opn (lexer : lexbuf -> token) (lexbuf : lexbuf) : AST.t =
  let () =
    if _dbg then
      Format.eprintf "Starting parsing opn in file %s@."
        lexbuf.lex_curr_p.pos_fname
  in
  let lexer_state = RL.of_lexer_lexbuf is_eof lexer lexbuf in
  let res = parse_repeatable Parser0.Incremental.opn lexer_state lexbuf in
  let () = if _dbg then Format.eprintf "Parsed opn.@." in
  res
