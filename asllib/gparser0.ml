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
   been implemented in the Supplier module, that uses a mutable record to store
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
module RS = RepeatableSupplier

type token_pos = token * position * position
type supplier = RS.state

let _dbg = false
let toks = SimpleLexer0.string_of_token

let rec list_first (f : 'a -> 'b option) : 'a list -> 'b option = function
  | [] -> None
  | [ x ] -> f x
  | x :: t -> ( match f x with Some res -> Some res | None -> list_first f t)

let try_in_order (process : supplier -> token -> AST.t) (supplier : supplier) :
    token list -> AST.t =
  let one_possibility tok =
    if _dbg then Format.eprintf "@[<hv 4>Trying %s:@ @[<hov>" (toks tok);
    try
      let res = process (RS.copy supplier) tok in
      if _dbg then Format.eprintf "@]";
      Some res
    with Error ->
      if _dbg then Format.eprintf "@]@ Did not work (%S).@]@ " (toks tok);
      None
  in
  fun tokens ->
    let () = if _dbg then Format.eprintf "@]@ " in
    match list_first one_possibility tokens with
    | Some res -> res
    | None -> raise Error

(** Main loop of the interpreter. Inspired by menhir documentation. *)
let rec loop (supplier : supplier) : 'a I.checkpoint -> AST.t = function
  | I.InputNeeded _ as cpt -> (
      let tok, p1, p2 = RS.next supplier in
      let continue = continue cpt (p1, p2) in
      match tok with
      | LT -> try_in_order continue supplier [ LBRACK; LT ]
      | GT -> try_in_order continue supplier [ RBRACK; GT ]
      | tok -> continue supplier tok)
  | (I.Shifting _ | I.AboutToReduce _) as checkpoint ->
      loop supplier (I.resume checkpoint)
  | I.HandlingError _ -> raise Error
  | I.Accepted ast -> ast
  | I.Rejected -> assert false

(** Continuation for [try_in_order] *)
and continue cpt (p1, p2) supplier tok =
  let () = if _dbg then Format.eprintf "%s@ " (toks tok) in
  loop supplier @@ I.offer cpt (tok, p1, p2)

(** The main entry-point for this module. Should be usable as a drop-in
    replacement for [Parser0.ast]. *)
let parse (lexer : lexbuf -> token) (lexbuf : lexbuf) : AST.t =
  if _dbg then Format.eprintf "@[<v 4>Starting parsing...@ @[<hov 4>";
  let first_checkpoint = Parser0.Incremental.ast lexbuf.lex_curr_p in
  let supplier = RS.of_lexer_lexbuf lexer lexbuf in
  let res = loop supplier first_checkpoint in
  let () = if _dbg then Format.eprintf "@]@." in
  res
