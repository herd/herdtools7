open Parser0
open Lexing
module I = Parser0.MenhirInterpreter

type token_pos = token * position * position

type state = {
  src_supplier : I.supplier;
  seen_tokens : token_pos list ref;
  nb_seen_tokens : int ref;
  mutable current_tok : int;
}
(*
  Invariants:
     nb_seen_tokens = List.length seen_tokens
     current_tok <= nb_seen_tokens

  Only way of editing seen_tokens and nb_seen_tokens is in refill.
  Two ways of editing current_tok:
    - With next
    - Not updating it from a previous update with copy
*)

(* Constructors *)
let of_supplier supplier =
  {
    src_supplier = supplier;
    seen_tokens = ref [];
    nb_seen_tokens = ref 0;
    current_tok = 0;
  }

let of_lexer_lexbuf lexer lexbuf =
  of_supplier @@ I.lexer_lexbuf_to_supplier lexer lexbuf

(** Fetch new tokens from the source to ensure safe pulling. *)
let refill state =
  (* The while here might be unnecessary as we are always having at most one
     fetch to do, but a little bit more of security does not do much harm. *)
  while !(state.nb_seen_tokens) <= state.current_tok do
    let tok = state.src_supplier () in
    state.seen_tokens := tok :: !(state.seen_tokens);
    incr state.nb_seen_tokens
    (* Here: nb_seen_tokens = List.length seen_tokens *)
  done
(* Here: nb_seen_tokens >= current_tok + 1 *)

let next state =
  (* Make sure we have enough tokens in store to see the next one. *)
  refill state;

  (* Here: nb_seen_tokens >= current_tok + 1 *)

  (* Incremenet current_tok as we will have produced another token. *)
  state.current_tok <- state.current_tok + 1;

  (* Return the corresponding token. *)
  let i = !(state.nb_seen_tokens) - state.current_tok in
  List.nth !(state.seen_tokens) i
(* Here i >= 0 because of refill post-condition
        i < List.length seen_tokens because current_tok > 0 and the
                state invariant
*)

let copy (state : state) = { state with current_tok = state.current_tok }
