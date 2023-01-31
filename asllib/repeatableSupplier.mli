(**
    This module implements a repeatable supplier for parsing purposes.
 
    As defined in mehnir documentation, they are token generators, i.e. of the
    type
 
         type supplier = unit -> token * position * position
 
    This module changes because it provides a functionnal interface to a
    repeatable supplier, i.e. that can be forked and restarted. Besides
    constructors, there are two main functions to use a repeatable supplier:
 
     - [next] that replaces the supplier.
     - [copy] that allows forking the supplier.
 
    State is shared between the different forked instances of the parser.
    It keeps in memory all the read tokens from the start.
  *)

open Parser0
open Lexing

type state
(** Internal state of the supplier *)

val of_supplier : MenhirInterpreter.supplier -> state
(** Construct a supplier from another supplier. *)

val of_lexer_lexbuf : (lexbuf -> token) -> lexbuf -> state
(** Construct a supplier from a lexer. *)

val next : state -> token * position * position
(** Produce a new token. *)

val copy : state -> state
(** Fork the supplier to be able to repeat from this position. *)
