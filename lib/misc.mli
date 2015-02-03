(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Miscellaneous functions *)

(***********************************************************)
(* Raised to finish the current test in various situations *)
(***********************************************************)

exception Exit
exception UserError of string
exception Fatal of string

(***********************)
(* Non-managed options *)
(***********************)

(* For switching something *)
val switch : bool ref
(* and something else *)
val switchelse : bool ref

(**************)
(* File names *)
(**************)
val dot_name : string -> string
val filebase : string -> string

(****************)
(* basic misc   *)
(****************)
external int_compare : int -> int -> int = "caml_int_compare"
val int_eq : int -> int -> bool
val string_eq : string -> string -> bool

external identity : 'a -> 'a = "%identity"

val is_none : 'a option -> bool
val as_some : 'a option -> 'a
val proj_opt : 'a -> 'a option -> 'a
val app_opt : ('a -> 'b) -> 'a option -> 'b option
val option_map : ('a -> 'b option) -> 'a list -> 'b list
val map_string : (char -> string) -> string -> string


(* Generalize int parsing *)
val string_of_intkm : string -> int option
val explode : string -> char list

(* Some useful function on lists *)
val consp : 'a list -> bool
val cons : 'a -> 'a list -> 'a list
val last : 'a list -> 'a
val pp_list :
  out_channel -> string -> (out_channel -> 'a -> unit) -> 'a list -> unit
val rev_iter : ('a -> unit) -> 'a list -> unit
val interval : int -> int -> int list
val replicate : int -> 'a -> 'a list
val iteri : (int -> 'a -> unit) -> 'a list -> unit
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val rev_filter : ('a -> bool) -> 'a list -> 'a list
val map3 :
    ('a -> 'b -> 'c -> 'd) ->
      'a list -> 'b list -> 'c list -> 'd list

(* strict version of List.for_all *)
val for_all_strict : ('a -> bool) -> 'a list -> bool

(* List.exists on list of list *)
val exists_exists : ('a -> bool) -> 'a list list -> bool

(* split a list into n list of as much as possible equal length *)
val nsplit : int -> 'a list -> 'a list list

(* Remove duplicates, according to equality function,
   WARNING, correct only when duplicates are in sequence *)
val rem_dups : ('a -> 'a -> bool) -> 'a list -> 'a list

(* Lift boolean connectors to predicates *)
val (|||) : ('a -> bool) -> ('a -> bool) -> 'a -> bool
val (&&&) : ('a -> bool) -> ('a -> bool) -> 'a -> bool

(* Array helpers *)
val array_map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array

(* String helpers *)
val split_comma : string -> string list

(******************)
(* Matrix helpers *)
(******************)

(* Fails on empty or non regular matrices *)
exception TransposeFailure
val transpose : 'a list list -> 'a list list

(* Pretty print (code) matrices *)
val pp_prog : out_channel -> string list list -> unit
val string_of_prog : string list list -> string
val lines_of_prog : string list list -> string list

(***************)
(* I/O helpers *)
(***************)

(* Call: 'ouput_protect f name'
   Does apply f to a channel chan resulting
   from opening file name.
   Channel chan is closed, even if f raises an exception.
   The exception is re-raised *)
val output_protect : (out_channel -> 'a) -> string -> 'a

(* Idem for input, raises Fatal msg if file cannot be opened *)
val input_protect : (in_channel -> 'a) -> string -> 'a

(* Generic versions, with opening functions as argumentrs *)
val output_protect_gen :
    (string -> out_channel) -> (out_channel -> 'a) -> string -> 'a
val input_protect_gen :
    (string -> in_channel) -> (in_channel -> 'a) -> string -> 'a

(* Another generic version, close safely *)
val output_protect_close  :
    ('chan -> unit) (* close *) ->
      ('chan -> 'b) (* action *) ->
        'chan -> 'b


(****************************)
(* Expand command line args *)
(****************************)

(* File names in style '@name' are replaced by their contents *)
val fold_argv : (string -> 'a -> 'a) -> string list -> 'a -> 'a
val iter_argv : (string -> unit) -> string list -> unit
val expand_argv : string list -> string list

type iter

val mk_iter : string list -> iter
val clean_iter : iter -> iter
val next_iter : iter -> (string * iter) option

(* Alternative: read stdin *)
val fold_stdin : (string -> 'a -> 'a) -> 'a -> 'a
val iter_stdin :  (string -> unit) -> unit

(****************************)
(* Cross product generators *)
(****************************)

(* Generate all elts in size n cross product and
   apply a function to them.

   'fold_cross  [xs1 ; ... xsN] f v0'
     computes f p1 (f p2 ... (f pM v0)), where the pi's are
     all lists [y1 ; ... yN] with
       y1,..., yN in (xs1 X ... X xsN)
*)
val fold_cross :  'a list list ->  ('a list -> 'b -> 'b) -> 'b -> 'b

(* Generalized cross product fold
   'fold_cross  add y0 [xs1 ; ... xsN] f v0
    computes  f p1 (f p2 ... (f pM v0)), as aboves where the pi's
    are 'add y1 (.. (add yN y0))'.

   Notice that fold_cross is
   fold_cross_gen (fun y ys -> y::ys) [] *)
val fold_cross_gen :
    ('a -> 'b -> 'b) -> 'b -> 'a list list -> ('b -> 'c -> 'c) -> 'c -> 'c
