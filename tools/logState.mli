(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

exception StateMismatch of string


(* Concrete types for states *)
type st_concrete 

type parsed_st =
  {
    p_noccs : Int64.t ;                  (* number of occurrences *)
    p_st :  st_concrete ;
 }

(* Concrete type for outcomes (ie all final states of a given test) *)
type parsed_sts =
  {
   p_nouts : Int64.t ; (* number of outcomes (ie sum of p_noccs above) *)
   p_sts : parsed_st list ;
 }

type parsed_topologies = (HashedString.t * Int64.t) list

(* Abstract type for states *)
type sts
type topologies

type kind =
  | Allow | Require | Forbid
  | NoKind (* No kind in log *)
  | ErrorKind (* Propagate Errors *)
  | Undefined (* UB (eg data races) *)

val is_reliable : kind -> bool

type validation = Undef | Ok | No | DontKnow | Run

(* tr_validate kref k v
   Change validation v w.r.t k into a kind,
   kref is a reference kind, to check test nature *)
val tr_validate : kind -> kind -> validation -> kind option

type test =
 { tname : string ;      (* name of the test, aka key *)
   states : sts ;        (* final states observed *)
   condition : LogConstr.cond option ;
   (* Plain condition, enables reconstruction of following fields.
      Elle est pas belle la vie? *)
   kind : kind ;       (* Style of test Require/Allow etc. *)
   loop : bool ;       (* Test contains loop *)
   validation : validation ; (* condition validation status *)
   witnesses : Int64.t * Int64.t ; (* witnesses pos/neg *)
   hash : string option ;  (* Hash of init state + code *)
   time : float option ; (* Time of run *)
   topologies : topologies ; (* Summary of topologies for observing condition *)
 }

type t =
  { name : string    ; (* Name of the log file *)
    is_litmus : bool ; (* Litmus? (if_false == memevents log) *)
    tests : test array ; }


(* A memory efficient version of log compare *)

type simple_sts (* Storted st_concrete *)

type simple_test =
  { s_tname : string ;
    s_states : simple_sts ;
    s_hash : string ; }

type simple_t = { s_name : string ; s_tests : simple_test list; }


module Make(O:sig val verbose : int end) : sig

val as_st_concrete :  HashedPair.key Hashcons.hash_consed list -> st_concrete

val is_empty_simple : simple_test -> bool
val empty_sts : sts
val get_nouts : sts -> Int64.t
val millions : Int64.t -> float

(* Extract bindings from states, break abstraction, use with care *)
val get_bindings : sts -> (string * string) list list

(* - first argument is to be prefixed to all states
   - second argument is pp mode.
   - third argument means: show occurence numbers *)
val pretty_states : string -> OutMode.t -> bool -> sts -> string list
val dump_states : out_channel -> sts -> unit
(* bool true means litmus log, false memevents log *)
val dump_states_cond : out_channel -> bool -> sts -> unit
val no_states : sts -> bool

(* No states or one empty line in state *)
val no_states_or_no_obs : sts -> bool

val card : sts -> int

(* Topologies *)
val some_topologies : topologies -> bool
val dump_topologies : out_channel -> topologies -> unit


(* raise StateMismatch loc when loc is missing in  one of two sts *)
val union_states : sts -> sts -> sts

(* diff_states sts1 sts2 -> states in sts1 and not in sts2
   raises StateMismatch loc when loc is missing in  one of two sts *)
val diff_states : sts -> sts -> sts


val pp_kind : kind -> string
val parse_kind : string -> kind option
val pp_validation : validation -> string

(* Compare two normalized states
   the two states should hold bindings for the same locations in
   the same order *)

(* Normalize lists of final states, ie sort them *)

val normalize : string -> bool ->
  (string * kind *
   (parsed_st list *
      validation *
      (Int64.t * Int64.t) *
      LogConstr.cond option * bool *
      string option * parsed_topologies * float option)) list -> t

(* Conditions for from logs *)
val revalidate : LogConstr.cond option -> sts -> validation
val witness_again : LogConstr.cond option -> sts -> Int64.t * Int64.t
val filter : bool -> LogConstr.cond TblRename.t -> t -> test array
val count_outcomes : t -> int

(* Union logs *)
val union_logs : t list -> test array

(* Diff logs *)
val diff_logs : t -> t -> test array

(* Intersect logs *)
val inter_logs : t -> t -> test array


(* Rename logs *)
val rename : (string -> string) -> t -> t

(* Exclude some tests from logs, based upon name *)
val exclude : Str.regexp -> t -> t

(**************)
(* Simplified *)
(**************)

val normalize_simple :
    string ->
      bool ->
        (string * st_concrete list * string option) list ->
          simple_t


(* Check that tests in logs are the same *)
val simple_same :
    (string -> 'a -> 'a) -> (string -> 'a -> 'a) ->
      simple_t -> simple_t -> 'a -> 'a

(* Output test names whose output has elements  present in log and not in
   second *)
val simple_diff_not_empty :
    (string -> 'a -> 'a)  -> simple_t -> simple_t -> 'a -> 'a
(* Output test names whose output differ *)
val simple_diff :
    (string -> 'a -> 'a)  -> simple_t -> simple_t -> 'a -> 'a
end
