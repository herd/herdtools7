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

(** Miscellaneous functions *)

(***********************************************************)
(* Raised to finish the current test in various situations *)
(***********************************************************)

exception Exit
exception Timeout
exception UserError of string
exception Fatal of string
exception NoIsync

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
val polymorphic_compare : 'a -> 'a -> int
val int_compare : int -> int -> int
val int_eq : int -> int -> bool
val max_int : int -> int -> int
val min_int : int -> int -> int
val string_eq : string -> string -> bool
val bool_eq : bool -> bool -> bool

val identity : 'a -> 'a
(* ignore argument(s) *)
val ing : 'a -> unit
val ing2 : 'a -> 'b -> unit
(* raise Not_found *)
val not_found : unit -> 'a

val some : 'a -> 'a option
val is_none : 'a option -> bool
val is_some : 'a option -> bool
val as_some : 'a option -> 'a
val proj_opt : 'a -> 'a option -> 'a
val seq_opt : ('a -> 'b option) -> 'a option -> 'b option
val opt_list_fold : ('a -> 'b option) -> 'a list -> 'b list option

val app_opt : ('a -> 'b) -> 'a option -> 'b option
val check_opt : ('a -> unit) -> 'a option -> unit
val snd_opt : ('a * 'b) option -> 'b option
val map_opt : ('a -> 'b) -> 'a option -> 'b option
val app_opt_def : 'b -> ('a -> 'b) -> 'a option -> 'b
val pp_opt_arg : ('a -> string) -> 'a option -> string

val app_opt2 : ('v -> 'r) -> (unit -> 'r) -> ('i -> 'v option) -> 'i -> 'r
val delay_parse : ('v -> 'r) -> ('i -> 'v option) -> 'i -> 'r

val map_string : (char -> string) -> string -> string
val fmt_percent : string -> string
val skip_spaces : string -> string

val opt_compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int
val opt_eq : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
val pair : 'a -> 'b -> 'a * 'b
val pair_compare :
    ('a -> 'a -> int) -> ('b -> 'b -> int) -> 'a * 'b -> 'a * 'b -> int
val pair_eq :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> 'a * 'b -> 'a * 'b -> bool
val list_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
val list_eq : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool

val char_uppercase : char -> char
val lowercase : string -> string
val uppercase : string -> string
val capitalize : string -> string
val uncapitalize : string -> string

(* strip characters to form a valid c variable/type/enum name *)
val to_c_name : string -> string

(* Backward compatibility *)
val find_opt : ('a -> bool) -> 'a list -> 'a option
val split_on_char : char -> string -> string list
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(* Float pair (position) parsint *)
val pos_of_string : string -> (float * float) option

(* Generalize int parsing *)
val string_as_int : string -> int
val string_as_int64 : string -> Int64.t
val string_of_intkm : string -> int option
val explode : string -> char list


(* Fold utilities *)
val fold_to_iter :
    (('a -> unit -> unit) -> unit -> unit) -> ('a -> unit) -> unit
val fold_bool : (bool -> 'a -> 'a) -> 'a -> 'a


(* Some useful function on lists *)
val nilp : 'a list -> bool
val consp : 'a list -> bool
val cons : 'a -> 'a list -> 'a list
val last : 'a list -> 'a
val pop_last : 'a -> 'a list -> 'a * 'a list
val pop_filter_opt : ('a -> bool) -> 'a list -> 'a option * 'a list
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

(* Returns a list of pairs prefix * suffix of the argument list,
   with suffixes of size strictly less than size limit *)
val cuts : int -> 'a list -> ('a list * 'a list) list

(* strict version of List.for_all *)
val for_all_strict : ('a -> bool) -> 'a list -> bool

(* List.exists on list of list *)
val exists_exists : ('a -> bool) -> 'a list list -> bool

(* Does a pair in list satisfy predicate ? *)
val exists_pair : ('a -> 'a -> bool) -> 'a list -> bool

(* split a list into n list of as much as possible equal length *)
val nsplit : int -> 'a list -> 'a list list

(* Remove duplicates, according to equality function,
   WARNING, correct only when duplicates are in sequence *)
val rem_dups : ('a -> 'a -> bool) -> 'a list -> 'a list

(* group elements, quadratic *)
val group : ('a -> 'a -> bool) -> 'a list -> 'a list list
val group_iter : ('a -> 'a -> bool) -> ('a -> 'a list -> unit) -> 'a list -> unit
val group_iteri : ('a -> 'a -> bool) -> (int -> 'a -> 'a list -> unit) -> 'a list -> unit

(* Check that f yields the same result on all list elements *)
val check_same : ('a -> 'a -> bool) -> ('b -> 'a) -> 'b list -> 'a option

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

(* Prety print of symbolic registers *)
val dump_symbolic : string -> string

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

(* Choose late *)
val fold_argv_or_stdin : (string -> 'a -> 'a) -> string list -> 'a -> 'a
val iter_argv_or_stdin : (string -> unit) -> string list -> unit

(*********************)
(* Subset generators *)
(*********************)

(* Apply accumulating fonction on all subsets of a set.
  fold_subsets_gen xs f v0 computes f s1 (f s2 .. (f sN v0))
  where the si's are the sub-lists of xs.
  Notice: element order is preserved. *)
val fold_subsets : 'a list -> ('a list -> 'b -> 'b) -> 'b -> 'b

(* Generic version,
  fold_subsets_gen add y0 xs f v0 computes f s1 (f s2 .. (f sN v0))
  where the si's are, is add (yM .. (add y2 ... (add y1 start))) for
  each subset ysi = [y1;y2;...;yM;] of xs. *)
val fold_subsets_gen :
    ('a -> 'b -> 'b) -> 'b -> 'a list -> ('b -> 'c -> 'c) -> 'c -> 'c

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

(*******************)
(* Simple bindings *)
(*******************)

module Simple : sig
  type 'a bds = (string * 'a) list

  val assoc : string -> 'a bds -> 'a
  val assoc_opt : string -> 'a bds -> 'a option
  val mem : string -> string list -> bool
  val mem_assoc : string -> 'a bds -> bool
  val map : ('a -> 'b) -> 'a bds -> 'b bds
end

(*************)
(* Test name *)
(*************)

val clean_name : string -> string

(*************)
(* Tag names *)
(*************)

(* Tag names capability tags locations in memory.
   For location x, tag location is x.ctag *)

val add_ctag : string -> string
val check_ctag : string -> bool
val tr_ctag : string -> string

(* Tag names abstract tags locations in memory.
   For location x, tag location is x.atag *)

val add_atag : string -> string
val check_atag : string -> bool
val tr_atag : string -> string option
val is_atag : string -> bool

val add_pte : string -> string
val tr_pte : string ->  string option
val is_pte : string -> bool
val pp_pte : string -> string
val add_tlb : string -> string
val add_af : string -> string
val tr_af : string ->  string option
val add_db : string -> string
val tr_db : string ->  string option
val add_dbm : string -> string
val tr_dbm : string ->  string option
val add_valid : string -> string
val add_oa : string -> string

val add_physical : string -> string
val tr_physical : string -> string option
val is_physical : string -> bool
val pp_physical : string -> string

(******************)
(* Hash utilities *)
(******************)

val mix : int -> int -> int -> int

(*********************************)
(* Group by optional integer key *)
(*********************************)

val group_by_int : ('k -> int option) -> ('k * 'v) list -> ('k * 'v) list list
