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

(** Values, ie constants and variables *)

module type S =
    sig

(* Constants, notice that they include symbolic "rigid" constants *)
      type cst = SymbConstant.v

(* flexible variables *)
      type csym = int (* Opened by Susmit, lose pointless abstraction *)
      val pp_csym : csym -> string
      val compare_csym : csym -> csym -> int

(* Values, ie constants + variables, that should be instanciated
   to constants later *)
      type v = 
        | Var of csym
        | Val of cst

      val pp_v  : v -> string
      val pp : bool (* hexa *) -> v -> string 

(* produce a fresh variable *)
      val fresh_var : unit -> v
      val from_var : csym -> v
      val as_var : v -> csym option

       
(* Equality (for constraint solver) is possible *)	  
      val equalityPossible : v -> v -> bool

(* Please use this for comparing constants... *)
      val compare : v -> v -> int

(* Build constant values, either numerical or symbolic *)
      val intToV  : int -> v 
      val nameToV  : string -> v
      val cstToV : cst -> v
      val maybevToV : MiscParser.maybev -> v

(* Convenience for intToV (0|1) *)
      val zero : v
      val one : v
      val two : v


      val is_zero : v -> bool
      val is_one : v -> bool

      (* The following operations may raise
         exception "Undetermined", if their arguments of
	 type v are not determined enough to yield a result *)
      exception Undetermined

      val op1 : Op.op1 -> v -> v
      val op : Op.op -> v -> v -> v
      val op3 : Op.op3 -> v -> v -> v -> v

      (* ??? *)
      val fold_over_vals : (v -> 'a -> 'a) -> 'a -> 'a

      module ValueSet : MySet.S with type elt = v
      module Solution : Map.S with type key = csym

      type solution = v Solution.t

      val is_var_determined : v -> bool
      val determined_val : v -> cst option
      val simplify_var : solution -> v -> v

    end
