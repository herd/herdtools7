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

(** Extra functionalities for all architectures *)

(** Input signature, a reduced [Arch.ARCH] *)
module type I = sig
  module V : Value.S

  type arch_reg
  val pp_reg : arch_reg -> string
  val reg_compare : arch_reg -> arch_reg -> int

  type arch_instruction

end

(** Output signature, functionalities added *)
module type S = sig

  module I : I

  type global_loc = I.V.v 
 
  type proc = int
  val pp_proc : proc -> string

  type program_order_index = int
  val pp_prog_order_index : program_order_index -> string

  val zero_po_index : program_order_index
  val next_po_index : program_order_index -> program_order_index

  type inst_instance_id = {
      proc       : proc;
      program_order_index   : program_order_index;
      inst : I.arch_instruction; 
      unroll_count : int; (* number of loop unrollings *)
    }

  val inst_instance_compare :
      inst_instance_id -> inst_instance_id -> int

  include Location.S
  with type loc_reg = I.arch_reg and type loc_global = I.V.v
(* Extra for locations *)
  val maybev_to_location : MiscParser.maybev -> location
  val dump_location : location -> string

  val undetermined_vars_in_loc : location -> I.V.v option
  val simplify_vars_in_loc : I.V.solution ->  location -> location
  val map_loc : (I.V.v -> I.V.v) -> location -> location

  type state (* Now abstract, suicide ? *)

  val state_empty : state
  val pp_state : state -> string
  val dump_state : state -> string
  val pp_nice_state :
      state -> string (* delim, as in String.concat  *)
	-> (location -> I.V.v -> string) -> string

  (* for explict state construction *)
  val build_state : (location * ('t * I.V.v)) list -> state
  val build_concrete_state : (location * int) list -> state


  (* No comment *)
  val state_is_empty : state -> bool
  val state_to_list : state -> (location * I.V.v) list
  val state_size : state -> int
  val state_fold :
      (location -> I.V.v -> 'a -> 'a) -> state -> 'a -> 'a

  (* Look for what loc is bound to *)
  exception LocUndetermined (* raised when location is yet unkown *)
  val look_in_state : state -> location -> I.V.v

  (* Add a binding , shadow previous binding if any *)
  val state_add : state -> location -> I.V.v -> state

  (* Does loc binds v in state ? *)
  val state_mem : state -> location -> I.V.v -> bool

  (* State restriction to some locations *)
  val state_restrict : state -> (location -> bool) -> state

  (* Set of states *)
  module StateSet : MySet.S with type elt = state

  (*********************************)
  (* Components of test structures *)
  (*********************************)

  (* Test structures represent programmes loaded in memory
     and ready to start, plus some items that describe
     the test, such as its name (cf. Test.mli) *)


  (* Code memory is a mapping from labels to sequences of instructions,
     Too far from actual machine, maybe *)
  type code = (I.V.cst * I.arch_instruction) list

  module LabelMap : Map.S with type key = string


  (* Program loaded in memory *)
  type program = code LabelMap.t

  (* A starting address per proc *)
  type start_points = (proc * code) list


  (* Constraints *)
  type prop =  (location,I.V.v) ConstrGen.prop
  type constr = prop ConstrGen.constr


end

module type Config = sig
  val texmacros : bool
  val hexa : bool
  val brackets : bool
end

module Make(C:Config) (I:I) : S with module I = I
= struct

  module I = I

  type global_loc = I.V.v

  type proc = int

  let pp_proc = string_of_int

  type program_order_index = int
  let pp_prog_order_index = string_of_int

  let zero_po_index = 0
  let next_po_index po = po + 1


  type inst_instance_id = {
      proc       : proc;
      program_order_index   : program_order_index;
      inst : I.arch_instruction ;
      unroll_count: int; (* number of loop unrollings *)
    }


  let inst_instance_compare i1 i2 = match Misc.int_compare i1.proc i2.proc with
  | 0 -> Misc.int_compare i1.program_order_index i2.program_order_index
  | r -> r

  let pp_global = I.V.pp C.hexa

  include
      Location.Make
      (struct
        type arch_reg = I.arch_reg
        let pp_reg = I.pp_reg
        let reg_compare = I.reg_compare

        type arch_global = I.V.v
        let pp_global = pp_global
        let global_compare = I.V.compare
      end)

  let maybev_to_location v = Location_global (I.V.maybevToV v)

  let do_brackets =
    if C.brackets then Printf.sprintf "[%s]"
    else fun s -> s

  let dump_location = function
    | Location_reg (proc,r) ->
        string_of_int proc ^ ":" ^ I.pp_reg r
    | Location_global a -> do_brackets (pp_global a)

(* This redefines pp_location from Location.Make ... *)
  let pp_location l = match l with
  | Location_reg (proc,r) -> 
      let bodytext = string_of_int proc ^ ":" ^ I.pp_reg r in
      if C.texmacros 
      then "\\asm{Proc " ^ bodytext ^ "}" else bodytext
  | Location_global a -> do_brackets (pp_global a)

  let undetermined_vars_in_loc l =  match l with
  | Location_reg _ -> None
  | Location_global a ->
      if I.V.is_var_determined a then None
      else Some a


  let simplify_vars_in_loc soln l = match l with
  | Location_reg _  -> l
  | Location_global a ->
      Location_global (I.V.simplify_var soln a)

  let map_loc fv loc = match loc with
  | Location_reg _ -> loc
  | Location_global a -> Location_global (fv a)

(***************************************************)
(* State operations, implemented with library maps *)
(***************************************************)
  module State =
    Map.Make
      (struct
	type t = location
        let compare = location_compare
      end)

  type state = I.V.v State.t

  let state_empty = State.empty

  let pp_nice_state st delim pp_bd =
    let bds =
      State.fold
	(fun l v k -> (pp_bd l v)::k)
	st [] in
    String.concat delim  (List.rev bds)

  let pp_equal = if C.texmacros then "\\mathord{=}" else "="

  let pp_state st =
    pp_nice_state st " "
      (fun l v -> pp_location l ^ pp_equal ^ I.V.pp C.hexa v ^";")

  let dump_state st =
    pp_nice_state st " "
      (fun l v -> pp_location l ^ "=" ^ I.V.pp C.hexa v ^";")

  let build_state bds =
    List.fold_left (fun st (loc,(_,v)) -> State.add loc v st)
      State.empty bds

  let build_concrete_state bds =
    List.fold_left
      (fun st (loc,v) ->
	State.add loc (I.V.intToV v) st)
      State.empty bds

  let state_is_empty = State.is_empty

  let state_to_list st =
    List.rev (State.fold (fun l v k -> (l,v)::k) st [])

  let state_size st = State.fold (fun _ _ k -> 1+k) st 0

  let state_fold  f st x = State.fold f st x

(* State sets *)

  module StateSet =
    MySet.Make
      (struct
	type t = state

	let compare st1 st2 = State.compare I.V.compare st1 st2
      end)

(* To get protection against wandering undetermined locations,
   all loads from state are by this function *)
  exception LocUndetermined

  let look_in_state st loc =
    match undetermined_vars_in_loc loc with
    | Some _ -> 
    (* if loc is not determined, then we cannot get its
       content yet *)
	raise LocUndetermined
    | None ->
	try State.find loc st with Not_found -> I.V.zero

  let state_add st l v = State.add l v st

  let state_mem st loc v =
    try
      let w = look_in_state st loc in
      I.V.compare v w = 0
    with LocUndetermined -> assert false


  let state_restrict st loc_ok =
    State.fold
      (fun loc v k ->
        if loc_ok loc then State.add loc v k
        else k)
      st State.empty

  (*********************************)
  (* Components of test structures *)
  (*********************************)

  (* Code memory is a mapping from globals locs, to instructions *)
  type code = (I.V.cst * I.arch_instruction) list

  module LabelMap =
    Map.Make
      (struct
	type t = string
	let compare = String.compare
      end)


  (* Programm loaded in memory *)
  type program = code LabelMap.t

  (* A starting address per proc *)
  type start_points = (proc * code) list


  (* Constraints *)
  type prop =  (location,I.V.v) ConstrGen.prop
  type constr = prop ConstrGen.constr

end


