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

(** Extra functionalities for all architectures *)

(** Input signature, a reduced [Arch.ARCH] *)
module type I = sig
  module V : Value.S
  val endian : Endian.t

  type arch_reg
  val pp_reg : arch_reg -> string
  val reg_compare : arch_reg -> arch_reg -> int

  type arch_instruction

  val fromto_of_instr : arch_instruction -> (Label.Set.t * Label.Set.t) option
end

(** Output signature, functionalities added *)
module type S = sig

  module I : I

  type global_loc = I.V.v
  type v = I.V.v

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
      labels : Label.Set.t;
    }

  val inst_instance_compare :
      inst_instance_id -> inst_instance_id -> int

  include Location.S
  with type loc_reg = I.arch_reg and type loc_global = v
(* Extra for locations *)
  val maybev_to_location : MiscParser.maybev -> location
  val do_dump_location : (string -> string) -> location -> string
  val dump_location : location -> string

  val undetermined_vars_in_loc : location -> v option
  val simplify_vars_in_loc : I.V.solution ->  location -> location
  val map_loc : (v -> v) -> location -> location

(* Mixed size *)
  val byte : MachSize.sz
  val endian : Endian.t
  val byte_sz : int
  val mask : string
  val nshift : int
  val nsz : MachSize.sz -> int
  (* decompose effective address increasimg order *)
  val byte_indices :  MachSize.sz -> int list
  (* decompose effective address increasimg order, endianess order *)
  val byte_eas :  MachSize.sz -> v -> v list
  val explode : MachSize.sz -> v ->  v list
  val recompose : v list -> v

(* State *)
  type state (* Now abstract, suicide ? *)
  type size_env

  val state_empty : state
  val pp_state : state -> string
  val do_dump_state : (string -> string) -> state -> string
  val dump_state : state -> string
  val pp_nice_state :
      state -> string (* delim, as in String.concat  *)
        -> (location -> v -> string) -> string

(* for explict state construction *)
  val build_state : (location * ('t * v)) list -> state
  val build_concrete_state : (location * int) list -> state


(* No comment *)
  val state_is_empty : state -> bool
  val state_to_list : state -> (location * v) list
  val state_size : state -> int
  val state_fold :
      (location -> v -> 'a -> 'a) -> state -> 'a -> 'a

(* Look for what address loc is bound to *)
  exception LocUndetermined (* raised when location is yet unkown *)

  val look_address_in_state : state -> location -> v
(* Look what memory cell is bound to *)
  val look_in_state : size_env -> state -> location -> v

(* Add a binding , shadow previous binding if any *)
  val state_add : state -> location -> v -> state

(* Does loc binds v in state ? *)
  val state_mem : size_env -> state -> location -> v -> bool

(* State restriction to some locations *)
  val state_restrict : state -> (location -> bool) -> state

(* Set of states *)
  module StateSet : MySet.S with type elt = state

(* Typing environment *)
  val size_env_empty : size_env
  val build_size_env : (location * (MiscParser.run_type * 'v)) list -> size_env
  val look_size : size_env -> string -> MachSize.sz
  val look_size_location : size_env -> location -> MachSize.sz

  val state_restrict_locs : LocSet.t -> size_env -> state -> state

(*********************************)
(* Components of test structures *)
(*********************************)

(* Test structures represent programmes loaded in memory
   and ready to start, plus some items that describe
   the test, such as its name (cf. Test.mli) *)


(* Code memory is a mapping from labels to sequences of instructions, too far from actual machine, maybe *)
  type code = (int * I.arch_instruction) list

  module LabelMap : Map.S with type key = string


(* Program loaded in memory *)
  type program = code LabelMap.t

(* A starting address per proc *)
  type start_points = (proc * code) list


(* Constraints *)
  type prop =  (location,v) ConstrGen.prop
  type constr = prop ConstrGen.constr


end

module type Config = sig
  val texmacros : bool
  val hexa : bool
  val brackets : bool
  val variant : Variant.t -> bool
  val byte : MachSize.sz
  val endian : Endian.t option
end

module Make(C:Config) (I:I) : S with module I = I
    = struct

      module I = I
      type v = I.V.v

      type global_loc = v

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
          labels : Label.Set.t ;
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

            type arch_global = v
            let pp_global = pp_global
            let global_compare = I.V.compare
          end)

      let maybev_to_location v = Location_global (I.V.maybevToV v)

      let do_brackets =
        if C.brackets then Printf.sprintf "[%s]"
        else fun s -> s

      let do_dump_location tr = function
        | Location_reg (proc,r) ->
            tr (string_of_int proc ^ ":" ^ I.pp_reg r)
        | Location_global a -> do_brackets (pp_global a)
        | Location_deref (a,idx) -> Printf.sprintf "%s[%i]" (pp_global a) idx

      let dump_location = do_dump_location Misc.identity

(* This redefines pp_location from Location.Make ... *)
      let pp_location l = match l with
      | Location_reg (proc,r) ->
          let bodytext = string_of_int proc ^ ":" ^ I.pp_reg r in
          if C.texmacros
          then "\\asm{Proc " ^ bodytext ^ "}" else bodytext
      | Location_global a -> do_brackets (pp_global a)
      | Location_deref (a,idx) ->  Printf.sprintf "%s[%i]" (pp_global a) idx

      let undetermined_vars_in_loc l =  match l with
      | Location_reg _ -> None
      | Location_global a|Location_deref (a,_) ->
          if I.V.is_var_determined a then None
          else Some a


      let simplify_vars_in_loc soln l = match l with
      | Location_reg _ -> l
      | Location_global a ->
          Location_global (I.V.simplify_var soln a)
      | Location_deref (a,idx) ->
          Location_deref (I.V.simplify_var soln a,idx)

      let map_loc fv loc = match loc with
      | Location_reg _ -> loc
      | Location_global a -> Location_global (fv a)
      | Location_deref (a,idx) -> Location_deref (fv a,idx)

(************************)
(* Mixed size utilities *)
(************************)

      let byte = C.byte

      let endian = match C.endian with
      | None -> I.endian
      | Some e -> e

      let byte_sz =  MachSize.nbytes byte

      let mask = match byte_sz with
      | 1 -> "0xff"
      | 2 -> "0xffff"
      | 4 -> "0xffffffff"
      | _ ->
          Warn.user_error "Size cannot be %s in mixed-size mode"
            (MachSize.pp C.byte)
      let nshift = MachSize.nbits byte

      let nsz sz =
        let n = MachSize.nbytes sz in
        if n < byte_sz then
          Warn.fatal "Size mismatch %s bigger than %s\n"
            (MachSize.debug sz) (MachSize.debug byte) ;
        assert (n mod byte_sz = 0) ;
        n / byte_sz

      let byte_indices sz =
        let kmax = nsz sz in
        let rec do_rec k =
          if k >= kmax then []
          else
            let ds = do_rec (k+1) in
            let d = k*byte_sz in
            d::ds in
        0::do_rec 1

      let byte_eas_incr sz a =
        let kmax = nsz sz in
        let rec do_rec k =
          if k >= kmax then []
          else
            let ds = do_rec (k+1) in
            let d = I.V.op1 (Op.AddK (k*byte_sz)) a in
            d::ds in
        a::do_rec 1

      let byte_eas sz a =
        let r = byte_eas_incr sz a in
        match endian with
        | Endian.Little -> r
        | Endian.Big -> List.rev r


      let explode sz v =
        let rec do_rec k v =
          if k <= 1 then [v]
          else
            let d = I.V.op1 (Op.AndK mask) v
            and w = I.V.op1 (Op.LogicalRightShift nshift) v in
            let ds = do_rec (k-1) w in
            d::ds in
        do_rec (nsz sz) v

(*      let extract_bit sz v i =
        let l = explode sz v in
        List.nth l (i-1)

      let extract_interval l i j =
        let rec do_rec acc k =
          match (k-i) with
          | 0 -> acc
          | _ -> do_rec ((List.nth l (k-1))::acc) (k-1)
        in do_rec [] j

      let extract_bit_interval sz v i j =
        let l = explode sz v in
        extract_interval l i j *)

      let rec recompose ds = match ds with
      | [] -> assert false
      | [d] -> d
      | d::ds ->
          let w = recompose ds in
          I.V.op Op.Or (I.V.op1 (Op.LeftShift nshift) w) d

(***************************************************)
(* State operations, implemented with library maps *)
(***************************************************)
      module State =
        MyMap.Make
          (struct
            type t = location
            let compare = location_compare
          end)

      type state = v State.t

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

      let do_dump_state tr st =
        pp_nice_state st " "
          (fun l v -> do_dump_location tr l ^ "=" ^ I.V.pp C.hexa v ^";")

      let dump_state st = do_dump_state Misc.identity st

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

      let get_in_state loc st = State.safe_find I.V.zero loc st
      let get_of_val st a = State.safe_find I.V.zero (Location_global a) st

      let look_address_in_state st loc =
        match undetermined_vars_in_loc loc with
        | Some _ ->
            (* if loc is not determined, then we cannot get its
            content yet *)
            raise LocUndetermined
        | None -> get_in_state loc st

      let look_size env s = StringMap.safe_find MachSize.Word s env

      let look_size_location env loc = match loc with
      | Location_global (I.V.Val (Constant.Symbolic (s,0))) ->  look_size env s
      | _ -> assert false

      let look_in_state_mixed senv st loc =
        match undetermined_vars_in_loc loc with
      | Some _ ->
          (* if loc is not determined, then we cannot get its
             content yet *)
          raise LocUndetermined
      | None ->
          match loc with
          | Location_global (I.V.Val (Constant.Symbolic (s,0)) as a)   ->
              let sz = look_size senv s in
              let eas = byte_eas sz a in
              let vs = List.map (get_of_val st) eas in
              let v = recompose vs in
              v
          | _ ->
              assert (not (is_global loc)) ;
              get_in_state loc st


      let look_in_state =
        if C.variant Variant.Mixed then  look_in_state_mixed
        else fun _senv -> look_address_in_state (* No need for size-env when sizes are ignored *)

      let state_add st l v = State.add l v st

      let state_mem senv st loc v =
        try
          let w = look_in_state senv st loc in
          I.V.compare v w = 0
        with LocUndetermined -> assert false


      let state_restrict st loc_ok =
        State.fold
          (fun loc v k ->
            if loc_ok loc then State.add loc v k
            else k)
          st State.empty

      let state_restrict_locs_non_mixed locs _ st =
        LocSet.fold
          (fun loc r -> state_add r loc (look_address_in_state st loc))
          locs state_empty

          (* Typing *)

      type size_env = MachSize.sz StringMap.t
      let size_env_empty = StringMap.empty

          (* Simplified typing, size only, integer types only *)

      let size_of = function
        | "atomic_t"
        | "int"|"long"
        | "int32_t"
        | "uint32_t" ->  MachSize.Word
        | "char"|"int8_t" |"uint8_t" -> MachSize.Byte
        | "short" | "int16_t" | "uint16_t" -> MachSize.Short
        | "int64_t" | "uint64_t" -> MachSize.Quad
        | "intprt_t" | "uintprt_t" -> I.V.Cst.Scalar.machsize (* Maximal size = ptr size *)
        | t ->
            Warn.fatal "Cannot find the size of type %s" t


      let misc_to_size ty  = match ty with
        | MiscParser.TyDef -> size_of "int"
        | MiscParser.Ty t|MiscParser.Atomic t -> (size_of t)
        | MiscParser.Pointer _
        | MiscParser.TyDefPointer
        | MiscParser.TyArray _ ->
            Warn.fatal "Type %s is not allowed in mixed size mode"
              (MiscParser.pp_run_type ty)

      let build_size_env =
        if C.variant Variant.Mixed then
          fun bds ->
            List.fold_left
              (fun m (loc,(t,_)) -> match loc with
              | Location_global a -> StringMap.add (I.V.as_symbol a) (misc_to_size t) m
              | _ -> m)
              StringMap.empty bds
        else
          (fun _ -> StringMap.empty)


      let state_restrict_locs_mixed locs senv st =
        LocSet.fold
          (fun loc r -> state_add r loc (look_in_state_mixed senv st loc))
          locs state_empty


      let state_restrict_locs =
        if C.variant Variant.Mixed then state_restrict_locs_mixed
        else  state_restrict_locs_non_mixed

(*********************************)
(* Components of test structures *)
(*********************************)

(* Code memory is a mapping from globals locs, to instructions *)

      type code = (int * I.arch_instruction) list

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
      type prop =  (location,v) ConstrGen.prop
      type constr = prop ConstrGen.constr

    end
