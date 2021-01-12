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

  val is_mixed : bool

  module I : I

  type global_loc = I.V.v
  type v = I.V.v

  module VSet : MySet.S with type elt = v
  module VMap : MyMap.S with type key = v

  type proc = Proc.t
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

  val same_instruction :
      inst_instance_id -> inst_instance_id -> bool

  val pp_global : global_loc -> string
  include Location.S
  with type loc_reg := I.arch_reg and type loc_global := v

(* Extra for locations *)
  val maybev_to_location : MiscParser.maybev -> location
  val do_dump_location : (string -> string) -> location -> string
  val dump_location : location -> string

  val undetermined_vars_in_loc : location -> v option
  val simplify_vars_in_loc : I.V.solution ->  location -> location
  val map_loc : (v -> v) -> location -> location

(**********)
(* Faults *)
(**********)
  include Fault.S with type loc_global := v

(*********)
(* State *)
(*********)
  type state

  val state_empty : state
  val pp_state : state -> string
  val do_dump_state : (string -> string) -> state -> string
  val dump_state : state -> string
  val pp_nice_state :
      state -> string (* delim, as in String.concat  *)
        -> (location -> v -> string) -> string

  val build_state : (location * (MiscParser.run_type * v)) list -> state
  val build_concrete_state : (location * int) list -> state

(* Fails with user error when t is not an array type or in case of
   out-of-bound access *)
  val scale_array_reference : MiscParser.run_type -> location -> int -> location
(* This symbol is legal in non-mixed-size mode.
   That is, internal offset is null
    or symbol is an  aligned array cell access. *)

  val state_is_empty : state -> bool
  val state_add : state -> location -> v -> state
  val state_add_if_undefined  : state -> location -> v -> state
  val state_to_list : state -> (location * v) list
  val state_size : state -> int
  val state_fold : (location -> v -> 'a -> 'a) -> state -> 'a -> 'a
  val state_filter : (location -> bool) -> state -> state

  (* Exception raised when location is yet unkown *)
  exception LocUndetermined
  val look_address_in_state : state -> location -> v

(****************)
(* Environments *)
(****************)

  val size_of_t : string -> MachSize.sz

  type size_env
  val size_env_empty : size_env
  val build_size_env : (location * (MiscParser.run_type * 'v)) list -> size_env
  val look_size : size_env -> string -> MachSize.sz
  val look_size_location : size_env -> location -> MachSize.sz

  type type_env
  val type_env_empty : type_env
  val build_type_env : (location * (MiscParser.run_type * 'v)) list -> type_env
  val look_type : type_env -> location -> MiscParser.run_type
  val loc_of_rloc : type_env -> rlocation -> location

  (* Final states and sets of  *)
  type rstate
  val rstate_to_list : rstate -> (rlocation * v) list
  val rstate_filter : (rlocation -> bool) -> rstate -> rstate

  type final_state = rstate * FaultSet.t
  val do_dump_final_state : (string -> string) -> final_state -> string

  module StateSet : MySet.S with type elt = final_state

(*****************************************)
(* Size dependent items (for mixed-size) *)
(*****************************************)
  module Mixed : functor (SZ : ByteSize.S) -> sig
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

(* Look what memory cell is bound to *)
    val look_in_state : size_env -> state -> location -> v
    val look_in_state_rlocs : rstate -> rlocation -> v

(* State restriction to some locations *)
    val state_restrict_locs :
      bool (* keep all register bindings *) ->
      RLocSet.t -> type_env -> size_env -> state -> rstate
  end

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
  val endian : Endian.t option
end

module Make(C:Config) (I:I) : S with module I = I
    = struct

      let is_mixed = C.variant Variant.Mixed || C.variant Variant.Morello

      module I = I
      type v = I.V.v

      module OV =
        struct
          type t = v
          let compare = I.V.compare
        end

      module VSet = MySet.Make(OV)
      module VMap = MyMap.Make(OV)

      type global_loc = v

      type proc = Proc.t

      let pp_proc = Proc.dump

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

      let same_instruction i1 i2 = i1.inst == i2.inst

      let pp_global = I.V.pp C.hexa

      module LocArg =
        struct
          type arch_reg = I.arch_reg
          let pp_reg = I.pp_reg
          let reg_compare = I.reg_compare

          type arch_global = v
          let pp_global = pp_global
          let global_compare = I.V.compare
          let same_base g1 g2 =
            let b1 = I.V.get_sym g1
            and b2 = I.V.get_sym g2 in
            Misc.string_eq b1 b2
        end

      include Location.Make (LocArg)

      let maybev_to_location v = Location_global (I.V.maybevToV v)

      let do_brackets =
        if C.brackets then Printf.sprintf "[%s]"
        else fun s -> s

      let do_dump_location tr =
        let open Constant in function
        | Location_reg (proc,r) ->
            tr (string_of_int proc ^ ":" ^ I.pp_reg r)
        | Location_global a -> do_brackets (pp_global a)

      let dump_location = do_dump_location Misc.identity

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
      | Location_reg _ -> l
      | Location_global a ->
          Location_global (I.V.simplify_var soln a)

      let map_loc fv loc = match loc with
      | Location_reg _ -> loc
      | Location_global a -> Location_global (fv a)

(*********)
(* Fault *)
(*********)
      include Fault.Make(LocArg)

(************************)
(* Mixed size utilities *)
(************************)

      module State = LocMap

      type state = v State.t

      let state_empty = State.empty

      let state_add st l v = State.add l v st

      let state_add_if_undefined st l v =
        try
          ignore (State.find l st);
          Warn.fatal
            "Address %s non-unique in init state"
            (dump_location l)
        with Not_found -> State.add l v st

      let state_is_empty = State.is_empty

      let state_to_list st =
        List.rev (State.fold (fun l v k -> (l,v)::k) st [])

      let state_size st = State.fold (fun _ _ k -> 1+k) st 0

      let state_fold = State.fold
      let state_filter = State.filter

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

      let size_of_t = MiscParser.size_of I.V.Cst.Scalar.machsize

      let build_state bds =
        List.fold_left
          (fun st (loc,(t,v)) ->
            match t with
            | MiscParser.TyArray (array_prim,total_size) -> begin
              (* we expand v[3] = {a,b,c} into v+0 = a; v+1 = b; v+2 = c*)
              (* where 1 is the sizeof the underlying primitive type *)
              (* e.g uint64_t -> 8 bytes, so the above is v, v+8, v+16 *)
              let vs = match v with
              | I.V.Val (Constant.ConcreteVector (sz,vs))
                  when Misc.int_eq sz total_size -> vs
              | _ -> Warn.user_error "Unexpected scalar value %s, vector expected" (I.V.pp_v v) in
              let locval = match global loc with
              | Some x -> x
              | _ -> Warn.user_error "Non-global vector assignment in init" in
              let prim_sz = size_of_t array_prim in
              let nbytes = MachSize.nbytes prim_sz in
              let vs = List.mapi
                (fun i v ->
                  let open Constant in
                  let s = I.V.pp false locval in
                  let tag = None in
                  let cap = 0 in
                  let sym_data =
                    { name=s ;
                      tag=tag ;
                      cap=cap ;
                      offset=i*nbytes} in
                  Location_global
                    (I.V.Val (Symbolic sym_data)),
                  (MiscParser.Ty array_prim,I.V.cstToV v))
                vs in
              List.fold_left
                (fun st (loc,(_,v))-> state_add_if_undefined st loc v)
                st
                vs
            end
            (* if we have a value, store it *)
            | _ -> state_add_if_undefined st loc v)
          State.empty bds

      let build_concrete_state bds =
        List.fold_left
          (fun st (loc,v) ->
            State.add loc (I.V.intToV v) st)
          State.empty bds

      (* We might have accesses in the final state like v[2] *)
      (* this depends on the size of the vector types in the initial state *)
      (* e.g when uint64_t v, each elem is 8 bytes, so 2*8 is 16 bytes offset*)
      (* This function scales the offset from type information, *)
      (* Raises User_error, if not an array or pointer type or  *)
      (* in case of out of bounds access.                       *)
      let scale_array_reference t loc os =
        let sz_elt,n_elts =
          let open MiscParser in
          match t with
          | TyArray (t,sz) -> size_of_t t,sz
          | TyDefPointer -> MachSize.Word,1
          | Pointer t -> size_of_t t,1
          | _ ->
              Warn.user_error
                "Location %s of type %s is used as an array"
                (pp_location loc) (MiscParser.pp_run_type t) in
        if os < 0 || os >= n_elts then
          Warn.user_error
            "Out of bounds access on array %s" (pp_location loc) ;
        if os = 0 then loc
        else
          match loc with
          | Location_global (I.V.Val (Constant.Symbolic s)) ->
              let s =
                { s with Constant.offset = MachSize.nbytes sz_elt * os} in
              Location_global (I.V.Val (Constant.Symbolic s))
          | _ -> (* Excluded by parsing *)
              Warn.fatal "Location %s is ot global" (pp_location loc)

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

      (* Sizes *)

      type size_env = MachSize.sz StringMap.t

      let look_size env s = StringMap.safe_find MachSize.Word s env

      let look_size_location env loc =
        let open Constant in
        match global loc with
        | Some (I.V.Val (Symbolic {name=s;offset=0;_})) -> look_size env s
        | _ -> assert false

      let size_env_empty = StringMap.empty

      let misc_to_size ty =
        let maximal = I.V.Cst.Scalar.machsize in
        match ty with
        | MiscParser.TyDef ->
            MiscParser.size_of maximal "int"
        | MiscParser.Ty t|MiscParser.Atomic t
        | MiscParser.TyArray (t,_)
          ->
            MiscParser.size_of maximal t
        | MiscParser.Pointer _| MiscParser.TyDefPointer ->
            maximal

      let build_size_env bds =
        List.fold_left
          (fun m (loc,(t,_)) ->
            match loc with
            | Location_global a ->
                StringMap.add (I.V.as_symbol a) (misc_to_size t) m
            | _ -> m)
          size_env_empty bds

      (* Types *)

      type type_env = MiscParser.run_type LocMap.t

      let type_env_empty = LocMap.empty

      let build_type_env bds =
        List.fold_left
          (fun m (loc,(t,_)) -> LocMap.add loc t m)
          type_env_empty bds

      let look_type m loc =
        LocMap.safe_find MiscParser.TyDef loc m

      let loc_of_rloc tenv =
        let open ConstrGen in
        function
        | Loc loc -> loc
        | Deref (loc,o) ->
            let t = look_type tenv loc in
            scale_array_reference t loc o

      (* Final (include faults) *)
      module RState = RLocMap

      type rstate = v RState.t

      let rstate_to_list st =
        List.rev (RState.fold (fun l v k -> (l,v)::k) st [])

      let rstate_filter = RState.filter

      type final_state = rstate * FaultSet.t

      let pp_nice_rstate st delim pp_bd =
        let bds =
          RState.fold
            (fun l v k -> (pp_bd l v)::k)
            st [] in
        String.concat delim  (List.rev bds)

      let do_dump_rstate tr st =
        pp_nice_rstate st " "
          (fun l v ->
            ConstrGen.dump_rloc (do_dump_location tr) l ^
              "=" ^ I.V.pp C.hexa v ^";")

      let do_dump_final_state tr (st,flts) =
        let pp_st = do_dump_rstate tr st in
        if FaultSet.is_empty flts then pp_st
        else
          pp_st ^ " " ^ FaultSet.pp_str " " (fun f -> pp_fault f ^ ";") flts

      module StateSet =
        MySet.Make
          (struct
            type t = final_state

            let compare (st1,flt1) (st2,flt2) =
              match RState.compare I.V.compare st1 st2 with
              | 0 -> FaultSet.compare flt1 flt2
              | r -> r
          end)

      module Mixed (SZ : ByteSize.S) = struct

        let morello = C.variant Variant.Morello
        let byte = SZ.byte

        let endian = match C.endian with
        | None -> I.endian
        | Some e -> e

        let byte_sz =  MachSize.nbytes byte

        let mask = match byte_sz with
        | 1 -> "0xff"
        | 2 -> "0xffff"
        | 4 -> "0xffffffff"
        | 8 -> "0xffffffffffffffff"
        | _ ->
            Warn.user_error "Size cannot be %s in mixed-size mode"
              (MachSize.pp byte)

        let nshift = MachSize.nbits byte

        let nsz sz =
          let n = MachSize.nbytes sz in
          if n < byte_sz then
            Warn.fatal "Size mismatch %s smaller than %s\n"
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

        let rec recompose ds = match ds with
        | [] -> assert false
        | [d] -> d
        | d::ds ->
            let w = recompose ds in
            I.V.op Op.Or (I.V.op1 (Op.LeftShift nshift) w) d

        let look_in_state_mixed senv st loc =
          match undetermined_vars_in_loc loc with
          | Some _ ->
              (* if loc is not determined, then we cannot get its
                 content yet *)
              raise LocUndetermined
          | None ->
              let open Constant in
              match global loc with
              | Some (I.V.Val (Symbolic {name=s;offset=0;_}) as a)   ->
                  let sz = look_size senv s in
                  let eas = byte_eas sz a in
                  let vs = List.map (get_of_val st) eas in
                  let v = recompose vs in
                  if morello then
                    let ts = get_of_val st (I.V.op1 Op.CapaTagLoc a) in
                    I.V.op Op.CapaSetTag v ts
                  else v
              | _ ->
                  assert (not (is_global loc)) ;
                  get_in_state loc st


        let look_in_state =
          if is_mixed || morello then look_in_state_mixed
          else fun _senv -> look_address_in_state (* No need for size-env when sizes are ignored *)

        let look_in_state_rlocs st rloc =
          if is_mixed || morello then
            Warn.fatal "Mixed-size look_in_state_rloc not implemented"
          else
            try RState.find rloc st
            with Not_found -> assert false

(* Can be seen as performing two actions:
   1. Change rloc into actual locations
   2. Eliminate binding whose location is not listed in locs
 *)

        let reg_rlocs st =
          State.fold
            (fun loc _ k ->
              match loc with
              | Location_reg _ -> RLocSet.add (ConstrGen.Loc loc) k
              | Location_global _ -> k)
            st RLocSet.empty

        let add_reg_locs keep_regs st locs =
          if keep_regs then
            RLocSet.union (reg_rlocs st) locs
          else locs

        let state_restrict_locs_non_mixed keep_regs locs tenv _ st =
          let loc_of_rloc = loc_of_rloc tenv in
          RLocSet.fold
            (fun loc r ->
              RState.add loc (look_address_in_state st (loc_of_rloc loc)) r)
            (add_reg_locs keep_regs st locs) RState.empty

        let state_restrict_locs_mixed keep_regs locs tenv senv st =
          let loc_of_rloc = loc_of_rloc tenv in
          RLocSet.fold
            (fun loc r ->
              RState.add
                loc (look_in_state_mixed senv st (loc_of_rloc loc)) r)
            (add_reg_locs keep_regs st locs) RState.empty

        let state_restrict_locs  =
          if is_mixed || morello then state_restrict_locs_mixed
          else  state_restrict_locs_non_mixed

      end

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
