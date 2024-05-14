(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
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
  val get_val : arch_reg -> V.v -> V.v

  val fromto_of_instr : V.Cst.Instr.t -> (Label.Set.t * Label.Set.t) option

  module FaultType : FaultType.S
end

(** Output signature, functionalities added *)
module type S = sig



  val is_mixed : bool

  module I : I

  type global_loc = I.V.v
  type v = I.V.v

  type proc = Proc.t
  val pp_proc : proc -> string

  type program_order_index = int
  val pp_prog_order_index : program_order_index -> string

  val zero_po_index : program_order_index
  val next_po_index : program_order_index -> program_order_index

  include Location.S with type loc_reg := I.arch_reg and type loc_global := v

  type reg_state
  val reg_state_empty : reg_state
  val pp_reg_state : reg_state -> string

  (*********************************)
  (* Components of test structures *)
  (*********************************)

  (* Test structures represent programmes loaded in memory
   and ready to start, plus some items that describe
   the test, such as its name (cf. Test.mli) *)


  (* Code memory is a mapping from labels to sequences of instructions, too far from actual machine, maybe *)
  type instr = I.V.Cst.Instr.t
  type code = (int * instr) list

  val convert_if_imm_branch : int -> int -> int Label.Map.t -> int Label.Map.t -> instr -> instr

  (* Program loaded in memory *)
  type program = int Label.Map.t

  (* A starting address per proc *)
  type start_points = (proc * code * code option) list

  (* A mapping from code addresses to code *)
  type code_segment = (proc * code) IntMap.t

  (* A mapping from code addresses to sets of labels *)
  type entry_points = int -> Label.Set.t

  (* Constraints *)
  type prop =  (location,v,I.FaultType.t) ConstrGen.prop
  type constr = prop ConstrGen.constr

  (* Register contents (when known) X size of last load exclusive *)

  type ii_env =  {
      regs:reg_state;
      lx_sz:MachSize.sz option;
      fh_code:code option;
    }

  type inst_instance_id = {
      fetch_proc : proc; (* Fetching source *)
      proc       : proc; (* Current thread *)
      program_order_index   : program_order_index;
      inst : instr;
      labels : Label.Set.t; lbl2addr:program;
      addr : int;
      addr2v : string -> I.V.v;
      env : ii_env;
      in_handler : bool;
    }

  val inst_instance_compare :
      inst_instance_id -> inst_instance_id -> int

  val same_instruction :
      inst_instance_id -> inst_instance_id -> bool

  val pp_global : global_loc -> string
  val pp_location_old : location -> string

  val symbol : location -> Constant.symbol option
  val offset : location -> int option
  val symbolic_data : location -> Constant.symbolic_data option
  val of_symbolic_data : Constant.symbolic_data -> location

(* Extra for locations *)
  val maybev_to_location : MiscParser.maybev -> location
  val do_dump_location : (string -> string) -> location -> string
  val do_dump_location_no_brackets : (string -> string) -> location -> string
  val dump_location : location -> string

  val undetermined_vars_in_loc_opt : location -> I.V.v option
  val undetermined_vars_in_loc : location -> I.V.ValueSet.t
  val simplify_vars_in_loc : I.V.solution ->  location -> location
  val map_loc : (v -> v) -> location -> location

  val same_base_virt : location -> location -> bool
  val access_of_location_init : location -> Access.t

(**********)
(* Faults *)
(**********)
  include Fault.S with type loc_global := v and type fault_type := I.FaultType.t

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
  val map_state : (v -> v) -> state -> state

  val build_state : (location * (TestType.t * v)) list -> state
  val build_concrete_state : (location * int) list -> state

  val state_is_empty : state -> bool
  val state_add : state -> location -> v -> state
  val state_add_if_undefined  : state -> location -> v -> state
  val state_to_list : state -> (location * v) list
  val state_size : state -> int
  val state_fold : (location -> v -> 'a -> 'a) -> state -> 'a -> 'a
  val state_filter : (location -> bool) -> state -> state

  (* Exception raised when location is yet unknown *)
  exception LocUndetermined
  val look_address_in_state : state -> location -> v

(******************)
(* Register state *)
(******************)

  val build_reg_state : proc -> I.arch_reg list -> state -> reg_state
  val look_reg : I.arch_reg -> reg_state -> I.V.v option
  val set_reg : I.arch_reg -> v -> reg_state -> reg_state
  val kill_regs : I.arch_reg list -> reg_state -> reg_state
  val fold_reg_state : (I.arch_reg -> v -> 'a -> 'a) -> reg_state -> 'a -> 'a

(****************)
(* Environments *)
(****************)

  val size_of_t : string -> MachSize.sz
  val mem_access_size_of_t : TestType.t -> MachSize.sz
  val mask_type : TestType.t -> v -> v

  type size_env
  val size_env_empty : size_env
  val build_size_env : (location * (TestType.t * 'v)) list -> size_env
  val look_size : size_env -> string -> MachSize.sz
  val look_size_location : size_env -> location -> MachSize.sz

  type type_env
  val type_env_empty : type_env
  val build_type_env : (location * (TestType.t * 'v)) list -> type_env
  val look_type : type_env -> location -> TestType.t
  val look_rloc_type : type_env -> rlocation -> TestType.t
  val loc_of_rloc : type_env -> rlocation -> location
(* Expand array rlocation to locations of its elements *)
  val locs_of_rloc : type_env -> rlocation -> location list
(* Combine rlocs_of_rloc and value extraction from memory *)
  val val_of_rloc : (location -> v) -> type_env -> rlocation -> v

  (* Final state, our outcome *)
  type rstate
  val rstate_to_list : rstate -> (rlocation * v) list
  val rstate_filter : (rlocation -> bool) -> rstate -> rstate
  val debug_rstate : rstate -> string

  type final_state = rstate * FaultSet.t
  val do_dump_final_state :
    type_env -> FaultAtomSet.t ->
    (string -> string) -> final_state -> string

  (* Set of final states *)
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
    (* decompose effective address increasing order,
       first arg is starting offset *)
    val byte_indices :  int -> MachSize.sz -> int list
    (* decompose effective address increasing order, endianess order *)
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

end

module type Config = sig
  val verbose : int
  val texmacros : bool
  val hexa : bool
  val brackets : bool
  val variant : Variant.t -> bool
  val endian : Endian.t option
  val default_to_symb: bool
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

      let pp_global = I.V.pp C.hexa

      module LocArg =
        struct
          type arch_reg = I.arch_reg
          let pp_reg = I.pp_reg
          let reg_compare = I.reg_compare

          type arch_global = v
          let pp_global = pp_global
          let global_compare = I.V.compare
        end

      include Location.Make (LocArg)

      module RegOrd = struct
        type t = I.arch_reg
        let compare = I.reg_compare
      end

      module RegMap = MyMap.Make(RegOrd)

      type reg_state = I.V.v RegMap.t

      let reg_state_empty = RegMap.empty

      let pp_reg_state =
        RegMap.pp_str_delim "; "
          (fun r v -> Printf.sprintf "%s->%s" (I.pp_reg r) (I.V.pp_v v))

      (*********************************)
      (* Components of test structures *)
      (*********************************)

      (* Code memory is a mapping from globals locs, to instructions *)
      type instr = I.V.Cst.Instr.t
      type code = (int * instr) list

      (* This function is a default behaviour for all architectures.
         When variant -self is enabled, it fails trying to convert a branch
         instruction to a label into a branch-with-offset representation. *)
      let convert_if_imm_branch _ _ _ _ i =
        if C.variant Variant.Ifetch then
          Warn.fatal "Functionality %s not implemented for -variant self" "convert_if_imm_branch"
        else
          i

      (* Programm loaded in memory *)
      type program = int Label.Map.t

      (* A starting address per proc *)
      type start_points = (proc * code * code option) list

      (* Mapping from code addresses to code *)
      type code_segment = (proc * code) IntMap.t

      (* A mapping from code addresses to sets of labels *)
      type entry_points = int -> Label.Set.t

      (* Constraints *)
      type prop =  (location,v,I.FaultType.t) ConstrGen.prop
      type constr = prop ConstrGen.constr

      type ii_env =  {
          regs:reg_state;
          lx_sz:MachSize.sz option;
          fh_code:code option;
        }

      type inst_instance_id = {
          fetch_proc : proc;
          proc       : proc;
          program_order_index   : program_order_index;
          inst : instr;
          labels : Label.Set.t; lbl2addr : program;
          addr : int ;
          addr2v : string -> I.V.v;
          env : ii_env;
          in_handler : bool;
        }


      let inst_instance_compare i1 i2 = match Misc.int_compare i1.proc i2.proc with
      | 0 -> begin
            Misc.int_compare i1.program_order_index i2.program_order_index
        end
      | r -> r

      let same_instruction i1 i2 = i1.inst == i2.inst

      let symbol loc =
        let open Constant in
        match global loc with
        | Some (I.V.Val (Symbolic sym)) -> Some sym
        | _ -> None

      let offset loc =
        let open Constant in
        match symbol loc with
        | Some (Virtual si) -> Some si.offset
        | Some (Physical (_,o)) -> Some o
        | Some _ -> Some 0
        | None -> None

      let symbolic_data loc =
        let open Constant in
        match global loc with
        | Some (I.V.Val (Symbolic (Virtual sym))) -> Some sym
        | _ -> None

      let of_symbolic_data s =
        Location_global (I.V.Val (Constant.of_symbolic_data s))

      let maybev_to_location v = Location_global (I.V.maybevToV v)

      let do_brackets =
        if C.brackets then Printf.sprintf "[%s]"
        else fun s -> s

      let do_dump_location tr = function
        | Location_reg (proc,r) ->
            tr (string_of_int proc ^ ":" ^ I.pp_reg r)
        | Location_global a -> do_brackets (pp_global a)

      let dump_location = do_dump_location Misc.identity

      let do_dump_location_no_brackets tr = function
        | Location_reg (proc,r) ->
            tr (string_of_int proc ^ ":" ^ I.pp_reg r)
        | Location_global a -> pp_global a


      let do_pp_location do_brackets l = match l with
      | Location_reg (proc,r) ->
          let bodytext = string_of_int proc ^ ":" ^ I.pp_reg r in
          if C.texmacros
          then "\\asm{Proc " ^ bodytext ^ "}" else bodytext
      | Location_global a -> do_brackets (pp_global a)

(* This redefines pp_location from Location.Make ... *)
      let pp_location = do_pp_location do_brackets
      and pp_location_old = do_pp_location Misc.identity

      let some_undetermined_vars_in_loc l =  match l with
      | Location_reg _ ->  false
      | Location_global a -> not (I.V.is_var_determined a)

      let undetermined_vars_in_loc_opt l =  match l with
      | Location_reg _
      | Location_global (I.V.Val _) ->
          None
      | Location_global (I.V.Var _ as v) ->
          Some v

      let undetermined_vars_in_loc l =  match l with
      | Location_reg _ -> I.V.ValueSet.empty
      | Location_global a -> I.V.undetermined_vars a

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
      module FaultArg = struct
        include LocArg
        open Constant

(* Compare id in fault and other id, at least one id must be allowed in fault *)
        let same_sym_fault sym1 sym2 = match sym1,sym2 with
(* Both ids allowed in fault, compare *)
          |(Virtual {name=s1;_},Virtual {name=s2;_})
          |(System (PTE,s1),System (PTE,s2))
           -> Misc.string_eq s1 s2
(* One id allowed, the other on forbidden, does not match *)
          | (Virtual _,(System ((PTE|TLB|PTE2),_)|Physical _|TagAddr _))
          | ((TagAddr _|Physical _|System ((PTE|TLB|PTE2),_)),Virtual _)
          | (System (PTE,_),System ((TLB|PTE2),_))
          | (System ((TLB|PTE2),_),System (PTE,_))
          | ((Physical _|TagAddr _),System (PTE,_))
          | (System (PTE,_),(TagAddr _|Physical _))
            -> false
(* Both forbidden, failure *)
          | (TagAddr _|Physical _|System ((TLB|PTE2),_)),
            (TagAddr _|Physical _|System ((TLB|PTE2),_))
            ->
              Warn.fatal
                "Illegal id (%s or %s) in fault"
                (pp_symbol sym1) (pp_symbol sym2)

        let same_id_fault v1 v2 = match v1,v2 with
          | I.V.Val (Symbolic sym1), I.V.Val (Symbolic sym2)
            -> same_sym_fault sym1 sym2
          | I.V.Val (Constant.Label (_, l1)),I.V.Val (Constant.Label (_, l2))
            -> Misc.string_eq l1 l2
          | I.V.Val (Symbolic _), I.V.Val (Constant.Label (_, _))
          | I.V.Val (Constant.Label (_, _)), I.V.Val (Symbolic _)
            -> false
          | _,_
            ->
              Warn.fatal
                "Illegal value (%s or %s) in fault"
                (I.V.pp_v v1) (I.V.pp_v v2)

        type fault_type = I.FaultType.t
        let pp_fault_type = I.FaultType.pp
        let fault_type_compare = I.FaultType.compare
      end

      include Fault.Make(FaultArg)

      let same_base_virt loc1 loc2 =
        match loc1,loc2 with
        | Location_global v1,Location_global v2
          -> FaultArg.same_id_fault v1 v2
        |  _,_ -> false

      let access_of_location_init = function
        | Location_reg _ -> Access.REG
        | Location_global v -> I.V.access_of_value v

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

      let get_val loc v = match loc with
      | Location_reg (_,reg) -> I.get_val reg v
      | _ -> v

      let do_dump_state tr st =
        pp_nice_state st " "
          (fun l v -> do_dump_location tr l ^ "=" ^ I.V.pp C.hexa (get_val l v) ^";")

      let dump_state st = do_dump_state Misc.identity st

      let map_state f st = State.map f st
(******************)
(* Register state *)
(******************)

      let reg_default_value = I.V.zero

      let build_reg_state p defaults st =
        List.fold_right
          (fun r -> RegMap.add r reg_default_value)
          defaults RegMap.empty |>
        LocMap.fold
          (fun loc v k ->
            match loc with
            | Location_reg (q,r) when Proc.equal p q ->
               RegMap.add r v k
            | _ -> k)
          st

      let look_reg r st = RegMap.find_opt r st
      let set_reg r v st = RegMap.add r v st
      let kill_regs rs st =  List.fold_right RegMap.remove rs st
      let fold_reg_state = RegMap.fold

(****************)
(* Environments *)
(****************)

      let size_of_t = TestType.size_of I.V.Cst.Scalar.machsize

      let mem_access_size_of_t t =
        let open TestType in
        match t with
        | Atomic b|Ty b|TyArray (b,_) ->
           if b = "ins_t" then MachSize.Word (* Ok for most archs... *)
           else size_of_t b
        | TyDef -> size_of_t TestType.default
        | TyDefPointer|Pointer _ -> I.V.Cst.Scalar.machsize

      let mask_type t v =
        let sz = mem_access_size_of_t t in
        I.V.map_scalar (I.V.Cst.Scalar.mask sz) v

      let signed_of_t t =
        let open TestType in
        match t with
        | Atomic b|Ty b|TyArray (b,_) -> is_signed b
        | TyDef -> true
        | TyDefPointer|Pointer _ -> false

      let build_state bds =
        List.fold_left
          (fun st (loc,(t,v)) ->
            match t with
            | TestType.TyArray (array_prim,total_size) -> begin
              (* we expand v[3] = {a,b,c} into v+0 = a; v+1 = b; v+2 = c*)
              (* where 1 is the sizeof the underlying primitive type *)
              (* e.g uint64_t -> 8 bytes, so the above is v, v+8, v+16 *)
              let vs = match v with
                | I.V.Val (Constant.ConcreteVector vs) ->
                   if Misc.int_eq (List.length vs) total_size then
                     vs
                   else
                     Warn.user_error
                       "Vector size mismatch, %s (exepected size %d)\n"
                       (I.V.pp_v v) total_size
                | _ ->
                   Warn.user_error
                     "Unexpected scalar value %s, vector expected"
                     (I.V.pp_v v) in
              let locval = match global loc with
              | Some x -> x
              | _ -> Warn.user_error "Non-global vector assignment in init" in
              let prim_sz = size_of_t array_prim in
              let nbytes = MachSize.nbytes prim_sz in
              let vs = List.mapi
                (fun i v ->
                  let s = I.V.pp false locval in
                  let tag = None in
                  let cap = 0L in
                  let sym_data =
                    { Constant.name=s ;
                      tag=tag ;
                      cap=cap ;
                      offset=i*nbytes} in
                  of_symbolic_data sym_data,(TestType.Ty array_prim,I.V.cstToV v))
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

      let size_of_array t =
        let open TestType in
        match t with
        | TyArray (t,sz) -> Some (size_of_t t,sz)
        | TyDefPointer -> Some (MachSize.Word,1)
        | Pointer t -> Some (size_of_t t,1)
        | _ -> None
(* Fails with user error when t is not an array type
   or in case of out-of-bound access *)
      let scale_array_reference t loc os =
        let sz_elt,n_elts =
          match size_of_array t with
          | Some (a,b) -> (a,b)
          | None ->
             Warn.user_error
               "Location %s of type %s is used as an array"
               (pp_location_old loc) (TestType.pp t) in
        if os < 0 || os >= n_elts then
          Warn.user_error
            "Out of bounds access on array %s" (pp_location_old loc) ;
        if os = 0 then loc
        else
          match symbolic_data loc with
          | Some s ->
              let s =
                { s with Constant.offset = MachSize.nbytes sz_elt * os} in
              of_symbolic_data s
          | _ -> (* Excluded by parsing *)
              Warn.fatal "Location %s is not global" (pp_location_old loc)

(* To get protection against wandering undetermined locations,
   all loads from state are by this function *)
      exception LocUndetermined

      let get_in_state loc st =
        try get_val loc (State.find loc st)
        with Not_found ->
          let open Constant in
          if C.default_to_symb then I.V.fresh_var () else
          match loc with
          | Location_global (I.V.Var _)
          (* As called from look_address_in_state below *)
            -> assert false
          | Location_global (I.V.Val (Symbolic (System (PTE,s)))) ->
              I.V.Val (PteVal (I.V.Cst.PteVal.default s))
          | Location_global (I.V.Val (Symbolic (TagAddr _))) ->
              I.V.Val default_tag
          | Location_global
              (I.V.Val
                (Symbolic (System ((PTE2|TLB),_))))
            ->
              Warn.user_error
                "No default value defined for location %s\n"
                (pp_location loc)
          | Location_global
              (I.V.Val
                 (Concrete _|ConcreteVector _|ConcreteRecord _
                 |Label _|Instruction _|Frozen _
                 |Tag _|PteVal _))
            ->
              Warn.user_error
                "Very strange location (look_address) %s\n"
                (pp_location loc)
          | Location_global (I.V.Val (Symbolic (Virtual _|Physical _)))
          | Location_reg _ -> reg_default_value

      let get_of_val st a = State.safe_find I.V.zero (Location_global a) st

      let look_address_in_state st loc =
        if some_undetermined_vars_in_loc loc then
            (* if loc is not determined, then we cannot get its
               content yet *)
            raise LocUndetermined
        else get_in_state loc st

      (* Sizes *)

      type size_env = MachSize.sz StringMap.t

      let size_env_empty = StringMap.empty

      let look_size env s = StringMap.safe_find MachSize.Word s env

      let look_size_location env loc =
        match symbolic_data loc with
        | Some {Constant.name=s;_} -> look_size env s
        | _ -> assert false

      let build_size_env bds =
        List.fold_left
          (fun m (loc,(t,_)) ->
            match symbolic_data loc with
            | Some sym ->
                StringMap.add sym.Constant.name (mem_access_size_of_t t) m
            | _ -> m)
          size_env_empty bds

      (* Types *)
      type type_env = TestType.t LocMap.t

      let type_env_empty = LocMap.empty

      let build_type_env bds =
        List.fold_left
          (fun m (loc,(t,_)) -> LocMap.add loc t m)
          type_env_empty bds

      let look_type m loc = LocMap.safe_find TestType.TyDef loc m

      let look_rloc_type m rloc =
        let open ConstrGen in
        match rloc with
        | Loc loc -> look_type m loc
        | Deref (loc,_) ->
           let t = look_type m loc in
           TestType.Ty (TestType.get_array_primitive_ty t)

      let loc_of_rloc tenv =
        let open ConstrGen in
        function
        | Loc loc -> loc
        | Deref (loc,o) ->
            let t = look_type tenv loc in
            scale_array_reference t loc o

    let locs_of_rloc tenv rloc =
      let open ConstrGen in
      match rloc with
      | Loc loc ->
         begin
           let t = look_type tenv loc in
           match t with
           | TestType.TyArray (_,sz) ->
              let rec do_rec o =
                if o >= sz then []
                else
                 scale_array_reference t loc o::do_rec (o+1) in
              do_rec 0
           | _ -> [loc]
         end
      | Deref (loc,o) ->
          let t = look_type tenv loc in
          [scale_array_reference t loc o]

    let demote = function
      | I.V.Var _ as v -> v
      | I.V.Val c -> I.V.Val (Constant.map_scalar I.V.Cst.Scalar.demote c)

    let val_of_rloc look tenv rloc =
      match locs_of_rloc tenv rloc with
      | [loc] -> look loc |> demote
      | locs ->
         let cs =
           List.map
             (fun loc ->
               match look loc|> demote  with
               | I.V.Val c -> c
               | I.V.Var v -> I.V.freeze v)
             locs in
         I.V.Val (Constant.ConcreteVector cs)


      (* Final (include faults) *)
      module RState = RLocMap

      type rstate = v RState.t

      let rstate_to_list st =
        List.rev (RState.fold (fun l v k -> (l,v)::k) st [])

      let rstate_filter = RState.filter

      let debug_rstate rs =
        let bds = rstate_to_list rs in
        List.map
          (fun (loc,v) ->
            Printf.sprintf "%s -> %s"
              (pp_rlocation loc)
              (I.V.pp true v))
             bds |> String.concat "; "



      type final_state = rstate * FaultSet.t

      let pp_nice_rstate st delim pp_bd =
        let bds =
          RState.fold
            (fun l v k -> (pp_bd l v)::k)
            st [] in
        String.concat delim  (List.rev bds)


      (* Scalars are always for the same effective type,
         (V.Cst.Scalar.t). For printing a scalar of "external"
         type t, the value itself is changed, masking for unsigned
         types, sign extension for signed types.
         However, in hexadecimal all scalars are seen
         as unsigned. *)

      let cast_for_pp_with_base b sc =
        let sz = size_of_t b in
        if sz = I.V.Cst.Scalar.machsize then sc
        else if TestType.is_signed b && not C.hexa then I.V.Cst.Scalar.sxt sz sc
        else I.V.Cst.Scalar.mask sz sc

      let cast_for_pp_with_type t =
        let open TestType in
        match t with
        | Atomic b|Ty b|TyArray (b,_) -> cast_for_pp_with_base b
        | TyDef -> cast_for_pp_with_base TestType.default
        | Pointer _|TyDefPointer -> Misc.identity

      let pp_typed t v =
        let max_unsigned =
           MachSize.equal (mem_access_size_of_t t) I.V.Cst.Scalar.machsize
          && not (signed_of_t t) in
        let v = I.V.map_scalar (cast_for_pp_with_type t) v in
        if max_unsigned then I.V.pp_unsigned C.hexa v
        else I.V.pp C.hexa v

      let do_dump_rstate tenv tr st =
        pp_nice_rstate st " "
          (fun l v ->
            let t = look_rloc_type tenv l in
            let dump_loc =
              let open ConstrGen  in
              match l,t with
              | (Deref _,_)
              | (_,TestType.TyArray _)
                ->
                 do_dump_location_no_brackets
              | _,_ ->
                 do_dump_location in
            ConstrGen.dump_rloc (dump_loc tr) l ^
              "=" ^ pp_typed t v ^";")

      let do_dump_final_state tenv fobs tr (st,flts) =
        let pp_st = do_dump_rstate tenv tr st in
        if FaultSet.is_empty flts && FaultAtomSet.is_empty fobs then pp_st
        else
          let noflts =
            FaultAtomSet.fold
              (fun (((p,lbl),loc,ftype) as f0) k ->
                if
                  FaultSet.exists (fun f -> check_one_fatom f f0) flts
                then k
                else
                  let tr_lbl = match lbl with
                    | None -> Label.Set.empty
                    | Some lbl -> Label.Set.singleton lbl in
                  (" ~" ^ pp_fault (((p,tr_lbl),loc,ftype,None)) ^ ";")::k)
              fobs [] in
          let flts =
            if !Opts.dumpallfaults then
              flts
            else
              FaultSet.filter
                (fun f -> FaultAtomSet.exists
                    (fun f0 -> check_one_fatom f f0) fobs)
                flts in
          pp_st ^ " " ^
          FaultSet.pp_str " "  (fun f -> pp_fault f ^ ";")  flts ^
          String.concat "" noflts

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

        let byte_indices o sz =
          let kmax = nsz sz in
          let rec do_rec k =
            if k >= kmax then []
            else
              let ds = do_rec (k+1) in
              let d = k*byte_sz in
              o+d::ds in
          o::do_rec 1

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
            if k <= 1 then [I.V.op1 (Op.AndK mask) v]
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
          if some_undetermined_vars_in_loc loc then
            (* if loc is not determined, then we cannot get its
                 content yet *)
              raise LocUndetermined
          else
              let open Constant in
              match loc with
              | Location_global
                (I.V.Val (Symbolic (Virtual {name=s; offset=_;_})) as a)
                ->
                  let sz = look_size senv s in
                  let eas = byte_eas sz a in
                  let vs = List.map (get_of_val st) eas in
                  let v = recompose vs in
                  if morello then
                    let ts = get_of_val st (I.V.op1 Op.CapaTagLoc a) in
                    I.V.op Op.CapaSetTag v ts
                  else v
              | Location_global (I.V.Val (Symbolic (TagAddr _))) ->
                 get_in_state loc st
              | _ ->
                 (* No mixed variant combination other than mte and morello *)
                 if is_global loc then begin
                   Warn.user_error "Cannot handle %s in mixed-size mode"
                     (pp_location loc)
                 end ;
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

        let do_state_restrict look keep_regs locs tenv st =
          RLocSet.fold
            (fun rloc r ->
              let v = val_of_rloc (look st) tenv rloc in
              RState.add rloc v r)
            (add_reg_locs keep_regs st locs) RState.empty


        let state_restrict_locs_non_mixed keep_regs locs tenv _ st =
          do_state_restrict look_address_in_state keep_regs locs tenv st

        let state_restrict_locs_mixed keep_regs locs tenv senv st =
          do_state_restrict (look_in_state_mixed senv) keep_regs locs tenv st

        let state_restrict_locs  =
          if is_mixed || morello then state_restrict_locs_mixed
          else  state_restrict_locs_non_mixed

      end

    end
