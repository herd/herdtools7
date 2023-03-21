(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique, ARM Ltd and the authors. All rights reserved.            *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

module type Config = sig
  val memory : Memory.t
  val preload : Preload.t
  val mode : Mode.t
  val kind : bool
  val hexa : bool
  val exit_cond : bool
  val have_fault_handler : bool
  val do_stats : bool
  val sysarch : Archs.System.t
  val c11 : bool
  val variant : Variant_litmus.t -> bool
  val ascall : bool
end

let no_timebase_error sysarch =
  match sysarch with
  | #Archs.System.arch ->
      Warn.user_error
        "No timebase for arch %s" (Archs.System.pp sysarch)
  | `Unknown ->
      Warn.user_error
        "Timebase not available for C without option -carch <arch>"

type stat =
    { tags : string list ;
      name : string ;
      max : string; tag : string;
      process : string -> string; }

module G = Global_litmus

let type_name loc =  Printf.sprintf "%s_t" loc

let pp_global_env ge =
  String.concat ","
    (List.map
       (fun (x,t) -> sprintf "<%s,%s>" (G.pp x) (CType.dump t))
       ge)

open CType

let dump_global_type loc t = match t with
| Array _ -> type_name loc
| _ ->  CType.dump t


let rec nitems t = match t with
| Array (_,sz) -> sz
| Volatile t|Atomic t|Const t -> nitems t
| Base _|Pointer _ -> 1

let dump_fatom_tag d ((p,lbl),v,_) =
  sprintf "fault_P%d%s%s" p
    (match lbl with None -> "" | Some lbl -> "_" ^ lbl)
    (match v with
     | None -> ""
     | Some v -> "_" ^ d v)

let data_symb_id s = sprintf "DATA_SYMB_ID_%s" (String.uppercase_ascii s)
let instr_symb_id s = sprintf "INSTR_SYMB_ID_%s" (String.uppercase_ascii s)
let fault_id s = sprintf "Fault%s" (Misc.to_c_name s)

module PteValUtil(P:PteVal.S) = struct

  let dump_pteval_flags s p =
    match P.as_flags p with
    | Some msk ->
       sprintf "litmus_set_pte_flags(%s,%s)" s msk
    | None -> s
end

module type PConfig = sig
  val avail : int option
  val mode : Mode.t
  val affinity : Affinity.t
  val smt : int
  val smtmode : Smt.t
end

module Param(Cfg:PConfig) = struct

  let do_affinity =
    let open Affinity in
    match Cfg.affinity with
    | Affinity.No -> false
    | Incr _|Random|Custom|Scan -> true

  let do_cores =
    do_affinity
    &&  Cfg.smt >= 1
    &&
      (match Cfg.smtmode with
       | Smt.No -> false
       | Smt.Seq|Smt.End -> true)

  let mk_dsa n =
    let open Mode in
    match Cfg.mode with
    | PreSi | Kvm -> true
    | Std ->
       do_cores
       && (match Cfg.avail with Some a -> a >= n | None -> false)

end

module Make
    (Cfg:Config)
    (P:sig type code end)
    (A:Arch_litmus.Base)
    (T:Test_litmus.S with
       type instruction = A.instruction and type P.code = P.code and
       module A = A and module FaultType = A.FaultType) : sig

(* Skeleton utilities, useful for Skel and PreSi *)

(* Info *)
      val get_info : string -> T.t -> string option
      val get_prefetch_info : T.t -> string

(* Typing utilities *)
      type env
      val pp_env : env -> string
      val build_env : T.t -> env
      val find_type : A.location -> env -> CType.t
      val find_rloc_type : A.rlocation -> env -> CType.t
      (* Convert constant to match location type *)
      val cast_constant : env -> A.rlocation -> A.V.v -> A.V.v
      val find_type_align : string -> env -> CType.t option
      val is_aligned : string -> env -> bool
      (* Maximal alignment constraint in bytes, 0 if none *)
      val max_align : T.t -> int
      val dump_mem_type : string -> CType.t -> env -> string

      val select_proc : int -> env -> CType.t A.RegMap.t
      val select_global : env -> (string * CType.t) list
      val select_aligned : env -> (string * CType.t) list

      val select_by_type : (CType.t -> bool) -> env -> A.location list

(* Some dumping stuff *)
      val register_type : 'loc ->  CType.t -> CType.t
      val fmt_outcome_as_list :
        T.t -> (CType.base -> string) -> string -> A.RLocSet.t -> env
        -> (string * string list) list
      val fmt_outcome : T.t -> (CType.base -> string) -> A.RLocSet.t -> env -> string
      val fmt_faults : (A.V.v, A.FaultType.t) Fault.atom list -> string

(* Globals *)
      exception NotGlobal
      val tr_global : A.rlocation -> Global_litmus.displayed

(* Locations *)
      val get_displayed_locs : T.t -> A.RLocSet.t
      val get_displayed_globals : T.t -> Global_litmus.DisplayedSet.t
      val get_displayed_ptes : T.t -> StringSet.t
      val get_observed_locs : T.t -> A.RLocSet.t
      val get_observed_globals : T.t -> Global_litmus.DisplayedSet.t
      val get_stabilized : T.t -> StringSet.t
      val is_ptr : A.location -> env -> bool
      val is_rloc_ptr : A.rlocation -> env -> bool
      val is_rloc_label : A.rlocation -> env -> bool
      val ptr_in_outs : env -> T.t -> bool
      val is_pte : A.location -> env -> bool
      val is_rloc_pte : A.rlocation -> env -> bool
      val is_rloc_parel1 : A.rlocation -> env -> bool
      val pte_in_outs : env -> T.t -> bool
      val parel1_in_outs : env -> T.t -> bool
      val ptr_pte_in_outs : env -> T.t -> bool
      val instr_in_outs : env -> T.t -> bool
      val label_in_outs : env -> T.t -> bool
      val get_faults : T.t -> (A.V.v, A.FaultType.t) Fault.atom list
      val find_label_offset : Proc.t -> string -> T.t -> int

(* Instructions *)
      val do_store : CType.t -> string -> string -> string
      val do_load : CType.t -> string -> string

(* Condition *)
      val pp_cond : T.t -> string

(* Dump stuff *)
      module Dump : functor (O:Indent.S) -> functor(EPF:EmitPrintf.S) -> sig
        (* Some small dump functions common std/presi *)

        val dump_mbar_def : unit -> unit

       (* Dump (typedef) array types, boolean argument commands
           also dumping types used for alignment. *)
        val dump_vars_types : bool -> T.t -> unit

       (* Dump (typedef) array register type *)
        val dump_array_typedefs : T.t -> unit

        (* Dump definition of struct fields that point in code*)
        val define_label_fields : Label.Full.Set.t -> unit

        (* Dump initialisation code of label constants,
           first argument is a pointer to struct *)
        val initialise_labels : string -> Label.Full.Set.t -> unit

        (* Dump definition of label offsets, _i.e_ offset from code start *)
        val define_label_offsets : env -> T.t -> unit

        (* Dump definitions relating to labels in the post-condition *)
        val dump_label_defs : Label.Full.full list -> unit

        (* Dump functions relating to labels in the post-condition *)
        val dump_label_funcs : bool -> Label.Full.full list -> int -> unit

        (* Same output as shell script in (normal) shell driver mode *)
        val prelude : Name.t -> T.t -> unit

        (* Dump results *)
        val postlude :
            Name.t -> T.t -> Affi.t option -> bool ->
              stat list -> unit

        (* Dump functions that return relevant opcodes *)
        val dump_getinstrs : T.t -> unit

        (* Initialise relevant opcode glogal variables *)
        val dump_init_getinstrs : T.t -> unit

        (* Dump function to translate back opcodes into instructions *)
        val dump_opcode : env -> T.t -> unit

       (* Dump topology-definitions as renaming of external ones *)
        val dump_topology_external : int -> unit
end

    end = struct

      let dbg = false

      module G = Global_litmus

(* Info *)
      let get_info key test =
        try Some (List.assoc key test.T.info)
        with Not_found -> None

      let get_prefetch_info test = match get_info "Prefetch" test with
      | Some i -> i
      | None -> ""

(* Typing stuff *)


      type env = (* Second component list aligned locations *)
          CType.t A.LocMap.t * CType.t StringMap.t

      let pp_env (env,_m) =
        A.LocMap.pp_str
          (fun loc t ->
            sprintf "%s -> %s" (A.pp_location loc) (CType.dump t))
          env

      let build_env test = test.T.type_env

      let find_type loc (env,_) =
        try A.LocMap.find loc env
        with Not_found -> Compile.base

      let find_rloc_type rloc (env,_ as p) =
        let open ConstrGen in
        match rloc with
        | Loc loc -> find_type loc p
        | Deref (loc,_) ->
            begin
              try CType.element_type (A.LocMap.find loc env)
              with Not_found -> Compile.base
            end

      let do_mask tr sz v = match sz with
        | None -> v
        | Some sz ->
           Constant.map_scalar (tr sz) v

      let cast_constant env loc v =
        let t = find_rloc_type loc env in
        let sz_t = CType.base_size t in
        let mask_ok =
          match sz_t with
          | None -> false
          | Some sz_t ->
              MachSize.compare sz_t A.V.Scalar.machsize <= 0 in
        if mask_ok then
          do_mask
            (if CType.signed t && not Cfg.hexa then A.V.Scalar.sxt
             else A.V.Scalar.mask)
            sz_t
            v
        else v

      let is_aligned loc (_,env) =
        try ignore (StringMap.find loc env) ; true with Not_found -> false

      let is_not_ins_ptr a env =
        let loc = A.Location_global (Global_litmus.Addr a) in
        let t = find_type loc env in
        not (CType.is_ins_ptr_t t)

      let nbytes t =
        let sz =
          match CType.base_size t with
          | Some sz -> sz
          | None -> MachSize.Quad (* Largest available *) in
        MachSize.nbytes sz

        let max_align test =
          let _,env = build_env test in
          StringMap.fold
            (fun _ t k ->
              let sz =
                match t with
                  | CType.Array (t,sz) -> sz*nbytes (CType.Base t)
                  | _ -> nbytes t in
              Misc.max_int sz k)
            env 0

      let dump_mem_type loc t env =
        if is_aligned loc env then type_name loc
        else dump_global_type loc t

      let do_find_type_align loc env =
        try Some (StringMap.find loc env)
        with Not_found -> None

      let find_type_align loc (_,env) = do_find_type_align loc env

      let select_types_reg f env =
        A.LocMap.fold
          (fun loc t k -> match f loc with
          | Some r -> A.RegMap.add r t k
          | None -> k)
          env A.RegMap.empty

      let select_proc (p:int) (env,_) =
        select_types_reg
          (function
            | A.Location_reg (q,reg) when Proc.equal p q -> Some reg
            | A.Location_global _ | A.Location_reg _  -> None)
          env

      let select_types f (env,_) =
        A.LocMap.fold
          (fun loc t k -> match f loc with
          | Some r -> (r,t)::k
          | None -> k)
          env []

      let select_global env =
        select_types
          (function
            | A.Location_reg _|A.Location_global (G.Pte _|G.Phy _) -> None
            | A.Location_global (G.Addr a) -> Some a)
          env

      let select_aligned env =
        select_types
          (function
            | A.Location_reg _|A.Location_global (G.Pte _|G.Phy _) -> None
            | A.Location_global (G.Addr loc) ->
                if is_aligned loc env then Some loc else None)
          env

      let select_by_type f (env,_) =
        A.LocMap.fold
          (fun loc t k -> match f t with
          | true -> loc::k
          | false -> k)
          env []

      let tr_out test = OutMapping.info_to_tr test.T.info

      let pp_loc tr_out loc =  match loc with
        | A.Location_reg (proc,reg) ->
           tr_out (sprintf "%d:%s" proc (A.pp_reg reg))
        | A.Location_global s -> G.pp s

      let pp_loc_brk tr_out loc =  match loc with
        | A.Location_reg (proc,reg) ->
           tr_out (sprintf "%d:%s" proc (A.pp_reg reg))
        | A.Location_global s -> sprintf "[%s]" (G.pp s)

      let pp_rloc tr_out t rloc =
        let open ConstrGen in
        match rloc with
        | Loc loc ->
           let pp_loc =
             match t with
             | CType.Array _ -> pp_loc
             | _ -> pp_loc_brk in
           pp_loc tr_out loc
        | Deref (loc,i) -> sprintf "%s[%d]" (pp_loc tr_out loc) i

      let register_type _loc t = t (* Systematically follow given type *)

      let fmt_outcome_as_list test pp_fmt_base fmt_label rlocs env =
        let tr_out = tr_out test in
        let rec pp_fmt t = match t with
        | CType.Pointer t when CType.is_ins_t t -> [fmt_label]
        | CType.Pointer _ -> ["%s"]
        | CType.Base "pteval_t" ->
            ["("; "oa:%s";  ", af:%d"; ", db:%d";
             ", dbm:%d"; ", valid:%d"; ", el0:%d"; ")"]
        | CType.Base "parel1_t" ->
            ["("; "oa:%s";  ", f:%d"; ")"]
        | CType.Base t -> [pp_fmt_base t]
        | CType.Atomic t|CType.Volatile t|CType.Const t-> pp_fmt t
        | CType.Array (t,sz) ->
            let fmt_elt = pp_fmt_base t in
            let fmts = Misc.replicate sz fmt_elt in
            let fmt = String.concat "," fmts in
            [sprintf "{%s}" fmt] in
        A.RLocSet.map_list
          (fun rloc ->
            let t = find_rloc_type rloc env in
            let pp1 = pp_rloc tr_out t rloc
            and pp2 = pp_fmt (register_type rloc t) in
            (pp1,pp2))
         rlocs

      let fmt_outcome test pp_fmt_base locs env =
        let pps = fmt_outcome_as_list test pp_fmt_base {|label:\"P%s\"|} locs env in
        String.concat " "
          (List.map
             (fun (p1,p2) -> sprintf "%s=%s;" p1 (String.concat "" p2))
             pps)

      let fmt_faults fs =
        String.concat ""
          (List.map
             (fun f -> sprintf " %s%s;" "%s" (Fault.pp_fatom A.V.pp_v A.FaultType.pp f))
             fs)

(* Locations *)
      let get_displayed_locs t =
        A.RLocSet.union
          (T.C.rlocations t.T.condition)
          (A.RLocSet.of_list t.T.flocs)

      exception NotGlobal

      let tr_global =
        let open ConstrGen in
        function
        | Loc (A.Location_global (G.Addr a)) -> Loc a
        | Deref (A.Location_global (G.Addr a),k) -> Deref (a,k)
        | _ -> raise NotGlobal

      let filter_globals rlocs =
        A.RLocSet.fold
          (fun a k ->
            try G.DisplayedSet.add (tr_global a) k
            with NotGlobal -> k)
          rlocs G.DisplayedSet.empty

      let filter_ptes locs =
        A.RLocSet.fold
          (fun a k -> match ConstrGen.loc_of_rloc a with
          | A.Location_global (G.Pte a) ->  StringSet.add a k
          | A.Location_reg _
          | A.Location_global (G.Phy _|G.Addr _)
            -> k)
          locs StringSet.empty

      let get_displayed_globals t = filter_globals (get_displayed_locs t)

      let get_displayed_ptes t = filter_ptes (get_displayed_locs t)

      let get_observed_locs t =
        let locs =  get_displayed_locs t in
        match t.T.filter with
        | None ->  get_displayed_locs t
        | Some filter ->
            A.RLocSet.union locs (T.C.rlocations_prop filter)

      let get_observed_globals t =  filter_globals (get_observed_locs t)

      let get_stabilized t =
        let rlocs = get_observed_globals t in
        let env = build_env t in
        G.DisplayedSet.fold
          (fun a k ->
            let open ConstrGen in
            match a with
            | Loc a when not (is_aligned a env) && is_not_ins_ptr a env ->
               (* Stabilisaton is not checked for
                  - Non aligned items
                  - Code label
                  Mosty because it would be too complex and that
                  stabilisation check is not that useful.
                *)
               StringSet.add a k
            | Loc _|Deref _ -> k)
          rlocs StringSet.empty

      let is_ptr loc env  =
        let t = find_type loc env in
        CType.is_ptr t

      let is_rloc_label loc env  =
        let t = find_rloc_type loc env in
        CType.is_ins_ptr_t t

      let is_rloc_ptr loc env  =
        let t = find_rloc_type loc env in
        CType.is_ptr t

      let ptr_in_outs env test =
        let locs = get_displayed_locs test in
        if dbg then Printf.eprintf "locs={%s}\n" (A.RLocSet.pp_str "," A.pp_rlocation locs) ;
        A.RLocSet.exists
          (fun loc -> (is_rloc_ptr loc env) && not (is_rloc_label loc env)) locs

      let is_pte loc env =
        let t = find_type loc env in
        CType.is_pte t

      let is_rloc_pte loc env =
        let t = find_rloc_type loc env in
        CType.is_pte t

      let is_rloc_parel1 loc env =
        let t = find_rloc_type loc env in
        CType.is_parel1 t

      let pte_in_outs env test =
        let locs = get_displayed_locs test in
        A.RLocSet.exists (fun loc -> is_rloc_pte loc env) locs

      let parel1_in_outs env test =
        let locs = get_displayed_locs test in
        A.RLocSet.exists (fun loc -> is_rloc_parel1 loc env) locs

      let is_ptr_pte loc env =
        let t = find_rloc_type loc env in
        CType.is_ptr t || CType.is_pte t

      let ptr_pte_in_outs env test =
        let locs = get_displayed_locs test in
        A.RLocSet.exists (fun loc ->is_ptr_pte loc env) locs

      let is_instr loc env =
        let t = find_rloc_type loc env in
        CType.is_ins_t t

      let instr_in_outs env test =
        let locs = get_displayed_locs test in
        A.RLocSet.exists (fun loc -> is_instr loc env) locs

      let label_in_outs env t =
        let locs = get_displayed_locs t in
        A.RLocSet.exists (fun loc -> is_rloc_label loc env)
          locs

      let get_faults test =
        let inc = T.C.get_faults test.T.condition
        and inf = test.T.ffaults in
        inc@inf

      let find_label_offset p lbl test =
        try
          T.find_offset_out p lbl test
        with Not_found ->
          let v = Constant.mk_sym_virtual_label p lbl in
          Warn.user_error "Non-existant label %s" (A.V.pp_v v)

(* Instructions *)
      let do_store t loc v =
        if CType.is_atomic t then
          sprintf "atomic_store_explicit(&%s,%s,memory_order_relaxed)" loc v
        else
          sprintf "%s = %s" loc v

      let do_load t loc =
        if CType.is_atomic t then
          sprintf "atomic_load_explicit(&%s,memory_order_relaxed)" loc
        else loc


(* Dump *)
      open ConstrGen

      let pp_atom tr_out a =
        let pp_loc loc = tr_out (A.pp_location loc)
        and pp_loc_brk loc = tr_out (A.pp_location_brk loc)
        and pp_v v = A.V.pp Cfg.hexa v
        and pp_ft = A.FaultType.pp in
        ConstrGen.dump_atom pp_loc pp_loc_brk pp_v pp_ft a

      let pp_cond test =
        let tr_out = tr_out test in
        ConstrGen.constraints_to_string (pp_atom tr_out) test.T.condition

(* Instructions as values *)

      let get_instrs_init t =
        let open Constant in
        List.fold_left
          (fun k (_,v) ->
            match v with
            | Instruction i -> A.V.Instr.Set.add i k
            | _ -> k)
          A.V.Instr.Set.empty t.T.init

      let get_instrs_final t = T.C.get_instrs t.T.condition

      let nop_set =
        match A.nop with
        | None -> A.V.Instr.Set.empty
        | Some nop -> A.V.Instr.Set.singleton nop

      let get_instrs_others t =
        A.V.Instr.Set.union3
          (get_instrs_init t) (get_instrs_final t)
          (if Cfg.variant Variant_litmus.Self then
            A.V.Instr.Set.of_list A.GetInstr.self_instrs
          else nop_set)

      let all_instrs t =
        let from_code = T.from_labels t
        and from_others = get_instrs_others t in
        from_code,from_others

      module Dump (O:Indent.S) (EPF:EmitPrintf.S) = struct

        let dump_mbar_def () =
          if Cfg.c11 && Cfg.sysarch = `Unknown then begin
            O.o "inline static void mbar(void) {" ;
            O.oi "atomic_thread_fence(memory_order_seq_cst);";
            O.o "}" ;
          end else
            let module Insert =  ObjUtil.Insert(Cfg) in
            Insert.insert O.o "mbar.c"

        let dump_array_typedefs test =
          let iter_outs f proc = List.iter (f proc) in
          let iter_all_outs f test =
            List.iter
              (fun (proc,(_,(outs,_))) -> iter_outs f proc outs)
              test.T.code in
          iter_all_outs
            (fun _ (_,t) ->
              match t with
              | CType.Array (t',sz) ->
                O.f "typedef %s %s[%i];" t' (CType.dump t) sz
              | _ -> ())
              test

        let dump_vars_types dump_align test =
          let _,env = build_env test in
          let globs = test.T.globals in
          List.iter
            (fun (s,t) -> match t with
            | CType.Array (t,sz) ->
                O.f "typedef %s %s[%d];" t (type_name s) sz
            | _ ->
               if dump_align then
                begin match do_find_type_align s env with
                | None -> ()
                | Some (CType.Array (t,sz)) ->
                    O.f "typedef %s %s[%i];" t (type_name s) sz
                | Some (CType.Base t) ->
                    O.f "typedef %s %s;" t (type_name s)
                | Some _ -> assert false
                end)
            globs ;
          begin match globs with _::_ -> O.o "" | [] -> () end ;
          ()

(* Definition of label constant fields *)
        let define_label_fields lbls =
          if not (Label.Full.Set.is_empty lbls) then begin
            let pp =
              Label.Full.Set.pp_str ","
                (fun (p,lbl) -> "*" ^ OutUtils.fmt_lbl_var p lbl)
                lbls in
            O.fi "ins_t %s;"pp
          end

        let check_ascall () =
          if not (Cfg.ascall) then
            Warn.user_error
              "Use option `-ascall true` for this test"

(* Label constant initialisation *)
        let initialise_labels ptr label_init =
          let open OutUtils in
          let procs =
            Label.Full.Set.fold
              (fun (p,_) k -> IntSet.add p k)
              label_init IntSet.empty in
          IntSet.iter
            (fun p ->
              check_ascall () ;
              O.fi "size_t %s = prelude_size((ins_t *)code%i);"
                (fmt_prelude p) p)
            procs ;
          Label.Full.Set.iter
            (fun (p,lbl) ->
              O.fi
                "%s->%s = ((ins_t *)code%i)+%s+%s;" ptr
                (fmt_lbl_var p lbl) p
                (fmt_prelude p) (fmt_lbl_offset p lbl))
            label_init ;
          ()

(* Output offset of labels *)
        let define_label_offsets env test  =
          let lbls =
            (* If printing labels, we need all offsets *)
            if label_in_outs env test then
              T.all_labels test
            else
            (* Otherwise, initial values are enough *)
              T.get_init_labels test |> Label.Full.Set.elements in
          if Misc.consp lbls then begin
            let find p lbl = find_label_offset p lbl test in
            List.iter
              (fun (p,lbl) ->
                let off = find p lbl in
                O.f "static const size_t %s = %i;"
                  (OutUtils.fmt_lbl_offset p lbl) off)
              lbls ;
            O.o ""
          end

        let dump_label_defs lbls =
          O.f "#define %-25s 0" (instr_symb_id "UNKNOWN") ;
          (* Define indices for labels *)
          List.iteri
            (fun i (p,lbl) ->
               let flbl = OutUtils.fmt_lbl_var p lbl in
               O.f "#define %-25s  %d" (instr_symb_id flbl) (i + 1))
            lbls ;
          O.o "" ;
          O.f "static const char *instr_symb_name[] = {" ;
          O.oi "\"UNKNOWN\"," ;
          (* Define names for inst symbols *)
          List.iter (fun (p,lbl) -> O.fi "\"%d:%s\","p lbl) lbls ;
          O.o "};" ;
          O.o ""

        let dump_label_funcs_skel do_self lbls nprocs =
          if do_self then
            O.o "static int get_instr_symb_id(ctx_t *ctx, ins_t* ins, int i) {"
          else
            O.o "static int get_instr_symb_id(ctx_t *ctx, ins_t* ins) {" ;
          for p=0 to nprocs - 1 do
            if List.exists (fun (p1,_) -> p = p1) lbls then begin
              let code = OutUtils.fmt_code p in
              let prelude = OutUtils.fmt_prelude p in
              O.fi "size_t %s = prelude_size((ins_t *)%s);" prelude code ;
              if do_self then
                let sz = OutUtils.fmt_code_size p in
                O.fi "ins_t *_%s = &ctx->%s[ctx->%s * i];" code code sz
              else
                O.fi "ins_t *_%s = (ins_t *)%s;" code code
            end
          done ;
          List.iter
            (fun (p,lbl) ->
               let lbl_off = OutUtils.fmt_lbl_offset p lbl in
               let code = OutUtils.fmt_code p in
               let prelude = OutUtils.fmt_prelude p in
               O.fi "if (ins == _%s + %s + %s)" code prelude lbl_off ;
               let flbl = (OutUtils.fmt_lbl_var p lbl) in
               O.fii "return %s;" (instr_symb_id flbl))
            lbls ;
          O.fi "return %s;" (instr_symb_id "UNKNOWN") ;
          O.o "};" ;
          O.o ""

        let dump_label_funcs_presi _do_self lbls _nprocs =
          O.o "static int get_instr_symb_id(labels_t *lbls, ins_t* pc) {" ;
          List.iter
            (fun (p,lbl) ->
               let flbl = (OutUtils.fmt_lbl_var p lbl) in
               O.fi "if (pc == lbls->%s)" flbl ;
               O.fii "return %s;" (instr_symb_id flbl))
            lbls ;
          O.fi "return %s;" (instr_symb_id "UNKNOWN") ;
          O.o "};" ;
          O.o ""

        let dump_label_funcs =
          match Cfg.mode with
          | Mode.Std ->
            dump_label_funcs_skel
          | Mode.PreSi|Mode.Kvm ->
            dump_label_funcs_presi

        open Preload

        let prelude doc test =
          O.o "#ifdef ASS" ;
          O.o "static void ass(FILE *out) { }" ;
          O.o "#else" ;
          O.f "#include \"%s\"" (MyName.outname doc.Name.file ".h") ;
          O.o "#endif" ;
          O.o "" ;
          let dstring s =  EPF.fi "%s\n"
              [Printf.sprintf "\"%s\"" (String.escaped s)] in
(* Static information *)
          O.o "static void prelude(FILE *out) {" ;
          let title = sprintf "%% Results for %s %%" doc.Name.file in
          let nice = String.make (String.length title) '%' in
          dstring nice ;
          dstring title ;
          dstring nice ;
          let xs = T.D.lines doc test.T.src in
          List.iter dstring xs ;
          dstring "Generated assembler" ;
          O.oi "ass(out);" ;
          O.o "}" ;
          O.o "" ;
          ()

(* Postlude *)

        let pp_nstates nstates =
          EPF.fi "Histogram (%d states)\n" [nstates]

        let cstring s = sprintf "%S" s

        let show_stats = Cfg.do_stats

        let postlude doc test affi show_topos stats =
          let t = if Cfg.exit_cond then "int" else "void" in
          O.o "#define ENOUGH 10" ;
          O.o "" ;
          begin match Cfg.mode with
          | Mode.Std ->
              O.f "static %s postlude(FILE *out,cmd_t *cmd,hist_t *hist,count_t p_true,count_t p_false,tsc_t total) {" t
          | Mode.PreSi|Mode.Kvm ->
              O.f "static %s postlude(FILE *out,global_t *g,count_t p_true,count_t p_false,tsc_t total) {" t ;
              O.oi "hash_t *hash = &g->hash ;"
          end ;
(* Print header *)
          let c = test.T.condition in
          if Cfg.kind then
            EPF.fi
              (sprintf "Test %s %s\n"
                 doc.Name.name
                 (ConstrGen.pp_kind (ConstrGen.kind_of c)))
              []
          else
            EPF.fi (sprintf "Test %s"  doc.Name.name) [] ;
(* Print histogram *)
          begin match Cfg.mode with
          | Mode.Std ->
              pp_nstates "finals_outs(hist->outcomes)" ;
              O.oi "just_dump_outcomes(out,hist);"
          | Mode.PreSi|Mode.Kvm ->
              pp_nstates "hash->nhash" ;
              O.oi "pp_hash(out,hash,g->verbose > 1,g->group);" ;
              ()
          end ;
(* Print condition and witnesses *)
          let pp_cond = sprintf "\"%s\"" (String.escaped (pp_cond test)) in
          if Cfg.kind then begin
            let to_check = match c with
            | ExistsState _ -> "p_true > 0"
            | ForallStates _|NotExistsState _ -> "p_true == 0" in
            O.fi "int cond = %s;" to_check ;
            EPF.fi "%s\n" ["cond?\"Ok\":\"No\""] ;
            EPF.fi "\nWitnesses\n" [] ;
            let fmt = "Positive: %PCTR, Negative: %PCTR\n" in
            EPF.fi fmt
              [(match c with
              | ExistsState _ -> "p_true"
              | NotExistsState _|ForallStates _ -> "p_false");
               (match c with
               | ExistsState _ -> "p_false"
               | NotExistsState _|ForallStates _ -> "p_true")] ;
            EPF.fi
              "Condition %s is %svalidated\n"
              [pp_cond ; sprintf "%s ? \"\" : \"NOT \"" "cond" ;] ;
          end else begin
            EPF.fi
              "\nCondition %s\n" [pp_cond ;]
          end ;

(* Print meta-information *)
          List.iter
            (fun (k,vs) ->
              if k = "Relax" then
                let fmt = sprintf "Relax %s %%s %%s\n" doc.Name.name in
                EPF.fi fmt ["p_true > 0 ? \"Ok\" : \"No\"";cstring vs]
              else if k = "Prefetch" then begin
              end else
                let fmt = sprintf "%s=%s\n" k vs in
                EPF.fi fmt [])
            test.T.info ;
(* Prefetch shown whenever activated *)
          begin match Cfg.mode with
          | Mode.Std ->
              begin match Cfg.preload with
              | CustomPL ->
                  let fmt = "%s=" in
                  O.fi "fprintf(out,\"%s\",\"%s\");" fmt "Prefetch" ;
                  O.oi "prefetch_dump(out,cmd->prefetch);" ;
                  O.oi "putc('\\n',out);"
              | StaticPL|StaticNPL _ ->
                  let fmt = "%s=%s\\n" in
                  let prf = get_prefetch_info test in
                  O.fi "fprintf(out,\"%s\",\"%s\",\"%s\");" fmt "Prefetch" prf
              | NoPL|RandomPL -> ()
              end
          | Mode.Kvm|Mode.PreSi -> ()
          end ;
(* Affinity info, as computed *)
          begin match Cfg.mode with
          | Mode.Std ->
              begin match affi with
              | Some affi ->
                  O.oi "if (cmd->aff_mode == aff_custom) {" ;
                  let fmt = sprintf "Affinity=%s\n"  (Affi.pp affi) in
                  EPF.fii fmt [] ;
                  O.oi "}"
              | None -> ()
              end
          | Mode.Kvm|Mode.PreSi -> ()
          end ;
(* Observation summary *)
          O.fi
            "count_t cond_true = %s;"
            (match test.T.condition with
            | ExistsState _|NotExistsState _ -> "p_true"
            | ForallStates _ -> "p_false") ;
          O.fi
            "count_t cond_false = %s;"
            (match test.T.condition with
            | ExistsState _|NotExistsState _ -> "p_false"
            | ForallStates _ -> "p_true") ;
          let fmt =
            sprintf
              "Observation %s %%s %%PCTR %%PCTR\n"
              doc.Name.name  in
          let obs =
            "!cond_true ? \"Never\" : !cond_false ? \"Always\" : \"Sometimes\""
          in
          EPF.fi fmt [obs;"cond_true";"cond_false";] ;
(* Parameter sumaries,
   meaningful only when 'remarkable outcomes are present *)
          if show_stats then begin
            O.oi "if (p_true > 0) {" ;
(* Topologies sumaries *)
            begin match Cfg.mode with
            | Mode.Std ->
                if show_topos then begin
                  O.oii "if (cmd->aff_mode == aff_scan) {" ;
                  O.oiii "for (int k = 0 ; k < SCANSZ ; k++) {" ;
                  O.oiv "count_t c = ngroups[k];" ;
                  let fmt = "\"Topology %-6\" PCTR\":> %s\\n\"" in
                  O.fiv "if ((c*100)/p_true > ENOUGH) { printf(%s,c,group[k]); }" fmt ;
                  O.oiii "}" ;
                  O.oii "} else if (cmd->aff_mode == aff_topo) {"  ;
                  O.oiii "printf(\"Topology %-6\" PCTR \":> %s\\n\",ngroups[0],cmd->aff_topo);" ;
                  O.oii "}"
                end
            | Mode.Kvm|Mode.PreSi ->
                O.oii "count_t *ngroups = &g->stats.groups[0];" ;
                O.oii "for (int k = 0 ; k < SCANSZ ; k++) {" ;
                O.oiii "count_t c = ngroups[k];" ;
                O.oiii "if ((g->verbose > 1 && c > 0) || (c*100)/p_true > ENOUGH) {" ;
                let fmt = "Topology %-6PCTR:> part=%d %s\n" in
                EPF.fiv fmt ["c";"k";"g->group[k]"] ;
                O.oiii "}" ;
                O.oii "}"
            end ;
(* Other stats *)
            List.iter
              (fun {tags; name; max; tag; process; } ->
                let ks = Misc.interval 0 (List.length tags) in
                let rec loop_rec i = function
                  | [] ->
                      O.fx i "{" ;
                      let j = Indent.tab i in
                      O.fx j "count_t c = g->stats.%s%s;" name
                        (String.concat ""
                           (List.map (sprintf "[k%i]") ks))  ;
                      let fmt =
                        sprintf "%s %%-6PCTR:> {%s}\n"
                          tag
                          (String.concat ", "
                             (List.map (sprintf "%s=%%d") tags))
                      and args =
                        List.map
                          (fun k -> process (sprintf "k%i" k))
                          ks in
                      O.fx j "if ((g->verbose > 1 && c > 0) || (c*100)/p_true >= ENOUGH) {" ;
                      EPF.fx (Indent.tab j) fmt ("c"::args) ;
                      O.fx j "}" ;
                      O.fx i "}"
                  | k::ks ->
                      let i = Indent.tab i in
                      O.fx i "for (int k%i = 0 ; k%i < %s; k%i++)"
                        k k max k ;
                      loop_rec i ks in
                loop_rec Indent.indent ks)
              stats ;
            O.oi "}" ;
            ()
          end ;
(* Show running time *)
          begin match Cfg.mode with
          | Mode.Std|Mode.PreSi ->
              let fmt = sprintf "Time %s %%f\n"  doc.Name.name in
              EPF.fi fmt ["total / 1000000.0"] ;
              O.oi "fflush(out);"
          | Mode.Kvm ->
              if
                Cfg.have_fault_handler
                && not (T.has_asmhandler test)
              then O.oi "pp_faults();" ;
              let s = sprintf "Time %s "  doc.Name.name in
              O.fi "puts(%S);" s ;
              O.oi "emit_millions(tsc_millions(total));" ;
              O.oi "puts(\"\\n\");"
          end ;
          begin match Cfg.mode with
          | Mode.PreSi|Mode.Kvm ->
             O.oi "if (!g->hash_ok) {" ;
             EPF.fii "Warning: some hash table was full, some outcomes were not collected\n" [] ;
             O.oi "}" ;
          | Mode.Std -> ()
          end ;
          if Cfg.exit_cond then O.oi "return cond;" ;
          O.o "}" ;
          O.o "" ;
          ()

       (* Dump opcode of relevant instructions *)

        let dump_getinstrs t =
          let module I = ObjUtil.Insert(Cfg) in
          I.insert_when_exists O.o "instruction.h" ; (* Always insert *)
          O.o "" ;
          let module D = A.GetInstr.Make(O) in
          let lbl2instr,is = all_instrs t in
          if
            A.GetInstr.active
            && not (A.V.Instr.Set.is_empty is && Misc.nilp lbl2instr)
          then begin
            O.o "/***************************/" ;
            O.o "/* Get instruction opcodes */" ;
            O.o "/***************************/" ;
            O.o "" ;
            A.V.Instr.Set.iter
              (fun i -> D.dump i ; O.o "")
              is ;
            A.V.Instr.Set.iter
              (fun i -> O.f "static ins_t %s;" (A.GetInstr.instr_name i))
              is ;
            List.iter
              (fun ((p,lab as lbl),_) ->
                O.f "static ins_t %s;" (OutUtils.fmt_lbl_instr lbl) ;
                O.f "static const size_t %s=%d;"
                  (OutUtils.fmt_lbl_instr_offset lbl)
                  (find_label_offset p lab t)
              )
              lbl2instr ;
            O.o ""
          end

        let dump_init_getinstrs t =
          O.o "static void init_getinstrs(void) {" ;
          if A.GetInstr.active then begin
            let lbl2instr,is = all_instrs t in
            A.V.Instr.Set.iter
              (fun i ->
              O.fi "%s = %s();"
                (A.GetInstr.instr_name i)
                (A.GetInstr.fun_name i))
            is ;
            let lbl2instrs =
              Misc.group
                (fun ((p1,_),_) ((p2,_),_) -> Proc.equal p1 p2)
                lbl2instr in
            let open OutUtils in
            List.iter
              (fun ps ->
                 match ps with
                 | [] -> assert false
                 | ((p,_),_)::_ ->
                     check_ascall () ;
                     O.fi "size_t %s = prelude_size((ins_t *)code%i);"
                       (fmt_prelude p) p ;
                     List.iter
                       (fun ((p,_ as lbl),_) ->
                          O.fi
                            "%s = *(((ins_t *)code%i)+%s+%s);"
                            (fmt_lbl_instr lbl) p
                            (fmt_prelude p)
                            (fmt_lbl_instr_offset lbl))
                       ps)
              lbl2instrs
          end ;
          O.o "}" ;
          O.o ""


        let dump_opcode env t =
          if instr_in_outs env t then begin
            let  lbl2instr,is = all_instrs t in
            O.o "static char *pretty_opcode(ins_t op) {" ;
            O.fi "if (op == 0) return %S;" "instr:\"UDF\"" ;
            A.V.Instr.Set.iter
              (fun i ->
                O.fi "else if (op == %s) return %S;"
                  (A.GetInstr.instr_name i)
                  (A.V.Instr.pp i))
              is ;
            List.iter
              (fun (lbl,i) ->
                O.fi "else if (op == %s) return %S;"
                  (OutUtils.fmt_lbl_instr lbl)
                  (A.V.Instr.pp i))
              lbl2instr ;
            O.oi "else return \"???\";" ;
            O.o "}"
          end

        let dump_topology_external n =
          O.o "#include \"topology.h\"" ;
          let open Mode in
          begin match Cfg.mode with
          | PreSi|Kvm ->
             O.f "#define inst inst_%d" n ;
             O.f "#define role role_%d" n ;
          | Std ->
             O.f "#define cpu_scan cpu_scan_%d" n
          end ;
          O.f "#define group group_%d" n;
          O.f "#define SCANSZ scansz_%d" n ;
          O.f "#define SCANLINE scanline_%d" n ;
          begin match Cfg.mode with
          | Mode.Std  ->
             O.o "static count_t ngroups[SCANSZ];"
          | Mode.PreSi|Mode.Kvm -> ()
          end ;
          O.o ""

      end
    end
