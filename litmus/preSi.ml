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

(* Alternative test generation, for PreSi *)
open Printf

module type Config = sig
  val verbose : int
  val hexa : bool
  val preload : Preload.t
  val driver : Driver.t
  val alloc : Alloc.t
  val word : Word.t
  val line : int
  val noccs : int
  val affinity : Affinity.t
  val logicalprocs : int list option
  val smt : int
  val nsockets : int
  val smtmode : Smt.t
  val force_affinity : bool
  val kind : bool
  val numeric_labels : bool
  val delay : int
  val c11 : bool
  val timelimit : float option
  val check_nstates : string -> int option
  val stdio : bool
  val exit_cond : bool
  include DumpParams.Config
  val precision : Fault.Handling.t
  val variant : Variant_litmus.t -> bool
end

module Make
    (Cfg:sig include Config
      val sysarch : Archs.System.t
      val is_kvm : bool
      val is_tb : bool
      val ascall : bool
    end)
    (P:sig type ins type code end)
    (A:Arch_litmus.Base with type instruction = P.ins)
    (T:Test_litmus.S with
     type instruction = A.instruction
     and type P.ins = P.ins
     and type P.code = P.code
     and module A = A
     and module FaultType = A.FaultType)
    (O:Indent.S)
    (Lang:Language.S with type t = A.Out.t)
       : sig
      val dump : Name.t -> T.t -> unit
    end = struct

(* Non valid mode for presi *)
  let () =
    if Cfg.variant Variant_litmus.NoInit then
      Warn.user_error "Switches `-variant NoInit` and `-mode (presi|kvm)` are not compatible"

  let () =
    if Cfg.variant Variant_litmus.FPac && not (Cfg.variant Variant_litmus.Pac) then
      Warn.user_error "\"fpac\" variant require \"pac\" variant"

  let () =
    if Cfg.variant Variant_litmus.ConstPacField && not (Cfg.variant Variant_litmus.Pac) then
      Warn.user_error "\"const-pac-field\" variant require \"pac\" variant"

  module Insert =
      ObjUtil.Insert
        (struct
          let sysarch = Cfg.sysarch
        end)

(* MacOS has no affinity control *)
    let do_affinity =
      match Cfg.affinity with
      | Affinity.No -> false
      | _ -> true

(* Handle instruction as data *)
    let has_instruction_ptr = Insert.exists "instruction.h"

(* Precise fault handling, impact on label tables *)
    let do_precise =
      has_instruction_ptr &&
      Cfg.is_kvm && Fault.Handling.is_fatal Cfg.precision

    module
      LocMake
        (CfgLoc:
           sig
             val all_labels : Label.Full.full list
             val need_prelude : bool
           end) = struct

    let do_label_init = Misc.consp CfgLoc.all_labels

    let do_ascall =
      Cfg.ascall || Cfg.is_kvm || do_label_init || CfgLoc.need_prelude
      || Cfg.variant Variant_litmus.Self

    let do_dynalloc =
        let open Alloc in
        match Cfg.alloc with
        | Dynamic -> true
        | Static|Before -> false

      (* Statistic struct may not be initialised when dynamically allocated *)
      let do_stats = not do_dynalloc

      let do_inlined = (* inline topology description *)
        match Cfg.driver with
        | Driver.C -> false
        | _ -> true

      open CType
      module G = Global_litmus

(*******************************************)
(* Set compile time parameters from config *)
(*******************************************)

      module EPF =
        DoEmitPrintf.Make
          (struct
            let emitprintf = Cfg.stdio
            let ctr = Fmt.I32
            let no_file = Cfg.is_kvm
            let brittle = Cfg.is_kvm
          end)(O)

      let timebase_possible =
        if Cfg.is_kvm then
          Insert.exists "kvm_timebase.c"
        else
          Insert.exists "timebase.c"

      let have_timebase =
        Cfg.is_tb &&
        (timebase_possible || SkelUtil.no_timebase_error Cfg.sysarch)

      let do_self = Cfg.variant Variant_litmus.Self

(*************)
(* Utilities *)
(*************)

      let have_fault_handler = Cfg.is_kvm && Insert.exists "kvm_fault_handler.c"

      module UCfg = struct
        let memory = Memory.Direct
        let preload = Cfg.preload
        let mode = Cfg.mode
        let kind = Cfg.kind
        let hexa = Cfg.hexa
        let exit_cond = Cfg.exit_cond
        let have_fault_handler = have_fault_handler
        let do_stats = do_stats
        let sysarch = Cfg.sysarch
        let c11 = Cfg.c11
        let variant = Cfg.variant
        let ascall = do_ascall
      end

      module U = SkelUtil.Make(UCfg)(P)(A)(T)
      module UD = U.Dump(O)(EPF)
      module PU = SkelUtil.PteValUtil(A.V.PteVal)

      let find_addr_type a env = U.find_type (A.location_of_addr a) env
      let see_faults test = Misc.consp (U.get_faults test)
      let see_faults_with_loc test =
        List.exists
          (fun (_,o,_) -> Misc.is_some o)
          (U.get_faults test)

      let some_labels test =
        do_precise || do_label_init || see_faults test

      let need_symbols env test = see_faults test || U.label_in_outs env test

(***************)
(* File header *)
(***************)
      module DP = DumpParams.Make(Cfg)

      let dump_header test =
        O.o "/* Parameters */" ;
        O.o "#define OUT 1" ;
        DP.dump O.o ;
        let n = T.get_nprocs test in
        O.f "#define N %i" n ;
        let nvars = List.length test.T.globals in
        O.f "#define NVARS %i" nvars ;
        let nexe =
          match Cfg.avail  with
          | None -> 1
          | Some a -> if a < n then 1 else a / n in
        O.f "#define NEXE %i" nexe ;
        O.f "#define NTHREADS %i" (nexe*n) ;
        O.f "#define NOCCS %i" Cfg.noccs ;
        if have_timebase then begin
          let delta = sprintf "%i" Cfg.delay in
          if have_timebase then O.f "#define DELTA_TB %s" delta
        end ;
        O.o "/* Includes */" ;
        Insert.insert_when_exists O.o "intrinsics.h" ;
        if do_dynalloc then O.o "#define DYNALLOC 1" ;
        if do_stats then O.o "#define STATS 1" ;
        if Cfg.is_kvm then begin
          O.o "#define KVM 1" ;
          O.o "#include <libcflat.h>" ;
          O.o "#include \"kvm-headers.h\"" ;
          O.o "#include \"utils.h\"" ;
          if not Cfg.stdio then begin
            O.o "#include \"litmus_io.h\"" ;
            O.o "#define NOSTDIO 1"
          end ;
        end else begin
          O.o "#include <stdlib.h>" ;
          O.o "#include <inttypes.h>" ;
          O.o "#include <unistd.h>" ;
          O.o "#include <errno.h>" ;
          O.o "#include <assert.h>" ;
          O.o "#include <time.h>" ;
          O.o "#include <limits.h>" ;
          if Cfg.stdio then begin
            O.o "#include <stdio.h>"
          end else begin
            O.o "#include \"litmus_io.h\"" ;
            O.o "#define NOSTDIO 1"
          end ;
          O.o "#include \"litmus_rand.h\"" ;
          O.o "#include \"utils.h\"" ;
          if Cfg.c11 then O.o "#include <stdatomic.h>";
          if do_affinity then begin (* Affinity always used *)
            O.o "#include \"affinity.h\"" ;
            begin match Cfg.logicalprocs with
            | Some procs ->
               let len = List.length procs in
               if len < DP.avail then
                 Warn.user_error
                   "Cannot run, %d available processors and mapping of size %d"
                  DP.avail len ;
               O.f "static int logical_procs[] = {%s};"
                 (LexSplit.pp_ints procs)
            | None -> ()
            end
          end
        end ;
        if Cfg.variant Variant_litmus.Pac then begin
          O.o "#include \"auth.h\""
        end;
        O.o "#include \"cache.h\"" ;
        O.o "" ;
        O.o "typedef uint32_t count_t;" ;
        O.o "#define PCTR PRIu32" ;
        O.o "" ;
        begin match Cfg.timelimit with
        | None -> ()
        | Some t ->
            O.f "#define TIMELIMIT %4.2f" t
        end ;
        O.o "" ;
        ()

(**********)
(* Delays *)
(**********)

      let nsteps = 9

      let dump_delay_def () =
        if have_timebase then begin
          O.f "#define NSTEPS %i" nsteps ;
          O.f "#define NSTEPS2 ((NSTEPS-1)/2)" ;
        end


(***************************************)
(* Various inclusions from C utilities *)
(***************************************)

(* Time base *)
      let dump_read_timebase () =
        if have_timebase then begin
          O.o "/* Read timebase */" ;
          O.o "#define HAVE_TIMEBASE 1" ;
          O.o "typedef uint64_t tb_t ;" ;
          O.o "#define PTB PRIu64" ;
          Insert.insert O.o
            (if Cfg.is_kvm && Insert.exists "kvm_timebase.c" then
              "kvm_timebase.c"
            else "timebase.c")
        end

(* Memory barrier *)
      let dump_mbar_def () =
        O.o "" ;
(*
        O.o "/* Full memory barrier */" ;
        UD.dump_mbar_def () ;
*)
        let dump_find_ins =
          do_self
          || CfgLoc.need_prelude
          || do_precise
          || do_label_init in
        if dump_find_ins then begin
          ObjUtil.insert_lib_file O.o "_find_ins.c" ;
          O.o ""
        end ;
        if do_self then begin
          Insert.insert O.o "self.c" ;
          O.o "" ;
          if Cfg.is_kvm then
            Insert.insert O.o "kvm-self.c"
          else
            Insert.insert O.o "presi-self.c" ;
          O.o "" ;
        end ;
        if CfgLoc.need_prelude then begin
          ObjUtil.insert_lib_file O.o "_prelude_size.c"
        end ;
        dump_find_ins


      (* Fault handler *)
      let dump_vector_table is_user name tgt =
        O.f "static ins_t *get_vector_table%s(void) {" name ;
        O.oi "ins_t *r;" ;
        O.oi "asm __volatile__ (" ;
        List.iter
          (fun ins -> O.fi "\"%s\\n\"" ins)
          (A.vector_table is_user tgt) ;
        O.oi ":\"=r\" (r));" ;
        O.oi "return r;" ;
        O.o "}" ;
        O.o ""

      let dump_fault_handler find_ins_inserted doc test =
        if have_fault_handler then begin
          let ok,no = T.partition_asmhandlers test in
          begin match no with
          | _::_ ->
             O.o "/* Fault Handling */" ;
             O.o "#define HAVE_FAULT_HANDLER 1" ;
             O.o "" ;
             O.o "typedef struct { int instance,proc; } who_t;" ;
             O.o "" ;
             if do_dynalloc then begin
                 O.o "static count_t *nfaults;" ;
                 O.o "static who_t *whoami;" ;
                 O.o "" ;
                 O.o "static void alloc_fault_handler(void) {" ;
                 O.oi "nfaults = malloc_check(NTHREADS*sizeof(*nfaults));" ;
                 O.oi "whoami = malloc_check(AVAIL*sizeof(*whoami));" ;
                 O.o "}" ;
                 O.o "" ;
                 O.o "static void free_fault_handler(void) {" ;
                 O.oi "free(whoami); free(nfaults);" ;
                 O.o "}"
               end else begin
                 O.o "static count_t nfaults[NTHREADS];" ;
                 O.o "static who_t whoami[AVAIL];"
               end ;
             O.o "" ;
             Insert.insert O.o "instruction.h" ;
             O.o "" ;
             begin
               let open Fault.Handling in
               match Cfg.precision with
               | Fatal ->
                  O.o "#define PRECISE 1" ;
                  O.o ""
               | Skip ->
                  O.o "#define FAULT_SKIP 1" ;
                  O.o ""
               | Handled -> ()
             end ;

             let insert_ins_ops () =
               if not find_ins_inserted then begin
                 ObjUtil.insert_lib_file O.o "_find_ins.c" ;
                 O.o ""
               end in

             let faults = U.get_faults test in
             begin match faults with
             | [] ->
                if do_precise then begin
                    insert_ins_ops () ;
                    if do_dynalloc then begin
                        O.o "static vars_t **vars_ptr;" ;
                        O.o "" ;
                        O.o "static void alloc_see_faults(void) {" ;
                        O.oi "vars_ptr = malloc_check(NEXE*sizeof(*vars_ptr));" ;
                        O.o "}" ;
                        O.o "" ;
                        O.o "static void free_see_faults(void) {" ;
                        O.oi "free(vars_ptr);" ;
                        O.o "}"
                      end
                    else
                      O.o "static vars_t *vars_ptr[NEXE];"
                  end
             | _::_ ->
                O.o "#define SEE_FAULTS 1" ;
                O.o "" ;
                begin match CfgLoc.all_labels with
                | [] -> if do_precise then insert_ins_ops ()
                | _ -> insert_ins_ops ()
                end ;
                if do_dynalloc then begin
                    O.o "static th_faults_info_t **th_faults;" ;
                    O.o "static vars_t **vars_ptr;" ;
                    O.o "" ;
                    O.o "static void alloc_see_faults(void) {" ;
                    O.oi "th_faults = malloc_check(NEXE*sizeof(*th_faults));" ;
                    O.oi "vars_ptr = malloc_check(NEXE*sizeof(*vars_ptr));" ;
                    O.o "}" ;
                    O.o "" ;
                    O.o "static void free_see_faults(void) {" ;
                    O.oi "free(th_faults);" ;
                    O.oi "free(vars_ptr);" ;
                    O.o "}"
                  end else begin
                    O.o "static th_faults_info_t *th_faults[NEXE];" ;
                    O.o "static vars_t *vars_ptr[NEXE];"
                  end ;
                O.o "" ;
             end ;
             O.o "static inline int log_fault(int proc, int instr_symb, int data_symb, int ftype)" ;
             O.o "{" ;
             List.iter (fun f ->
                 let ((p, lbl), loc, ftype) = f in
                 let lbl_cond = match lbl with
                   | None -> ""
                   | Some s -> sprintf " && instr_symb == %s" (SkelUtil.instr_symb_id (OutUtils.fmt_lbl_var p s))
                 and loc_cond = match loc with
                   | None -> ""
                   | Some s -> sprintf " && data_symb == %s" (SkelUtil.data_symb_id (A.V.pp_v_old s))
                 and ftype_cond = match ftype with
                   | None -> ""
                   | Some s -> sprintf " && ftype == %s" (SkelUtil.fault_id (A.FaultType.pp s))
                 in
                 O.fi "if (proc==%d%s%s%s)" p lbl_cond loc_cond ftype_cond;
                 O.fii "return 1;" ;
               ) faults;
             O.fi "return 0;" ;
             O.o "}" ;
             O.o "" ;
             Insert.insert O.o "kvm_fault_handler.c" ;
             O.o "" ;
             if not (T.has_asmhandler test) then begin
               O.o "static void pp_faults(void) {" ;
               O.oi "count_t total=0;" ;
               O.oi "for (int k=0 ; k < NTHREADS; k++) { total += nfaults[k]; }" ;
               O.oi "if (total > 0) {" ;
               O.fii "printf(\"Faults %s %%\"PCTR\"\",total);"  doc.Name.name ;
               O.oii "for (int k = 0 ; k < NTHREADS ; k++) {" ;
               O.oiii "count_t c = nfaults[k];" ;
               let fmt = " P%d:%\"PCTR\"" in
               O.fiii "if (c > 0) printf(\"%s\",k,c);" fmt;
               O.oii "}" ;
               O.oii "printf(\"\\n\");" ;
               O.oi "}" ;
               O.o "}" ;
               O.o ""
             end
          | [] ->
             begin match ok with
             | [] -> ()
             | _::_ ->
                Insert.insert O.o "instruction.h";
                O.o ""
             end ;
          end ;
          let procs_user = ProcsUser.get test.T.info in
          List.iter
            (fun p ->
              dump_vector_table
                (ProcsUser.is procs_user p)
                (sprintf "%d" p) (sprintf "asm_handler%d" p))
            ok ;
           List.iter
             (fun p ->
                if ProcsUser.is procs_user p then
                  dump_vector_table
                    (ProcsUser.is procs_user p)
                    (sprintf "%d" p) (sprintf "asm_handler%d" p))
             no ;
          Insert.insert O.o "asmhandler.c" ;
          O.o "" ;
          let no_ok,no_no =
            List.partition
              (ProcsUser.is procs_user)
              no in
          begin match ok@no_ok with
          | [] ->
             O.o "static void set_fault_vector(int role) { }"
          | _::_ as ok ->
             O.o "static void set_fault_vector(int role) {" ;
             O.oi "ins_t *r = NULL;" ;
             O.oi "switch(role) {" ;
             List.iter
               (fun p ->
                 O.fi "case %d:" p ;
                 O.fii "r = get_vector_table%d();" p ;
                 O.oii "break;")
               ok ;
             begin match no_no with
             | [] -> ()
             | _::_ ->
                O.oi "default:" ;
                O.oii "{" ;
                O.oiii "extern ins_t vector_table;" ;
                O.oiii "r = &vector_table;" ;
                O.oii "}"
             end ;
             O.oi "}" ;
             O.oi "exceptions_init_test(r);" ;
             O.o "}"
          end ;
          O.o ""
          end else if Cfg.is_kvm then begin
            O.o "static void set_fault_vector(int role) { }" ;
            O.o ""
          end

(* User mode *)
      let dump_user_stacks procs_user = match procs_user with
        | [] -> ()
        | _::_ ->
            Insert.insert O.o "kvm_user_stacks.c" ;
            O.o ""

(* Synchronisation barrier *)
      let lab_ext = if Cfg.numeric_labels then "" else "_lab"

      let dump_barrier_def () =
        let fname =  sprintf "barrier%s.c" lab_ext in
        Insert.insert O.o fname

(**************)
(* Topologies *)
(**************)

      let is_active = match Cfg.alloc with
        | Alloc.Dynamic -> false
        | Alloc.Static|Alloc.Before -> not Cfg.is_kvm

      let dbg = false

      let get_all_vars test =
        let all = List.map fst test.T.globals in
        let vs =
          List.map
            (fun (_,(out,_)) -> A.Out.get_addrs_only out)
            test.T.code in
        if dbg then begin
          eprintf "get_all_vars: all={%s} vs=%s\n" (String.concat "," all)
            (String.concat " " (List.map (fun vs -> sprintf "{%s}" (String.concat "," vs)) vs))
        end ;
        all,vs

      type pte_addr = V of string option * A.V.PteVal.t | P of string | Z

      let get_pte_init =
        if Cfg.is_kvm then
          fun env ->
            let open Constant in
            List.fold_right
              (fun bd k -> match bd with
              | A.Location_global (G.Pte pte),v ->
                  begin match v with
                  | Symbolic (Physical (phy,0)) -> (pte,P phy)::k
                  | Concrete z when A.V.Scalar.compare z A.V.Scalar.zero = 0 -> (pte,Z)::k
                  | PteVal pteval ->
                      begin match A.V.PteVal.as_physical pteval with
                      | None ->
                          Warn.user_error "litmus cannot handle pte initialisation with '%s'"
                            (A.V.pp_v v)
                      | Some s -> (pte,V ((if pte=s then None else Some s),pteval))::k
                      end
                  | _ ->
                      Warn.user_error "litmus cannot handle pte initialisation with '%s'"
                        (A.V.pp_v v)
                  end
              | _,_ -> k)
              env []
        else fun _ -> []

      let get_addrs test =
        List.map
          (fun (_,(out,_)) -> fst (A.Out.get_addrs out))
          test.T.code

      let dump_topology doc test =
        O.o "/************/" ;
        O.o "/* Topology */" ;
        O.o "/************/" ;
        O.o "" ;
        let n = T.get_nprocs test in
        if do_inlined then begin
          let module Topo =
          Topology.Make
            (struct
              let verbose = Cfg.verbose
              let file_name = doc.Name.file
              let nthreads = n
              let avail = DP.avail
              let do_affinity = do_affinity
              let smt = Cfg.smt
              let nsockets = Cfg.nsockets
              let smtmode = Cfg.smtmode
              let mode = if Cfg.is_kvm then Mode.Kvm else Mode.PreSi
              let is_active = is_active
              let inlined = true
            end) (O) in
          ignore (Topo.dump_alloc (get_addrs test))
        end else
          UD.dump_topology_external n

(************)
(* Outcomes *)
(************)

      let does_pad t =
        let open CType in
        match t with
        | Pointer _
        | Array (("int"|"int32_t"|"uint32_t"|"int64_t"|"uint64_t"),_)
        | Base ("int"|"int32_t"|"uint32_t"|"int64_t"|"uint64_t") -> true
        | _ -> false

      let dump_loc_tag_coded loc =  sprintf "%s_idx" (A.dump_loc_tag loc)

      let dump_rloc_tag_coded loc =  sprintf "%s_idx" (A.dump_rloc_tag loc)

      let choose_dump_rloc_tag rloc env =
        if U.is_rloc_ptr rloc env then dump_rloc_tag_coded rloc
        else  A.dump_rloc_tag rloc

      (* Fault types *)

      let pp_data_zero = if Cfg.is_kvm then "UNKNOWN" else "0"
      let data_zero =  SkelUtil.data_symb_id pp_data_zero

      let dump_data_indices test =
        O.f "#define %-25s 0" data_zero ;
        (* Define indices for data *)
        List.iteri
          (fun k (a,_) ->
            let idx = if Cfg.is_kvm then 2*k else k in
            O.f "#define %-25s  %i" (SkelUtil.data_symb_id a) (idx+1);
            if Cfg.is_kvm then begin
                O.f "#define %-25s  %i"
                  (SkelUtil.data_symb_id (Misc.add_pte a)) (idx+2)
              end)
          test.T.globals ;
        O.o "" ;
        O.f "static const char *data_symb_name[] = {" ;
        (* Define names for data symbols *)
        O.fi "\"%s\"," pp_data_zero ;
        List.iter
          (fun (a,_) ->
            O.fi "\"%s\"," a ;
            if Cfg.is_kvm then begin
                O.fi "\"%s\"," (Misc.pp_pte a)
              end)
          test.T.globals ;
        O.o "};"


      let dump_fault_type env test =
        if some_labels test then begin

          O.o "typedef struct {" ;
          List.iter
            (fun (p,lbl) ->
              O.fi "ins_t *%s;"
                (OutUtils.fmt_lbl_var p lbl)) CfgLoc.all_labels ;
          if do_precise then O.fi "ins_t *ret[N];" ;
          O.o "} labels_t;" ;
          O.o ""
        end ;
        if need_symbols env test then begin
          UD.dump_label_defs CfgLoc.all_labels ;
          UD.dump_label_funcs do_self CfgLoc.all_labels (T.get_nprocs test)
        end ;
        if see_faults test || U.ptr_in_outs env test then
          dump_data_indices test ;
        if see_faults test then
          Insert.insert O.o  "kvm_fault_type.c"

(* Collected locations *)

      let fmt_outcome test env locs =
        U.fmt_outcome_as_list test
          (fun t -> match Compile.get_fmt Cfg.hexa t with
          | CType.Direct fmt|CType.Macro fmt ->
              if Cfg.hexa then "0x%" ^ fmt else "%" ^ fmt)
          {|label:"P%s"|} locs env

      let some_test_vars test = Misc.consp test.T.globals
      let some_vars test = some_test_vars test || some_labels test

      let dump_outcomes env test =
        let rlocs = U.get_displayed_locs test
        and faults = U.get_faults test in
        O.o "/************/" ;
        O.o "/* Outcomes */" ;
        O.o "/************/" ;
        let dump_vars_loc = function
          | [] -> ()
          | locs ->
             O.fi "intmax_t %s;"
               (String.concat ","
                  (List.map (fun (a,_) -> sprintf "*%s" a) locs)) ;
             if Cfg.is_kvm then begin
                 O.fi "pteval_t %s;"
                   (String.concat ","
                      (List.map
                         (fun (a,_) ->
                           let pte = OutUtils.fmt_pte_tag a
                           and phy = OutUtils.fmt_phy_tag a in
                           sprintf "*%s,%s" pte phy)
                         locs))
               end in
        let dump_vars_code nprocs =
          List.iter
            (fun p ->
              let open OutUtils in
              O.fi "ins_t *%s;" (fmt_code p) ;
              O.fi "size_t %s;" (fmt_prelude p) ;
              O.fi "size_t %s;" (fmt_code_size p))
            (Misc.interval 0 nprocs) in
        if some_vars test then begin
          O.o "#define SOME_VARS 1" ;
          O.o "typedef struct {" ;
          dump_vars_loc test.T.globals ;
          if do_self then dump_vars_code (T.get_nprocs test) ;
          if some_labels test then O.oi "labels_t labels;" ;
          O.o "} vars_t;"
        end else begin
         O.o "typedef void vars_t;"
        end ;
        O.o "" ;
        UD.dump_vars_types false test ;
        UD.dump_array_typedefs test ;
        O.o "typedef struct {" ;
        let fields =
          A.RLocSet.fold
            (fun loc k -> (U.find_rloc_type loc env,loc)::k)
            rlocs [] in
        let rec move_rec lst fs = match lst,fs with
        | None,[] -> true,[]
        | Some f,[] -> false, [f]
        | None,(t,_ as f)::fs
          when does_pad t -> move_rec (Some f) fs
        | _,f::fs ->
            let pad,fs = move_rec lst fs in
            pad,f::fs in
        let pad,fields = move_rec None fields in
        List.iter
          (fun (t,rloc) ->
            if CType.is_ptr t then
              O.fi "int %s;"
                (dump_loc_tag_coded (ConstrGen.loc_of_rloc rloc))
            else match rloc with
            | ConstrGen.Loc (A.Location_global a as loc) ->
                O.fi "%s %s;"
                  (SkelUtil.dump_global_type
                     (G.as_addr a) t) (A.dump_loc_tag loc)
            | _ ->
                O.fi "%s %s;"
                  (CType.dump t) (A.dump_rloc_tag rloc))
          fields ;
        begin match faults with
        | [] -> ()
        | _ ->
           O.fi "th_faults_info_t th_faults[NTHREADS];"
        end ;
        if pad  then O.oi "uint32_t _pad;" ;
        O.o "} log_t;" ;
        O.o "" ;
(* There are some pointers in log *)
        let some_ptr_pte =  U.ptr_pte_in_outs env test in
        let do_see_faults_with_loc = see_faults_with_loc test in
        if some_ptr_pte || do_see_faults_with_loc then begin
          (* To log actual pointers *)
          if some_ptr_pte then begin
            O.o "#define SOME_PTR 1" ;
            O.o "typedef struct {" ;
            A.RLocSet.iter
              (fun rloc ->
                let t = U.find_rloc_type rloc env in
                if CType.is_ptr t || CType.is_pte t then
                  O.fi "%s %s;"  (CType.dump t) (A.dump_rloc_tag rloc))
              rlocs ;
            O.o "} log_ptr_t;" ;
            O.o ""
            end
          end ;
        let some_ptr =  U.ptr_in_outs env test in
        if see_faults test || some_ptr then begin
            (*  Translation to indices *)
            let addr = if Cfg.is_kvm then "p_addr" else "v_addr" in
            let dump_test (s,_) =
              O.fi "if (%s == p->%s) return %s;"
                addr s (SkelUtil.data_symb_id s) ;
              if Cfg.is_kvm then begin
                O.fi "if ((pteval_t *)v_addr == p->%s) return %s;"
                  (OutUtils.fmt_pte_tag s)
                  (SkelUtil.data_symb_id (Misc.add_pte s))
              end in
            begin
              O.o "static int idx_addr(intmax_t *v_addr,vars_t *p) {" ;
              if Cfg.variant Variant_litmus.Pac then
                O.oi "v_addr = (intmax_t*) strip_pauth_data((void*) v_addr);" ;
              begin match test.T.globals with
              | _::_ when Cfg.is_kvm ->
                 O.oi  "intmax_t *p_addr =" ;
                 O.oii "(intmax_t *)((uintptr_t)v_addr & PAGE_MASK);"
              | _ -> ()
              end ;
              if (not Cfg.is_kvm) then
                (* Compatibility with standard mode, recognise NULL *)
                O.fi "if (%s == NULL) return %s;" addr data_zero ;
              List.iter dump_test test.T.globals ;
              O.oi "fatal(\"Cannot find symbol for address\"); return -1;" ;
              O.o "}" ;
              O.o ""
            end ;
(* Pretty-print indices *)
            if some_ptr then begin
              O.o "static const char **pretty_addr = data_symb_name;" ;
              O.o ""
            end
        end ;
(* Now physical pages in output *)
          if Cfg.is_kvm &&
            (U.pte_in_outs env test ||
            U.parel1_in_outs env test)
            then begin
          List.iteri
            (fun k (a,_) ->
              O.f "static const int %s = %i;" (SkelUtil.data_symb_id (Misc.add_physical a)) k)
            test.T.globals ;
          O.o "" ;
          if U.pte_in_outs env test then begin
            O.o "static int idx_physical(pteval_t v,vars_t *p) {" ;
            List.iteri
              (fun k (s,_) ->
                let pref = if k=0 then "if" else "else if" in
                O.fi "%s (litmus_same_oa(v,p->saved_pte_%s)) return %s;"
                  pref s (SkelUtil.data_symb_id (Misc.add_physical s)))
              test.T.globals ;
            O.oi "else return NVARS;" ;
            O.o "}" end;
          if U.parel1_in_outs env test then begin
            O.o "static int idx_physical_parel1(parel1_t v,vars_t *p) {" ;
            O.oi "if (v & msk_f) return NVARS;" ;
            List.iteri
              (fun k (s,_) ->
                let pref = if k=0 then "if" else "else if" in
                O.fi "%s (litmus_same_oa_pte_par(v,p->saved_pte_%s)) return %s;"
                  pref s (SkelUtil.data_symb_id (Misc.add_physical s)))
              test.T.globals ;
            O.oi "else return NVARS;" ;
            O.o "}" end;
          O.o "" ;
          O.f "static const char *pretty_addr_physical[NVARS+1] = {%s,\"???\"};"
            (String.concat ","
               (List.map
                  (fun (s,_) -> sprintf "\"%s\"" (Misc.pp_physical s))
                  test.T.globals)) ;
          O.o ""
        end ;
        UD.dump_opcode env test ;
        O.o "/* Dump of outcome */" ;
        O.o "static void pp_log(FILE *chan,log_t *p) {"  ;
        let fmt = fmt_outcome test env rlocs
        and args =
          A.RLocSet.map_list
            (fun rloc -> match U.find_rloc_type rloc env with
            | Pointer _ when U.is_rloc_label rloc env ->
                None,
                ([sprintf "instr_symb_name[p->%s]" (dump_rloc_tag_coded rloc)], [])
            | Pointer _ ->
                None,
                ([sprintf "pretty_addr[p->%s]" (dump_rloc_tag_coded rloc)], [])
            | Array (_,sz) ->
                let tag = A.dump_rloc_tag rloc in
                let rec pp_rec k =
                  if k >= sz then []
                  else
                    sprintf "p->%s[%i]" tag k::pp_rec (k+1) in
                None,(pp_rec 0, [])
            | Base "pteval_t" ->
                let v = sprintf "p->%s" (A.dump_rloc_tag rloc) in
                let fs =
                  sprintf "pretty_addr_physical[unpack_oa(%s)]" v::
                  List.map
                    (fun f -> sprintf "unpack_%s(%s)" f v)
                    A.V.PteVal.fields in
                    let ds =A.V.PteVal.default_fields in
                Some v,(fs, ds)
            | Base "parel1_t" ->
              let v = sprintf "p->%s" (A.dump_rloc_tag rloc) in
              let fs =
                sprintf "pretty_addr_physical[unpack_oa(%s)]" v::
                List.map
                  (fun f -> sprintf "unpack_%s(%s)" f v)
                  A.V.AddrReg.fields in
              let ds = A.V.AddrReg.default_fields in
              Some v,(fs, ds)
            | t ->
                None,
                ([(if CType.is_ins_t t then sprintf "pretty_opcode(p->%s)"
                 else sprintf "p->%s")
                   (A.dump_rloc_tag rloc)], []))
            rlocs in
        let fst = ref true in
        List.iter2
          (fun (p1,p2) (as_whole,(arg, def_fields)) ->
            let prf = if !fst then "" else " " in
            fst := false ;
            match as_whole with
            | Some as_whole -> (* Assuming a pteval_t *)
                O.fi "if (%s == NULL_PACKED) {" as_whole ;
                EPF.fii ~out:"chan"
                  "%s;" ["\""^prf^p1^"="^(if Cfg.hexa then "0x0" else "0")^"\""] ;
                O.oi "} else {" ;
                let oa,rem = match arg with
                  | oa::rem -> oa,rem
                  | [] -> assert false
                and oa_fmt,rem_fmt = match p2 with
                  | o::oa::rem -> o^oa,rem
                  | _ -> assert false in
                EPF.fii ~out:"chan" (sprintf "%s%s=%s" prf p1 oa_fmt) [oa] ;
                let rec do_rec def_fields fs fmts = match def_fields,fs,fmts with
                  | [],[],[c] ->
                      let c = sprintf "\"%s\"" (String.escaped c) in
                      EPF.fii ~out:"chan" "%s;" [c]
                  | d::def_fields,f::fs,fmt::fmts ->
                      O.fii "if (%s != %s) {" f d ;
                      EPF.fiii ~out:"chan" fmt [f] ;
                      O.oii "}" ;
                      do_rec def_fields fs fmts
                  |_ ->  (* All, defaults, arguments and formats agree *)
                     assert false in
                do_rec def_fields rem rem_fmt ;
                O.oi "}"
            | None ->
                let p2 = String.concat "" p2 in
                EPF.fi ~out:"chan" (sprintf "%s%s=%s;" prf p1 p2) arg)
          fmt args ;
        if List.length faults > 0 then
          O.fi "pp_log_faults_init();";
        List.iter (fun f ->
            let ((p, lbl), loc, ft) = f in
            let lbl = match lbl with
              | None -> "UNKNOWN"
              | Some s -> OutUtils.fmt_lbl_var p s
            and loc = match loc with
              | None -> "UNKNOWN"
              | Some s -> A.V.pp_v_old s
            and ft = match ft with
              | None -> "Unknown"
              | Some ft -> A.FaultType.pp ft
            in
            O.fi "pp_log_faults(chan, &p->th_faults[%d], %d, %s, %s, %s);" p p
              (SkelUtil.instr_symb_id lbl) (SkelUtil.data_symb_id loc) (SkelUtil.fault_id ft)
          ) faults;
        O.o "}" ;
        O.o "" ;
        let locs = A.RLocSet.elements rlocs in (* Now use lists *)
        O.o "/* Equality of outcomes */" ;
        O.o "static int eq_log(log_t *p,log_t *q) {" ;
        O.oi "return" ;
        let do_eq rloc suf =
          let loc = choose_dump_rloc_tag rloc env in
          O.fii "p->%s == q->%s%s" loc loc suf in
        let do_eq_array rloc suf = match U.find_rloc_type rloc env with
        | Array (_,sz) ->
            let tag = choose_dump_rloc_tag rloc env in
            let rec pp_rec k =
              if k < sz then begin
                let suf = if k = sz-1 then suf else " &&" in
                O.fii "p->%s[%i] == q->%s[%i]%s" tag k tag k suf ;
                pp_rec (k+1)
              end in
            pp_rec 0
        | _ -> do_eq rloc suf in
        let do_eq_faults = function
          | [] -> O.oii "1;"
          | _ -> O.oii "eq_faults(p->th_faults, q->th_faults);"
        in
        let rec do_rec = function
          | [] -> do_eq_faults faults
          | x::rem  -> do_eq_array x " &&" ; do_rec rem in
        do_rec  locs ;
        O.o "}" ;
        O.o "" ;
        some_ptr_pte

      let dump_cond_fun env test =
        let module DC =
          CompCond.Make(O)
            (struct

              let with_ok = true

              module C = T.C

              let dump_with_instr v =
                A.GetInstr.dump_instr (T.C.V.pp O.hexa) v

              let dump_value loc v = match v with
              | Constant.(Symbolic (Virtual {pac; _}))
                when not (PAC.is_canonical pac) ->
                  Warn.user_error "PAC not supported in post-conditions in litmus"
              | Constant.(Symbolic (Virtual { name = Symbol.Label(p, lbl); _ }))
                   -> SkelUtil.instr_symb_id (OutUtils.fmt_lbl_var p lbl)
              | Constant.Symbolic _ -> SkelUtil.data_symb_id (T.C.V.pp O.hexa v)
              | Constant.PteVal p ->
                 A.V.PteVal.dump_pack SkelUtil.data_symb_id p
              | Constant.AddrReg a ->
                A.V.AddrReg.dump_pack SkelUtil.data_symb_id a
              | _ ->
                  begin match loc with
                  | Some loc ->
                      let t = U.find_type loc env in
                      if CType.is_pte t && C.V.eq C.V.zero v then
                        "NULL_PACKED"
                      else
                        dump_with_instr v
                  | None ->
                     dump_with_instr v
                  end

              module Loc = struct
                type location = A.location
                type t = location ConstrGen.rloc
                let compare = A.rlocation_compare
                let dump rloc = sprintf "p->%s" (choose_dump_rloc_tag rloc env)
                let dump_fatom dump a =
                  sprintf "p->%s" (SkelUtil.dump_fatom_tag dump a)
              end
            end) in

        begin match test.T.filter with
        | None -> ()
        | Some f ->
            DC.fundef_onlog_prop "filter_cond" (U.cast_constant env) f ;
            O.o "" ;
            ()
        end ;
        let cond = test.T.condition in
        DC.fundef_onlog (U.cast_constant env) cond ;
        ()

      let dump_cond_def env test =
        dump_cond_fun env test ;
        O.o "" ;
        ()

(**************)
(* Parameters *)
(**************)

      let pvtag s = s ^"p"
      let pdtag i = sprintf "d%i" i
      let pctag (i,s) = sprintf "c_%i_%s" i s

      let get_param_vars test =
        if Cfg.is_kvm then []
        else
          match  test.T.globals with
          | [] -> []
          | _::xs -> xs

      let get_tag_vars test =
        List.map (fun (s,_) -> pvtag s) (get_param_vars test)

      let get_param_delays =
        if have_timebase then fun test -> Misc.interval 1 (T.get_nprocs test)
        else fun _ -> []

      let get_tag_delays test = List.map pdtag  (get_param_delays test)

      let get_tag_max_delays test =
        List.map (fun d -> pdtag d,"NSTEPS")  (get_param_delays test)

      let mk_get_param_pos _env test = match test.T.globals with
      | [] -> fun _ -> assert false
      | (_,t0)::xs ->
          let _sum,m =
            List.fold_left
              (fun (i,m) (a,t) ->
                let sz = SkelUtil.nitems t in
                i+sz,StringMap.add a i m)
              (SkelUtil.nitems t0,StringMap.empty) xs in
          fun x -> StringMap.find x m

      let mk_get_param_prefix test =
        let m =
          let rec do_rec m prx = function
            | [] -> m
            | (a,_)::rem ->
                do_rec
                  (StringMap.add a prx m)
                  (a::prx)
                  rem in
          do_rec StringMap.empty [] (get_param_vars test) in
        fun loc -> StringMap.find loc m


      let get_param_caches test =
        let r =
          List.map
            (fun (proc,(out,_)) ->
               List.map (fun a -> proc,a) (A.Out.get_addrs_only out))
            test.T.code in
        List.flatten r

      let get_tag_caches test = List.map pctag (get_param_caches test)

      let do_get_stats test =

        let apply f g =
          let tags = f test in
          fun k -> match tags with | [] -> k | _::_ -> g tags::k in

        let open SkelUtil in
        let k =
          apply get_tag_caches
            (fun tags ->
              {tags;
               name = "dirs"; max="cmax"; tag="Cache";
               process=(fun s -> s);}) [] in
        let k =
          apply get_tag_delays
            (fun tags ->
              {tags;
               name = "delays"; max="NSTEPS"; tag="Delays";
               process = (sprintf "%s-NSTEPS2")}) k  in
        let k =
          apply  get_tag_vars
            (fun tags ->
              {tags;
               name = "vars"; max="NVARS"; tag = "Vars";
               process=(fun s -> s);};) k in
        k

      (* For now, limit kvm stats printing to topology *)
      let get_stats test =
        if Cfg.is_kvm || not do_stats then [] else do_get_stats test

      let has_params test =
        Misc.consp (get_param_vars test)
        || Misc.consp (get_param_delays test)
        || Misc.consp (get_param_caches test)

      let dump_parameters _env test =
        let v_tags = get_tag_vars test
        and d_tags = get_tag_delays test
        and c_tags = get_tag_caches test in
        let all_tags = "part"::v_tags@d_tags@c_tags in

        O.o "/**************/" ;
        O.o "/* Parameters */" ;
        O.o "/**************/" ;
        O.o "" ;
(* Define *)
        O.o "typedef enum { cignore, cflush, ctouch, cmax, } dir_t;" ;
        O.o "" ;
        O.o "typedef struct {" ;
        O.oi "int part;" ;
        let pp_tags t = function
          | [] -> ()
          | tags -> O.fi "%s %s;" t (String.concat "," tags) in
        pp_tags "int" v_tags ;
        pp_tags "int" d_tags ;
        pp_tags "int" c_tags ;
        O.o "} param_t;" ;
        O.o "" ;
        O.f "static param_t param = {%s};"
          (String.concat " "
             (List.map (fun _ -> "-1,") all_tags)) ;
        O.o "" ;
        O.o "static int id(int x) { return x; }" ;
        if have_timebase && T.get_nprocs test > 1 then
          O.o "static int addnsteps(int x) { return x+NSTEPS2; }" ;
        O.o "" ;
        O.o "static parse_param_t parse[] = {" ;
        O.oi "{\"part\",&param.part,id,SCANSZ}," ;
        let pp_tags f =
          List.iter (fun tag -> O.fi "%s," (f tag)) in
        pp_tags
          (fun tag -> sprintf "{\"%s\",&param.%s,id,NVARS}" tag tag)
          v_tags ;
        pp_tags
          (fun tag -> sprintf "{\"%s\",&param.%s,addnsteps,NSTEPS}" tag tag)
          d_tags ;
        pp_tags
          (fun tag -> sprintf "{\"%s\",&param.%s,id,cmax}" tag tag)
          c_tags ;
        O.o "};" ;
        O.o "";
        O.o "#define PARSESZ (sizeof(parse)/sizeof(parse[0]))" ;
        O.o "";
(* Print *)
        if do_stats then begin
          let is_delay tag =
            List.exists (fun x -> Misc.string_eq x tag) d_tags in
          O.f "static void pp_param(FILE *out,param_t *p) {" ;
          let fmt =
            "{" ^
              String.concat ", "
                (List.map (fun tag -> sprintf "%s=%%i" tag) all_tags) ^
                "}"
          and params =
            List.map
              (fun tag ->
                sprintf
                  (if is_delay tag then "p->%s-NSTEPS2" else "p->%s")
                  tag)
              all_tags  in
          EPF.fi fmt params ;
          O.o "}" ;
          O.o "" ;
          (* Statistics *)
          O.o "typedef struct {" ;
          O.oi "count_t groups[SCANSZ];" ;
          O.fi "count_t vars%s;"
            (String.concat "" (List.map (fun _ -> "[NVARS]") v_tags)) ;
          O.fi "count_t delays%s;"
            (String.concat "" (List.map (fun _ -> "[NSTEPS]") d_tags)) ;
          O.fi "count_t dirs%s;"
            (String.concat "" (List.map (fun _ -> "[cmax]") c_tags)) ;
          O.o "} stats_t;" ;
          O.o ""
        end ;
        ()

(*************)
(* Hashtable *)
(*************)

      let hash_max = 128 * 1024

      let hash_size n =
        let rec c_rec n k =
          if n <= 0 then k
          else
            let k = 3 * k in
            if k > hash_max then hash_max
            else c_rec (n-1) k in
        c_rec n 2

      let dump_hash_def tname env test =
        let rlocs = U.get_displayed_locs test
        and faults = U.get_faults test in
        let hashsz = match Cfg.check_nstates tname with
        | Some sz -> 3*sz
        | None ->
            let sz =
              A.RLocSet.fold
                (fun rloc sz ->
                  match U.find_rloc_type rloc env with
                  | CType.Array (_,sz0) -> sz0+sz
                  | _ -> 1+sz)
                rlocs 0 in
            hash_size sz in
        let hashsz = 1+List.fold_left (fun k _ -> 2*k) hashsz faults in
        O.f "#define HASHSZ %i" hashsz ;
        O.o "" ;
        ObjUtil.insert_lib_file O.o "_hash.c" ;
        O.o "" ;
        O.o "static void pp_entry(FILE *out,entry_t *p, int verbose, const char **group) {" ;
        let fmt = "%-6PCTR%c>" in
        EPF.fi fmt ["p->c";"p->ok ? '*' : ':'";] ;
        O.oi "pp_log(out,&p->key);" ;
        if do_stats then begin
          O.oi "if (verbose) {" ;
          EPF.fii " # " [] ;
          O.fii "pp_param(out,&p->p);" ;
          EPF.fii " %s" ["group[p->p.part]"];
          O.oi "}"
        end ;
        EPF.fi "%c" ["'\\n'"] ;
        O.o "}" ;
        O.o ""

(****************************************)
(* Feature enabling/disabling, per role *)
(****************************************)

      let forall_procs test p =
        let n = T.get_nprocs test in
        let rec do_rec k =
          if k = 0 then p 0
          else
            let aft = do_rec (k-1)
            and here = p k in
            if aft = here then here
            else raise Exit in
        try Some (do_rec (n-1)) with Exit -> None

      let dump_set_feature test db =
        if Cfg.is_kvm  then begin
            match db with
            | None ->
               O.o "static void set_feature(int _role) { }"
            | Some db ->
               let open DirtyBit in
               let ha_diff = Misc.is_none (forall_procs test db.ha)
               and hd_diff = Misc.is_none (forall_procs test db.hd) in
               if  ha_diff || hd_diff  then begin
                   O.o "static void set_feature(int role) {" ;
                   O.oi "switch (role) {" ;
                   for k=0 to T.get_nprocs test-1 do
                     O.fi "case %i:" k ;
                     let ha = if db.ha k then '1' else '0'
                     and hd = if db.hd k then '1' else '0' in
                     O.fii "set_hahd_bits(0b%c%c);" hd ha ;
                     O.fii "return;"
                   done ;
                   O.oi "}" ;
                   O.oi "return;" ;
                   O.o"}"
                 end else begin
                   O.o "static void set_feature(int _role) { }"
                 end ;
               O.o ""
          end

(*****************)
(* Test instance *)
(*****************)

      let dump_instance_def procs_user test =
        O.o "/***************/" ;
        O.o "/* Memory size */" ;
        O.o "/***************/" ;
        O.o "" ;
        O.f "/* %s line */"
          (if Cfg.is_kvm then "Page size" else "Cache line") ;
        if Cfg.is_kvm then begin
          O.o "#define LINE LITMUS_PAGE_SIZE" ;
        end else begin
          let nvars = List.length test.T.globals in
          let voff =
            Misc.max_int (MachSize.nbytes MachSize.Quad) (U.max_align test) in
          let needed = voff*nvars in (* bytes needed *)
          let line = Cfg.line + Cfg.line * ((needed-1)/Cfg.line) in
          O.f "#define LINE %i" line ;
          O.f "#define VOFF %d" voff ;
        end ;
        O.o "" ;
        if some_vars test || do_self then begin
          O.o "static void vars_init(vars_t *_vars,intmax_t *_mem) {" ;
          if Cfg.is_kvm then begin
          let has_user = Misc.consp procs_user in
          if Misc.consp test.T.globals then begin
              O.oi "const size_t _sz = LINE/sizeof(intmax_t);";
              O.oi "pteval_t *_p;" ;
              O.o ""
            end ;
          List.iter
            (fun (a,_) ->
              O.fi "_vars->%s = _mem;" a ;
              O.fi "_vars->%s = _p = litmus_tr_pte((void *)_mem);" (OutUtils.fmt_pte_tag a) ;
              O.fi "_vars->%s = *_p;" (OutUtils.fmt_phy_tag a) ;
              if has_user then begin
                O.oi "_p = litmus_tr_pte((void *)_p);";
                O.oi "unset_el0(_p);"
              end ;
              O.oi "_mem += _sz ;")
            test.T.globals ;
          if has_user then O.oi "flush_tlb_all();"
          end ;
          if do_self then begin
              let open OutUtils in
              List.iter
                (fun (n,(t,_)) ->
                  O.fi "_vars->%s = code_size((ins_t *)%s,%i);"
                    (fmt_code_size n) (fmt_code n) (A.Out.get_nrets t) ;
                  O.fi "_vars->%s = prelude_size((ins_t *)%s);"
                    (fmt_prelude n) (fmt_code n);
                  if Cfg.is_kvm then
                    O.fi "_vars->%s = memalign_pages(LINE, _vars->%s);"
                      (fmt_code n) (fmt_code_size n)
                  else
                    O.fi "_vars->%s = mmap_exec(_vars->%s);"
                      (fmt_code n) (fmt_code_size n)
                )
                test.T.code
            end ;
          O.o "}" ;
          O.o "" ;
          O.o "static void vars_free(vars_t *_vars) {" ;
          if do_self then begin
              let open OutUtils in
              List.iter
                (fun (n,_) ->
                  if Cfg.is_kvm then
                    O.fi "free_pages(_vars->%s);" (fmt_code n)
                  else
                    O.fi "munmap_exec(_vars->%s, _vars->%s);" (fmt_code n)
                      (fmt_code_size n))
                test.T.code
            end ;
          O.o "}" ;
          O.o "" ;
          O.o "static void labels_init(vars_t *_vars) {" ;
          if do_label_init || do_precise then
            O.fi "labels_t *lbls = &_vars->labels;" ;
          List.iter (fun (p,lbl) ->
              let off = U.find_label_offset p lbl test in
              let lhs = sprintf "lbls->%s" (OutUtils.fmt_lbl_var p lbl) in
              let proc = if do_self then
                  sprintf "_vars->%s" (LangUtils.code_fun p)
                else
                  LangUtils.code_fun p in
              let rhs =
                sprintf "((ins_t *)%s)+prelude_size((ins_t *)%s)+%d"
                  proc (LangUtils.code_fun p) off in
              O.fi "%s = %s;" lhs rhs)
            CfgLoc.all_labels ;
          if do_precise then begin
            List.iter
              (fun (p,(t,_)) ->
                 let proc =
                   if do_self then
                     sprintf "_vars->%s" (LangUtils.code_fun p)
                   else
                     LangUtils.code_fun p in
                 let rhs =
                   sprintf "((ins_t *)%s)+find_ins(nop,(ins_t *)%s,%d)"
                     proc (LangUtils.code_fun p)
                     (A.Out.get_nnops t-1) in
                 O.fi "lbls->ret[%d] = %s;" p rhs)
              test.T.code
          end ;
          O.o "}";
          ()
        end ;
        O.o "" ;
        ObjUtil.insert_lib_file O.o "_instance.c" ;
        O.o "" ;
        ()

(*****************)
(* Run test code *)
(*****************)


(* Responsability for initialising or collecting, per thread,
   attempt to distribute responsability evenly *)
      let responsible vss =
        let vss = List.map StringSet.of_list vss in
        let all = StringSet.unions vss in
        let tvs = Array.of_list vss in
        let tlen = Array.map StringSet.cardinal tvs in
        let sz = Array.length tvs in
        let tr = Array.make sz [] in
        StringSet.iter
          (fun a ->
            let ks = ref [] in
            for k = 0 to sz-1 do
              if StringSet.mem a tvs.(k) then ks := k :: !ks
            done ;
            let ks =
              List.sort
                (fun k1 k2 -> compare tlen.(k1) tlen.(k2))
                !ks in
            let k = match ks with | k::_ -> k | [] -> assert false in
            tlen.(k) <- tlen.(k)+1 ;
            tr.(k) <- a :: tr.(k))
          all ;
        Array.to_list tr

(* Thread code, as functions *)
      let dump_thread_code
            procs_user env (proc,(out,(_outregs,envVolatile)))  =
        let global_env = U.select_global env in
        let global_env =
          List.map (* Array -> pointer to first element *)
            (fun (loc,t) ->
              let t =
                let open CType in
                match t with | Array (b,_) -> Base b | _ -> t in
              loc,t)
            global_env in
        let user =  ProcsUser.is procs_user proc in
        let args0 =
          let open Template in
          if Cfg.is_kvm then begin
            if user then
              let default_handler = A.default_sync_handler user in
              { trashed=["tr0"];
                inputs=
                  (fun k ->
                     if A.Out.has_asmhandler out then k
                     else
                       ([],("default_handler","&"^default_handler))::k)
                  @@
                  [[CType.word,"cpu"],("sp_usr","user_stack[cpu]")];
                constants=[];
                clobbers=A.user_handler_clobbers;
                externs=
                  if A.Out.has_asmhandler out then []
                  else [(CType.quad,default_handler)];
              }
            else
              { no_extra_args with trashed = ["tr0";]; }
          end else no_extra_args in
        Lang.dump_fun
          O.out args0 global_env envVolatile proc out

(* Untouched variables, per thread + responsability *)
      let part_vars test =
        if dbg then eprintf "part_vars: init=%s\n" (A.debug_state test.T.init) ;
        let all,vs = get_all_vars test in
        let touched_set = StringSet.unions (List.map StringSet.of_list vs)
        and all_set = StringSet.of_list all in
        let rem = StringSet.elements (StringSet.diff all_set touched_set) in
        if dbg then begin
          eprintf "rem={%s}\n" (String.concat "," rem)
        end ;
        let rems = Misc.nsplit (T.get_nprocs test) rem in
        let vss = List.map2 (@) rems vs in
        List.combine rems (responsible vss)

      let memattrs_change a pte_init =
        match Misc.Simple.assoc_opt a pte_init with
        | Some (V (_,pteval)) ->
            not (A.V.PteVal.is_default_attrs pteval)
        | _ -> false

      let init_mem_loc indent clean env test a =
        let do_clean indent symb =
          if clean then begin
            O.fx indent "if (rand_bit(&_c->seed)) cache_flush(&%s);" symb ;
            O.fx indent "else cache_clean(&%s);" symb
          end
        in
        let at =  find_addr_type a env in
        let v = A.find_in_state (A.location_of_addr a) test.T.init in
        let pp_const v =
          let open Constant in
          match v with
          | Concrete i -> A.V.Scalar.pp Cfg.hexa i
          | ConcreteVector _ ->
            Warn.fatal "Vector used as scalar"
          | ConcreteRecord _ ->
            Warn.fatal "Record used as scalar"
          | Symbolic (Virtual a) when not (PAC.is_canonical a.pac) ->
            Warn.user_error "Litmus cannot initialize a virtual address with a non-canonical PAC field"
          | Symbolic (Virtual {name=Constant.Symbol.Label(p,lbl);_}) ->
            sprintf "_vars->labels.%s" (OutUtils.fmt_lbl_var p lbl)
          | Symbolic (Virtual {name=s; tag=None; offset=0; _}) ->
            sprintf "(%s)_vars->%s" (CType.dump at) (Symbol.pp s)
          | Tag _|Symbolic _ ->
            Warn.user_error "Litmus cannot handle this initial value %s"
              (A.V.pp_v v)
          | PteVal _| AddrReg _| Frozen _ -> assert false
          | Instruction _ -> Warn.fatal "FIXME: dump_run_thread functionality for -variant self"
        in
        match at with
        | Array (t,sz) ->
          begin match v with
            | Constant.ConcreteVector ws ->
              let rec init_rec k ws =
                if k < sz then begin
                  let w,ws = match ws with
                    | [] -> "0",[]
                    | w::ws -> pp_const w,ws in
                  let symb = sprintf "%s[%d]" a k in
                  O.fx indent "%s;"
                    (U.do_store (Base t) symb w) ;
                  do_clean indent symb ;
                  init_rec (k+1) ws
                end in
              init_rec 0 ws
            | _ ->
              O.fx indent "for (int _j = 0 ; _j < %i ; _j++) {" sz ;
              let symb = sprintf "%s[_j]" a in
              O.fx (Indent.tab indent) "%s;"
                (U.do_store (Base t) symb (pp_const v)) ;
              O.oii "}" ;
              do_clean indent symb
          end
        | _ ->
          let symb = sprintf "*%s" a in
          O.fx indent "%s;" (U.do_store at symb (pp_const v)) ;
          do_clean indent symb

      let dump_run_thread procs_user faults
          pte_init env test _some_ptr stats global_env
          (_vars,inits) (proc,(out,(_outregs,envVolatile)))  =
        let user_mode = List.exists (Proc.equal proc) procs_user in
        if dbg then eprintf "P%i: inits={%s}\n" proc (String.concat "," inits) ;
        if Misc.consp faults && T.has_asmhandler test then
          Warn.user_error "Post condition cannot check for faults when using custom fault handlers" ;
        let addrs = A.Out.get_addrs_only out in (* accessed in code *)
        O.fi "case %i: {" proc ;
        (* Delays *)
        if have_timebase then begin
          O.oii "int _delay = _g->delay;" ;
          if proc <> 0 then
            O.fii "_delay += (_p->d%i - (NSTEPS-1)/2)*_g->step;" proc
          end ;
        if do_self then
          O.fii "code_init(%s, %s, _vars->%s);" (LangUtils.code_fun_cpy proc)
            (OutUtils.fmt_code proc) (OutUtils.fmt_code_size proc) ;
        (* Initialize them *)
        List.iter (init_mem_loc Indent.indent2 Cfg.is_kvm env test) inits ;
(*        eprintf "%i: INIT {%s}\n" proc (String.concat "," inits) ; *)
        (* And cache-instruct them *)
        O.oii "barrier_wait(_b);" ;
        List.iter
          (fun addr ->
             O.fii "if (_p->%s == ctouch) cache_touch((void *)%s);"
               (pctag (proc,addr)) addr ;
             O.fii "else if (_p->%s == cflush) cache_flush((void *)%s);"
               (pctag (proc,addr)) addr)
          addrs ;
        begin match pte_init with
        | [] -> ()
        | bds ->
            O.oii "barrier_wait(_b);" ;
            List.iter
              (fun x ->
                try
                  begin match Misc.Simple.assoc x bds with
                  | P phy ->
                      O.fii
                        "(void)litmus_set_pte_safe(%s,_vars->pte_%s,_vars->saved_pte_%s);"
                        x x phy ;
                      O.fii "litmus_flush_tlb((void *)%s);" x
                  | Z ->
                      O.fii "(void)litmus_set_pte(%s,_vars->pte_%s,litmus_set_pte_invalid(*_vars->pte_%s));" x x x ;
                      O.fii "litmus_flush_tlb((void *)%s);" x
                  | V (o,pteval) ->
                      let is_default = A.V.PteVal.is_default pteval in
                      if not (o = None && is_default) then begin
                        let arg = match o with
                          | None -> sprintf "_vars->saved_pte_%s" x
                          | Some s -> sprintf "_vars->saved_pte_%s" s in
                        O.fii "pteval_t pte_%s = %s;" x (PU.dump_pteval_flags arg pteval) ;
                        List.iter
                          (fun attr ->
                             O.fii "litmus_set_pte_attribute(&pte_%s, %s);"
                               x attr)
                          (A.V.PteVal.attrs_as_kvm_symbols pteval) ;
                        O.fii "(void)litmus_set_pte_safe(%s,_vars->pte_%s,pte_%s);" x x x ;
                        O.fii "litmus_flush_tlb((void *)%s);" x
                      end
                  end
                with Not_found ->
                  ()
              )
              inits
        end ;
        (* Synchronise *)
        if have_timebase then O.oii "_ctx->next_tb = read_timebase();" ;
        O.oii "barrier_wait(_b);" ;
        if have_timebase then begin
          O.oii "tb_t _tb0 = _ctx->next_tb;" ;
          O.oii "int _delta;" ;
          O.oii "do { _delta = read_timebase() - _tb0; } while (_delta < _delay);"
        end ;
        (* Dump code *)
        if do_ascall then begin
            Lang.dump_call
              ((if do_self then LangUtils.code_fun_cpy else LangUtils.code_fun) proc)
              (if user_mode then ["_c->id"] else [])
              (fun _ s -> s)
              O.out (Indent.as_string Indent.indent2)
            (global_env,[]) envVolatile proc out
        end else begin
          Lang.dump
            O.out (Indent.as_string Indent.indent2)
            (global_env,[]) envVolatile proc out
        end ;
(* Synchronise *)
        O.oii "barrier_wait(_b);" ;
(* Save/Restore pte *)
        let ptes =
          if Cfg.is_kvm then U.get_displayed_ptes test
          else StringSet.empty in
        if Cfg.is_kvm then begin
            let i_ptes,i_non_ptes=
              List.partition
                (fun a -> StringSet.mem a ptes)
                inits in
          List.iter
            (fun a ->
               (* We check the final value of a location using the
                  default memory attributes. For locations that might
                  have been written to with memory attributes other
                  than the default, we clean the cache to make sure we
                  don't create the conditions for mismatched memory
                  attributes accidentally. *)
              if memattrs_change a pte_init then
                O.fii "cache_flush((void *)%s);" a ;
              let pte = OutUtils.fmt_pte_kvm a
              and phy = OutUtils.fmt_phy_kvm a in
              let rhs =
                sprintf "litmus_set_pte_safe(%s,%s,%s)" a pte phy in
              let lhs =
                if StringSet.mem a ptes then
                  sprintf  "_log_ptr->%s = " (OutUtils.fmt_pte_tag a)
                else
                  "(void)" in
              O.fii "%s%s;" lhs rhs)
            (i_ptes@i_non_ptes) ;
          List.iter
            (fun a -> O.fii "litmus_flush_tlb((void *)%s);" a)
            inits
        end ;
(* Collect shared locations final values, if appropriate *)
        let globs = U.get_displayed_globals test in
        if not (G.DisplayedSet.is_empty globs) then begin
          let to_collect = StringSet.of_list inits in
          let to_collect =
            A.RLocSet.filter
              (fun rloc ->
                try
              (* Notice: array accesses collected according to array owner *)
                  let s = ConstrGen.loc_of_rloc (U.tr_global rloc) in
                  StringSet.mem s to_collect
                with U.NotGlobal -> false)
              (U.get_displayed_locs test) in
          A.RLocSet.iter
            (fun loc ->
              let tag = A.dump_rloc_tag loc in
              match U.find_rloc_type loc env with
              | Array (_,sz) ->
                  O.fii
                    "for (int _j = 0 ; _j < %i ; _j++) %s[_j] = %s[_j];"
                    sz (OutUtils.fmt_presi_index tag) tag
              | _ ->
                  let lhs =
                    (if U.is_rloc_ptr loc env then
                       OutUtils.fmt_presi_ptr_index
                     else OutUtils.fmt_presi_index) tag
                    and rhs =
                      let open ConstrGen in
                      match loc with
                      | Deref (v,i) ->
                         sprintf "%s[%d]" (A.dump_loc_tag v) i
                      | _ -> "*"^tag in

                  O.fii "%s = %s;" lhs rhs)
            to_collect
        end ;
        if not (StringSet.is_empty ptes && G.DisplayedSet.is_empty globs) then
          O.oii "barrier_wait(_b);" ;
        if proc = 0 then begin
          (* addresse -> code *)
          A.RLocSet.iter
            (fun rloc ->
               if U.is_rloc_label rloc env then
                 O.fii "%s = get_instr_symb_id(&_vars->labels, %s);"
                   (OutUtils.fmt_presi_index (dump_rloc_tag_coded rloc))
                   (OutUtils.fmt_presi_ptr_index (A.dump_rloc_tag rloc))
               else if U.is_rloc_ptr rloc env then
                 O.fii "%s = idx_addr((intmax_t *)%s,_vars);"
                  (OutUtils.fmt_presi_index (dump_rloc_tag_coded rloc))
                  (OutUtils.fmt_presi_ptr_index (A.dump_rloc_tag rloc))
              else if U.is_rloc_pte rloc env then
                let src = OutUtils.fmt_presi_ptr_index (A.dump_rloc_tag rloc) in
                O.fii "%s = pack_pte(idx_physical(%s,_vars),%s);"
                  (OutUtils.fmt_presi_index (A.dump_rloc_tag rloc)) src src
              else if U.is_rloc_parel1 rloc env then
                let src = OutUtils.fmt_presi_index (A.dump_rloc_tag rloc) in
                O.fii "%s = pack_par_el1(idx_physical_parel1(%s,_vars),%s);"
                  (OutUtils.fmt_presi_index (A.dump_rloc_tag rloc)) src src)
            (U.get_displayed_locs test) ;
          (* condition *)
          let id = match test.T.filter with
          | None -> Indent.indent2
          | Some _f ->
              O.oii "if (filter_cond(_log)) {" ;
              Indent.indent3 in
          O.ox id "int _cond = final_ok(final_cond(_log));" ;
          (* recorded outcome *)
          O.fx id "int _added = hash_add(&_ctx->t,_log%s,1,_cond);"
            (if do_stats then ",_p" else "") ;
          O.ox id "if (!_added && _g->hash_ok) _g->hash_ok = 0; // Avoid writing too much." ;
          (* Result and stats *)
          O.ox id "if (_cond) {" ;
          let nid = Indent.tab id in
          O.ox nid "_ok = 1;" ;
          if do_stats then begin
            O.ox nid
              "(void)__sync_add_and_fetch(&_g->stats.groups[_p->part],1);" ;
            let open SkelUtil in
            List.iter
              (fun {tags; name; _} ->
                let idx =
                  String.concat ""
                    (List.map (sprintf "[_p->%s]") tags) in
                O.fx nid "(void)__sync_add_and_fetch(&_g->stats.%s%s,1);" name idx)
              stats
          end ;
          O.ox id "}" ;
          begin match test.T.filter with
          | None -> () | Some _ -> O.oii "}"
          end
        end ;
        O.oii "break; }" ;
        ()

      let dump_test_code env test procs_user =
        O.o "/*************/" ;
        O.o "/* Test code */" ;
        O.o "/*************/" ;
        O.o "" ;
        if do_ascall then begin
            List.iter
              (dump_thread_code procs_user env)
              test.T.code
          end

      let dump_run_def  env test some_ptr stats procs_user =
        let faults = U.get_faults test in
        (* Notice: initialise the "nop" global variable before others *)
        UD.dump_init_getinstrs test ;
        O.o "inline static int do_run(thread_ctx_t *_c, param_t *_p,global_t *_g) {" ;
        O.oi "int _ok = 0;" ;
        O.oi "int _role = _c->role;" ;
        O.oi "if (_role < 0) return _ok;" ;
        O.oi "ctx_t *_ctx = _c->ctx;" ;
        O.oi "sense_t *_b = &_ctx->b;" ;
        O.oi "log_t *_log = &_ctx->out;" ;
        if some_ptr then O.oi "log_ptr_t *_log_ptr = &_ctx->out_ptr;" ;
        if some_test_vars test || do_self || U.label_in_outs env test then begin
          O.oi "vars_t *_vars = &_ctx->v;"
        end ;
        begin match test.T.globals with
        | [] -> ()
        | globs ->
            (* Define locations *)
            List.iter
              (fun (a,t) ->
                let t =  match t with
                | Array (t,_) -> t
                | _ -> CType.dump t in
                let vopt =
                  if do_ascall || Cfg.is_kvm then "" else "volatile" in
                O.fi "%s %s*%s = (%s %s*)_vars->%s;" t vopt a t vopt a)
              globs ;
            ()
        end ;
        if do_self then
          List.iter
            (fun p -> O.fi "%s _%s = (%s)_vars->%s;" (LangUtils.code_fun_type p)
                        (OutUtils.fmt_code p) (LangUtils.code_fun_type p)
                        (OutUtils.fmt_code p))
            (Misc.interval 0 (T.get_nprocs test));
        let have_faults =
          not (T.has_asmhandler test) &&
            have_fault_handler && Misc.consp faults in
        if have_faults then begin
            O.oi "th_faults_info_init(&_ctx->out.th_faults[_role]);" ;
            ()
          end ;
        O.oi "barrier_wait(_b);" ;
        O.oi "switch (_role) {" ;
(* jade: not sure how to integrate the aligned bit into KVM mode:
   let glob  =
   let global = U.select_global env
   and aligned =
   if
   List.exists
   (fun (a,_) -> U.is_aligned a env)
   test.T.globals
   then
   Warn.fatal "align feature not implemented in presi mode";
   [] in
   let open Lang in
   { global; aligned; volatile=[]; } in
 *)
        let global_env = U.select_global env
        and pte_init = get_pte_init test.T.init in
        List.iter2
          (dump_run_thread
             procs_user faults pte_init env test some_ptr stats global_env)
          (part_vars test)
          test.T.code ;
        O.oi "}" ;
        O.oi "return _ok;" ;
        O.o "}" ;
        O.o "" ;
        ()

(********)
(* zyva *)
(********)

      let dump_choose_params_def env test =
        O.o "inline static int comp_param (st_t *seed,int *g,int max,int delta) {" ;
        O.oi "int tmp = *g;" ;
        O.oi "return tmp >= 0 ? tmp : delta+rand_k(seed,max-delta);" ;
        O.o "}";
        O.o "" ;
        O.o
          "static void choose_params(global_t *g,thread_ctx_t *c,int part) {" ;
        O.oi "int _role = c->role;" ;
        O.oi "if (_role < 0) return;" ;
        O.oi "ctx_t *ctx = c->ctx;" ;
        if has_params test then O.oi "param_t *q = g->param;" ;
        let have_globals =
          not Cfg.is_kvm &&
          begin match test.T.globals with
          | [] -> false
          | _::_ ->
              O.oi "intmax_t *_mem = ctx->mem;" ;
              O.oi "vars_t *_vars = &ctx->v;" ;
              true
          end in
        O.o "" ;
        O.oi "for (int _s=0 ; _s < g->size; _s++) {" ;
        let n = T.get_nprocs test in
        let ps = get_tag_max_delays test in
        let pss = Misc.nsplit n ps in
        let vs = test.T.globals in
        let vss = Misc.nsplit n vs in
        let cs = get_param_caches test in
        let css = Misc.nsplit n cs in
        O.oii "barrier_wait(&ctx->b);";
        O.oii "switch (_role) {" ;
        let get_param_pos = mk_get_param_pos env test in
        let get_param_prefix = mk_get_param_prefix test in
        List.iteri
          (fun i (vs,(ps,cs)) ->
            O.fii "case %i:" i ;
            if i=n-1 then O.oiii "ctx->p.part = part;" ;
(* Location placement comes first*)
            if not Cfg.is_kvm && have_globals then begin
              List.iter
                (fun (a,_) ->
                  try
                    let pos = get_param_pos a in
                    (* Must come first [raises Not_found] *)
                    let tag = pvtag a in
                    O.fiii
                      "ctx->p.%s = comp_param(&c->seed,&q->%s,NVARS,0);"
                      tag tag ;
                    O.fiii "_vars->%s = _mem + LINESZ*ctx->p.%s + %i*VOFFSZ;"
                      a tag pos
                  with Not_found ->
                    O.fiii "_vars->%s = _mem;" a)
                vs ;
              (* Wait for all variables to be allocated (see do_run) *)
              O.oiii "barrier_wait(&ctx->b);" ;
              ()
            end ;
(* Standard parameters *)
            List.iter
              (fun (tag,max) ->
                O.fiii
                  "ctx->p.%s = comp_param(&c->seed,&q->%s,%s,0);" tag tag max ;)
              ps ;
(* Cache parameters*)
            List.iter
              (fun (_proc,v as p) ->
                if is_active then begin
                  let more_test =
                    try
                      let prx = get_param_prefix v in
                      let tsts =
                        sprintf " && ctx->p.%s != 0" (pvtag v)::
                        List.map
                          (fun w ->
                            sprintf " && ctx->p.%s != ctx->p.%s"
                              (pvtag v) (pvtag w))
                          prx in
                      String.concat "" tsts
                    with Not_found -> "" in
                  O.fiii "if (c->act->%s%s) {"
                    (Topology.active_tag p) more_test ;
                  let tag = pctag p in
                  O.fiv "ctx->p.%s = comp_param(&c->seed,&q->%s,cmax,1);"
                    tag tag ;
                  O.oiii "} else {" ;
                  O.fiv "ctx->p.%s = cignore;" tag ;
                  O.oiii "}"
                end else begin
                  let tag = pctag p in
                  O.fiii "ctx->p.%s = comp_param(&c->seed,&q->%s,cmax,1);"
                    tag tag
                end)
              cs ;
            O.oiii "break;")
          (List.combine vss (List.combine pss css)) ;
        O.oii "}" ;
        O.oii "int ok = do_run(c,&ctx->p,g);" ;
        O.oii "if (g->speedcheck) {" ;
        O.oii "/* Global stop */" ;
        O.oiii "if (ok) g->stop_now = 1;" ;
        O.oiii "/* Copy global stop */" ;
        O.oiii "if (_role == 0) ctx->stop_now = g->stop_now;" ;
        O.oiii "/* Synchronise, expecting ctx->stop_now update */" ;
        O.oiii "barrier_wait(&ctx->b);" ;
        O.oiii "if (ctx->stop_now) return;" ;
        O.oii "}" ;
        O.oi "}" ;
        O.o "}" ;
        O.o ""

      let dump_choose_def env test =
        dump_choose_params_def env test ;
        O.o "static void choose(int id,global_t *g) {" ;
        O.oi "param_t *q = g->param;" ;
        O.oi "thread_ctx_t c; c.id = c.seed = id;" ;
        O.oi "st_t seed = 0; st_t seed0 = 0;" ;
        O.o "" ;
        O.oi "for (int nrun = 0; nrun < g->nruns ; nrun++) {" ;
        O.oii "if (SCANSZ <= 1 && !g->fix && id == 0) {" ;
        O.oii "/* Shuffle all threads in absence of topology information. */" ;
        O.oiii "interval_shuffle(&seed0,(int *)g->ind,AVAIL);" ;
        O.oii "}" ;
        O.oii
          "if (g->verbose>1) fprintf(stderr, \"Run %d of %d\\r\", nrun, g->nruns);" ;
        O.oii "/* Select threads partition amounts SCANSZ */" ;
        O.oii "int part = q->part >= 0 ? q->part : rand_k(&seed,SCANSZ);" ;
        O.oii "set_role(g,&c,part);";
        O.oii "choose_params(g,&c,part);" ;
        O.oii "if (g->speedcheck) {" ;
        O.oiii "/* Synchronise, expecting g->stop_now update */" ;
        O.oiii "barrier_wait(&g->gb);" ;
        O.oiii "if (g->stop_now) return;" ;
        O.oii "}" ;
        O.oi "}" ;
        O.o "}" ;
        O.o ""

      let dump_zyva_def tname env test db procs_user =
        O.o "/*******************/" ;
        O.o "/* Forked function */" ;
        O.o "/*******************/" ;
        O.o "" ;
        dump_choose_def env test ;
        O.o "typedef struct {" ;
        O.oi "int id;" ;
        O.oi "global_t *g;" ;
        O.o "} zyva_t;" ;
        O.o "" ;
        O.f "static void %szyva(void *_a) {" (if Cfg.is_kvm then "" else "*") ;
        if Cfg.is_kvm then begin
            if Cfg.variant Variant_litmus.Pac then
              O.oi "init_pauth();" ;
            O.oi "int id = smp_processor_id();" ;
            O.oi "if (id >= AVAIL) return;" ;
            O.oi "zyva_t *a = (zyva_t*)_a + id;" ;
          end
        else begin
            O.oi "zyva_t *a = (zyva_t*)_a;" ;
            O.oi "int id = a->id;" ;
          end;
        O.oi "global_t *g = a->g;" ;
        if Cfg.is_kvm then begin
          begin
            match db with
            | None -> ()
            | Some db ->
              let feat_same p = match forall_procs test p with
                | None -> None
                | Some b -> Some (if b then '1' else '0') in
              match feat_same db.DirtyBit.ha,feat_same db.DirtyBit.hd with
              | Some ha,Some hd ->
                O.fi "set_hahd_bits(0b%c%c);" hd ha
              | _,_ -> ()
          end ;
          if Misc.consp procs_user then begin
            O.oi "set_user_stack(id);"
          end ;
          if have_fault_handler && T.has_defaulthandler test then begin
            if Misc.consp procs_user then begin
              O.o "/* Fault handlers installation depends on user stacks */"
            end ;
            O.oi "install_fault_handler(id);" ;
            if not (T.has_asmhandler test) then begin
              (* Set vector table once for all, as it does not depend on role *)
              O.oi "extern ins_t vector_table;" ;
              O.oi "exceptions_init_test(&vector_table);"
            end
          end
        end ;
        if do_affinity then begin
          let id =
            match Cfg.logicalprocs with
            | Some _ ->
               "logical_procs[id]"
            | None -> "id" in
          O.oi
            (if Cfg.force_affinity then
              sprintf
                "force_one_affinity(%s,AVAIL,g->verbose,\"%s\");"
                id tname
            else
              sprintf "write_one_affinity(%s);" id)
        end ;
        O.oi "choose(id,g);" ;
        if Cfg.is_kvm then begin
          match Cfg.driver,db with
          | Driver.C,Some _ -> (* Reset for next test *)
              O.oi "reset_hahd_bits();"
          | ((Driver.XCode|Driver.Shell),_)
          | (_,None)
            -> ()
        end ;
        if not Cfg.is_kvm then begin
          O.oi "return NULL;"
        end ;
        O.o "}" ;
        O.o ""

(* Prelude *)
      let dump_prelude_def = match Cfg.driver with
      | Driver.Shell -> fun _ _ -> ()
      | Driver.C|Driver.XCode -> UD.prelude

(********)
(* Main *)
(********)

      let dump_main_def doc _env test stats =
        begin match Cfg.driver with
        | Driver.Shell ->
            O.o "#define RUN run" ;
            O.o "#define MAIN 1" ;
        | Driver.C|Driver.XCode ->
            O.f "#define RUN %s" (MyName.as_symbol doc) ;
            O.f "#define PRELUDE 1" ;
            ()
        end ;
        O.o "" ;
        UD.postlude doc test None true stats ;
        O.o "" ;
        ObjUtil.insert_lib_file O.o "_main.c" ;
        ()
  end
(***************)
(* Entry point *)
(***************)
      let dump doc test =
        let avail = match Cfg.avail with
          | None -> 0
          | Some a -> a
        and n = T.get_nprocs test in
        if n > avail then begin
            let pp_avail =
              Misc.app_opt_def "unspecified" (sprintf "%d") Cfg.avail in
            Warn.user_error
              "Cannot run test %s with %d threads on %s available cores"
              doc.Name.name n pp_avail
          end ;
        let module MLoc =
          LocMake
            (struct

              let label_init =
                if has_instruction_ptr then
                  T.get_init_labels test
                else Label.Full.Set.empty

              let all_labels =
                if has_instruction_ptr then
                  T.all_labels test
                else []

              let need_prelude =
                has_instruction_ptr &&
                  begin
                    Misc.consp all_labels
                    || not (Label.Full.Set.is_empty label_init)
                    || Misc.consp (T.from_labels test)
                    || Cfg.variant Variant_litmus.Self
                  end
            end) in
        let open MLoc in
        let db = DirtyBit.get test.T.info
        and procs_user = ProcsUser.get test.T.info in
        ObjUtil.insert_lib_file O.o "header.txt" ;
        dump_header test ;
        UD.dump_getinstrs test ;
        dump_delay_def () ;
        dump_read_timebase () ;
        let find_ins_inserted = dump_mbar_def () in
        dump_barrier_def () ;
        dump_topology doc test ;
        dump_user_stacks procs_user ;
        let env = U.build_env test in
        let stats = get_stats test in
        dump_fault_type env test ;
        let some_ptr = dump_outcomes env test in
        dump_fault_handler find_ins_inserted doc test ;
        dump_cond_def env test ;
        dump_parameters env test ;
        dump_hash_def doc.Name.name env test ;
        dump_set_feature test db ;
        dump_test_code env test procs_user ;
        dump_instance_def procs_user test ;
        dump_run_def env test some_ptr stats procs_user ;
        dump_zyva_def doc.Name.name env test db procs_user ;
        dump_prelude_def doc test ;
        O.o "static int feature_check(void) {" ;
        if do_self then
          O.oi "cache_line_size = getcachelinesize();" ;
        if Cfg.variant Variant_litmus.Pac then begin
          O.fi "if (!check_pac_variant(%S)) return 0;" doc.Name.name;
          let expect_fpac =
            if Cfg.variant Variant_litmus.FPac then "1" else "0" in
          O.fi "if (!check_fpac_variant(%S,%s)) return 0;" doc.Name.name expect_fpac
        end ;
        if Cfg.variant Variant_litmus.ConstPacField then
          O.fi "if (!check_const_pac_field_variant(%S)) return 0;" doc.Name.name;
        if Cfg.is_kvm then begin
          match db with
          | None ->
             O.oi "return 1;" ;
          | Some db ->
             let open DirtyBit in
             let to_check,msg  =
               if db.some_hd then Some "0b0010","dirty bit"
               else if db.some_ha then  Some "0b0001","access flag"
               else None,"" in
             (* Check if hardware features are present *)
             List.iter
               (fun (p,name) ->
                 if T.code_exists p test then begin
                   O.fi "if (!check_%s()) {" name ;
                   O.fii "puts(\"Test %s, required hardware feature '%s' not available on this system\\n\");" doc.Name.name name ;
                   O.fii "return 0;" ;
                   O.oi "}"
                 end)
               A.features ;
             (* Check ability to enable features *)
             begin match to_check with
             | None -> ()
             | Some b ->
                O.oi "uint64_t v = get_hafdbs();" ;
                O.fi "if (v  >= %s) return 1;" b ;
                O.oi "printf(\"HAFDBS is %lx\\n\",v);" ;
                O.fi
                  "puts(\"Test %s, hardware management of %s not available on this system\\n\");"
                  doc.Name.name msg
             end ;
             O.oi "return 0;"
          end
        else
          O.oi "return 1;" ;
        O.o "}" ;
        O.o "" ;
        dump_main_def doc env test stats ;
        ()

    end
