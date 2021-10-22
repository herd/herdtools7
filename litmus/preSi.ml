(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
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
  val pldw : bool
  val cacheflush : bool
  val exit_cond : bool
  include DumpParams.Config
  val precision : bool
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
     type P.ins = P.ins
     and type P.code = P.code
     and module A = A)
    (O:Indent.S)
    (Lang:Language.S
    with type arch_reg = T.A.reg and type t = A.Out.t
    and module RegMap = T.A.RegMap) : sig
      val dump : Name.t -> T.t -> unit
    end = struct
      let do_ascall = Cfg.ascall || Cfg.is_kvm
      let do_precise = Cfg.precision
      let do_dynalloc =
        let open Alloc in
        match Cfg.alloc with
        | Dynamic -> true
        | Static|Before -> false

      (* Statistic struct may not be initialised when dynamically allocated *)
      let do_stats = not do_dynalloc
      let do_inlined = (* inline topology description *)
        match Cfg.mode,Cfg.driver with
        | (Mode.Kvm|Mode.PreSi),Driver.C -> not do_dynalloc
        | _,_ -> true

      open CType
      module G = Global_litmus

(*******************************************)
(* Set compile time parameters from config *)
(*******************************************)
      module Insert =
        ObjUtil.Insert
          (struct
            let sysarch = Cfg.sysarch
          end)

      module EPF =
        DoEmitPrintf.Make
          (struct
            let emitprintf = Cfg.stdio
            let ctr = Fmt.I32
            let no_file = Cfg.is_kvm
          end)(O)

      let timebase_possible =
        if Cfg.is_kvm then
          Insert.exists "kvm_timebase.c"
        else
          Insert.exists "timebase.c"

      let have_timebase =
        Cfg.is_tb &&
        (timebase_possible || SkelUtil.no_timebase_error Cfg.sysarch)

      let have_cache = Insert.exists "cache.c"

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
      end

      module U = SkelUtil.Make(UCfg)(P)(A)(T)
      module UD = U.Dump(O)(EPF)

      let find_addr_type a env = U.find_type (A.location_of_addr a) env
      let see_faults test =  Misc.consp (U.get_faults test)

(***************)
(* File header *)
(***************)

      let dump_header test =
        O.o "/* Parameters */" ;
        O.o "#define OUT 1" ;
        let module D = DumpParams.Make(Cfg) in
        D.dump O.o ;
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
        if do_dynalloc then O.o "#define DYNALLOC 1" ;
        if do_stats then O.o "#define STATS 1" ;
        if Cfg.is_kvm then begin
          O.o "#define KVM 1" ;
          O.o "#include <libcflat.h>" ;
          O.o "#include \"kvm-headers.h\"" ;
          O.o "#include \"utils.h\""
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
          if true then begin (* Affinity always used *)
            O.o "#include \"affinity.h\""
          end
        end ;
        if not do_inlined then O.o "#include \"topology.h\"" ;
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

      let nsteps = 5

      let dump_delay_def () =
        if have_timebase then begin
          O.f "#define NSTEPS %i" nsteps ;
          O.f "#define NSTEPS2 ((NSTEPS-1)/2)" ;
          O.o "#define STEP (DELTA_TB/(NSTEPS-1))"
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
        O.o "/* Full memory barrier */" ;
        Insert.insert O.o "mbar.c" ;
        O.o ""

(* Fault handler *)
      let filter_fault_lbls =
        List.filter
          (fun ((_,o),_) -> Misc.is_some o)

      let tag_seen f =
        sprintf "see_%s" (SkelUtil.dump_fatom_tag A.V.pp_v_old f)
      and tag_code ((p,lbl),_) = sprintf "code_P%d%s" p
          (match lbl with None -> assert false | Some lbl -> "_" ^ lbl)
      and tag_log f =  SkelUtil.dump_fatom_tag A.V.pp_v_old f
      and dump_addr_idx s = sprintf "_idx_%s" s


      let dump_fault_handler doc test =
        if have_fault_handler then begin
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
          if do_precise then begin
            O.o "#define PRECISE 1" ;
            O.o "ins_t *label_ret[NTHREADS];" ;
            O.o ""
          end ;

          let insert_ins_ops () =
            ObjUtil.insert_lib_file O.o "_find_ins.c" ;
            O.o "" ;
            Insert.insert O.o "getnop.c" ;
            O.o "" in

          let faults = U.get_faults test in
          begin match faults with
          | [] -> if do_precise then insert_ins_ops ()
          | _::_ ->
              O.o "#define SEE_FAULTS 1" ;
              O.o "" ;
              begin match filter_fault_lbls faults with
              | [] ->
                  if do_precise then insert_ins_ops ()
              | faults ->
                  insert_ins_ops () ;
                  O.o "typedef struct {" ;
                  O.fi "ins_t %s;"
                    (String.concat ","
                       (List.map (fun f -> sprintf "*%s" (tag_code f)) faults)) ;
                  O.o "} labels_t;" ;
                  O.o "" ;
                  O.o "static labels_t labels;" ;
                  O.o ""
              end ;
              O.o "typedef struct {" ;
              O.fi "int %s;" (String.concat "," (List.map tag_seen faults)) ;
              O.o "} see_fault_t;" ;
              O.o "" ;
              if do_dynalloc then begin
                O.o "static see_fault_t **see_fault;" ;
                O.o "static vars_t **vars_ptr;" ;
                O.o "" ;
                O.o "static void alloc_see_faults(void) {" ;
                O.oi "see_fault = malloc_check(NEXE*sizeof(*see_fault));" ;
                O.oi "vars_ptr = malloc_check(NEXE*sizeof(*vars_ptr));" ;
                O.o "}" ;
                O.o "" ;
                O.o "static void free_see_faults(void) {" ;
                O.oi "free(see_fault); free(vars_ptr);" ;
                O.o "}"
              end else begin
                O.o "static see_fault_t *see_fault[NEXE];" ;
                O.o "static vars_t *vars_ptr[NEXE];"
              end ;
              O.o "" ;
              O.o "static void init_see_fault(see_fault_t *p) {" ;
              List.iter
                (fun f -> O.fi "p->%s = 0;" (tag_seen f))
                faults ;
              O.o "}" ;
              O.o "" ;
          end ;
          O.o "static void record_fault(who_t *w,ins_t *pc,void *loc) {" ;
          begin match faults with
          | [] -> ()
          | _ ->
              O.oi "int i = w->instance;" ;
              O.oi "int idx_loc = idx_addr(loc,vars_ptr[i]);" ;
              O.oi "see_fault_t *sf = see_fault[i];" ;
              O.oi "switch (w->proc) {" ;
              Misc.group_iter
                (fun ((p,_),_) ((q,_),_) -> Misc.int_eq p q)
                (fun ((p,_),_) fs ->
                  O.fi "case %d: {" p ;
                  Misc.group_iteri
                    (fun (_,v) (_,w) -> A.V.compare v w = 0)
                    (fun k (_,v) fs ->
                      let prf = if k > 0 then "else if" else "if"
                      and test =
                        sprintf "idx_loc == %s"
                          (dump_addr_idx (A.V.pp_v_old v)) in
                      O.fii "%s (%s) {" prf test ;
                      let no_lbl ((_,o),_) = Misc.is_none o in
                      let no,fs = List.partition no_lbl fs in
                      begin match no with
                      | [] -> ()
                      | f::_ ->
                          O.fiii "atomic_inc_fetch(&sf->%s);" (tag_seen f) ;
                      end ;
                      List.iteri
                        (fun k f  ->
                          let prf = if k > 0 then "} else if" else "if"
                          and test = sprintf "pc == labels.%s" (tag_code f)
                          and act =  sprintf "atomic_inc_fetch(&sf->%s)" (tag_seen f) in
                          O.fiii "%s (%s) {" prf test ;
                          O.fiv "%s;" act)
                        fs ;
                      if Misc.consp fs then O.fiii "}" ;
                      O.fii "}")
                    fs ;
                  O.oii "break;" ;
                  O.oi "}")
                faults ;
              O.oi "}" ;
          end ;
          O.oi "atomic_inc_fetch(&nfaults[w->proc]);" ;
          O.o "}" ;
          O.o "" ;
          Insert.insert O.o "kvm_fault_handler.c" ;
          O.o "" ;
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

(* User mode *)
      let dump_user_stacks procs_user = match procs_user with
        | [] -> ()
        | _::_ ->
            Insert.insert O.o "kvm_user_stacks.c" ;
            O.o ""

(* Cache *)
      let dump_cache_def () =
        if have_cache then begin
          O.o "/* Cache flush/fetch instructions */" ;
          begin match Cfg.sysarch with
          | `ARM when Cfg.pldw ->
              O.o "#define HAS_PLDW 1" ;
              O.o ""
          | _ -> ()
          end ;
          begin match Cfg.cacheflush with
          | true ->  O.o "#define CACHE_FLUSH 1" ;
          | false -> ()
          end ;
          Insert.insert O.o "cache.c" ;
          O.o ""
        end



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

      type pte_addr = V of string option * PTEVal.t | P of string | Z

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
                      let open PTEVal in
                      begin match PTEVal.as_physical pteval.oa with
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
              let avail = match Cfg.avail with
              | None -> 0
              | Some a -> a

              let smt = Cfg.smt
              let nsockets = Cfg.nsockets
              let smtmode = Cfg.smtmode
              let mode = if Cfg.is_kvm then Mode.Kvm else Mode.PreSi
              let is_active = is_active
              let inlined = true
            end) (O) in
          ignore (Topo.dump_alloc (get_addrs test))
        end else begin
          O.f "#define inst inst_%d" n ;
          O.f "#define role role_%d" n ;
          O.f "#define group group_%d" n;
          O.f "#define SCANSZ scansz_%d" n ;
          O.f "#define SCANLINE scanline_%d" n ;
          O.o "" ;
          ()
        end

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

(* Collected locations *)

      let fmt_outcome test env locs =
        U.fmt_outcome_as_list test
          (fun t -> match Compile.get_fmt Cfg.hexa t with
          | CType.Direct fmt|CType.Macro fmt ->
              if Cfg.hexa then "0x%" ^ fmt else "%" ^ fmt)
          locs env

      let some_vars test = Misc.consp test.T.globals

      let dump_outcomes env test =
        let rlocs = U.get_displayed_locs test
        and faults = U.get_faults test in
        O.o "/************/" ;
        O.o "/* Outcomes */" ;
        O.o "/************/" ;
        begin match test.T.globals with
        | [] -> ()
        | locs ->
            O.o "" ;
            O.o "#define SOME_VARS 1" ;
            O.o "typedef struct {" ;
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
            end ;
            O.o "} vars_t;" ;
        end ;
        O.o "" ;
        UD.dump_vars_types false test ;
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
        | fs ->
            O.fi "int %s;"
              (String.concat "," (List.map tag_log fs))
        end ;
        if pad  then O.oi "uint32_t _pad;" ;
        O.o "} log_t;" ;
        O.o "" ;
(* There are some pointers in log *)
        let some_ptr_pte =  U.ptr_pte_in_outs env test in
        let do_see_faults = see_faults test in
        if some_ptr_pte || do_see_faults then begin
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
          end ;
          let some_ptr =  U.ptr_in_outs env test in
          if some_ptr || do_see_faults then begin
            (* Define indices *)
            List.iteri
              (fun k (a,_) ->
                let idx = if Cfg.is_kvm then 2*k+1 else k+1 in
                O.f "static const int %s = %i;" (dump_addr_idx a) idx ;
                if Cfg.is_kvm then begin
                  O.f "static const int %s = %i;"
                    (dump_addr_idx (Misc.add_pte a)) (2*k+2)
                end)
              test.T.globals ;
            O.o "" ;
            (*  Translation to indices *)
            let dump_test (s,_) =
              O.fi "else if (v_addr == p->%s) return %s;"
                s (dump_addr_idx s) ;
              if Cfg.is_kvm then begin
                O.fi "else if ((pteval_t *)v_addr == p->%s) return %s;"
                  (OutUtils.fmt_pte_tag s)
                  (dump_addr_idx (Misc.add_pte s))
              end in
            O.o "static int idx_addr(intmax_t *v_addr,vars_t *p) {" ;
            O.oi "if (v_addr == NULL) { return 0;}" ;
            List.iter dump_test test.T.globals ;
            O.oi "else { fatal(\"???\"); return -1;}" ;
            O.o "}" ;
            O.o "" ;
(* Pretty-print indices *)
            if some_ptr then begin
              O.f "static const char *pretty_addr[%s+1] = {\"0\",%s};"
                (if Cfg.is_kvm then "(2*NVARS)" else "NVARS")
                (String.concat ""
                   (List.map (fun (s,_) ->
                     sprintf "\"%s\",%s"
                       s
                       (if Cfg.is_kvm then
                         sprintf "\"%s\","
                           (Misc.pp_pte s)
                       else ""))
                      test.T.globals)) ;
              O.o ""
            end
          end
        end ;
(* Now physical pages in output *)
        if Cfg.is_kvm && U.pte_in_outs env test then begin
          List.iteri
            (fun k (a,_) ->
              O.f "static const int %s = %i;" (dump_addr_idx (Misc.add_physical a)) k)
            test.T.globals ;
          O.o "" ;
          O.o "static int idx_physical(pteval_t v,vars_t *p) {" ;
          List.iteri
            (fun k (s,_) ->
              let pref = if k=0 then "if" else "else if" in
              O.fi "%s (litmus_same_oa(v,p->saved_pte_%s)) return %s;"
                pref s (dump_addr_idx (Misc.add_physical s)))
            test.T.globals ;
          O.oi "else return NVARS;" ;
          O.o "}" ;
          O.o "" ;
          O.f "static const char *pretty_addr_physical[NVARS+1] = {%s,\"???\"};"
            (String.concat ","
               (List.map
                  (fun (s,_) -> sprintf "\"%s\"" (Misc.pp_physical s))
                  test.T.globals)) ;
          O.o ""
        end ;
        O.o "/* Dump of outcome */" ;
        O.o "static void pp_log(FILE *chan,log_t *p) {"  ;
        let fmt = fmt_outcome test env rlocs
        and args =
          A.RLocSet.map_list
            (fun rloc -> match U.find_rloc_type rloc env with
            | Pointer _ ->
                None,
                [sprintf "pretty_addr[p->%s]" (dump_rloc_tag_coded rloc)]
            | Array (_,sz) ->
                let tag = A.dump_rloc_tag rloc in
                let rec pp_rec k =
                  if k >= sz then []
                  else
                    sprintf "p->%s[%i]" tag k::pp_rec (k+1) in
                None,pp_rec 0
            | Base "pteval_t" ->
                let v = sprintf "p->%s" (A.dump_rloc_tag rloc) in
                let fs =
                  sprintf "pretty_addr_physical[unpack_oa(%s)]" v::
                  List.map
                    (fun f -> sprintf "unpack_%s(%s)" f v)
                    ["af";"db";"dbm";"valid";"el0";] in
                Some v,fs
            | _ ->
                None,[sprintf "p->%s" (A.dump_rloc_tag rloc)])
            rlocs in
        let fst = ref true in
        List.iter2
          (fun (p1,p2) (as_whole,arg) ->
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
                let ds =
                  let open PTEVal in
                  let p = prot_default in
                  let ds = [p.af; p.db; p.dbm; p.valid;p.el0;] in
                  List.map (sprintf "%i") ds in
                let rec do_rec ds fs fmts = match ds,fs,fmts with
                  | [],[],[c] ->
                      let c = sprintf "\"%s\"" (String.escaped c) in
                      EPF.fii ~out:"chan" "%s;" [c]
                  | d::ds,f::fs,fmt::fmts ->
                      O.fii "if (%s != %s)" f d ;
                      EPF.fiii ~out:"chan" fmt [f] ;
                      do_rec ds fs fmts
                  |_ ->  (* All, defaults, arguments and formats agree *)
                     assert false in
                do_rec ds rem rem_fmt ;
                O.oi "}"
            | None ->
                let p2 = String.concat "" p2 in
                EPF.fi ~out:"chan" (sprintf "%s%s=%s;" prf p1 p2) arg)
          fmt args ;
        begin match faults with
        | [] -> () (* Would output 'printf("");', which gcc may reject. *)
        | _::_ ->
            let fmt2 = U.fmt_faults faults
            and args2 =
              List.map
                (fun f -> sprintf "p->%s?\"\":\"~\"" (tag_log f))
                faults in
            EPF.fi ~out:"chan" fmt2 args2 ;
        end ;
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
        let rec do_eq_faults = function
          | [] -> O.oii "1;"
          | f::fs ->
              let tag = tag_log f in
              O.fii "p->%s == q->%s &&" tag tag ;
              do_eq_faults fs in
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
              let dump_value loc v = match v with
              | Constant.Symbolic _ -> dump_addr_idx (T.C.V.pp O.hexa v)
              | Constant.PteVal p ->
                  let open PTEVal in
                  sprintf
                    "pack_pack(%s,%d,%d,%d,%d,%d)"
                    (dump_addr_idx (PTEVal.pp_oa_old p.oa)) p.af p.db p.dbm p.valid p.el0
              | _ ->
                  begin match loc with
                  | Some loc ->
                      let t = U.find_type loc env in
                      if CType.is_pte t && C.V.eq C.V.zero v then
                        "NULL_PACKED"
                      else
                        T.C.V.pp O.hexa v
                  | None ->  T.C.V.pp O.hexa v
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
        if have_cache then begin
          let r =
            List.map
              (fun (proc,(out,_)) ->
                List.map (fun a -> proc,a) (A.Out.get_addrs_only out))
              test.T.code in
          List.flatten r
        end else []

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
        if Cfg.is_kvm && some_vars test then begin
          let has_user = Misc.consp procs_user in
          O.o "static void vars_init(vars_t *_vars,intmax_t *_mem) {" ;
          O.oi "const size_t _sz = LINE/sizeof(intmax_t);";
          O.oi "pteval_t *_p;" ;
          O.o "" ;
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
          if has_user then O.oi "flush_tlb_all();" ;
          O.o "}"
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
        let myenv = U.select_proc proc env
        and global_env = U.select_global env in
        let global_env =
          List.map (* Array -> pointer to first element *)
            (fun (loc,t) ->
              let t =
                let open CType in
                match t with | Array (b,_) -> Base b | _ -> t in
              loc,t)
            global_env in
        let args0 =
          let open Template in
          if List.exists (Proc.equal proc) procs_user then
            { trashed=["tr0"];
              inputs=[(CType.word,"cpu"),("sp_usr","user_stack[cpu]")];}
          else no_extra_args in
        Lang.dump_fun O.out args0 myenv global_env envVolatile proc out

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

      let dump_run_thread procs_user faults
          pte_init env test _some_ptr stats global_env
          (_vars,inits) (proc,(out,(_outregs,envVolatile)))  =
        let user_mode = List.exists (Proc.equal proc) procs_user in
        if dbg then eprintf "P%i: inits={%s}\n" proc (String.concat "," inits) ;
        let have_faults = have_fault_handler && Misc.consp faults in
        let my_regs = U.select_proc proc env in
        let addrs = A.Out.get_addrs_only out in (* accessed in code *)
        O.fi "case %i: {" proc ;
        if proc = 0 && have_faults then begin
          O.oii "init_see_fault(&_ctx->f);"
        end ;
        (* Delays *)
        if have_timebase then begin
          O.oii "int _delay = DELTA_TB;" ;
          if proc <> 0 then
            O.fii "_delay += (_p->d%i - (NSTEPS-1)/2)*STEP;" proc
        end ;
        (* Initialize them *)
        List.iter
          (fun a ->
            let at =  find_addr_type a env in
            let v = A.find_in_state (A.location_of_addr a) test.T.init in
            let pp_const v =
              let open Constant in
              match v with
              | Concrete i -> A.V.Scalar.pp Cfg.hexa i
              | ConcreteVector _ ->
                  Warn.fatal "Vector used as scalar"
              | Symbolic (Virtual {name=s; tag=None; offset=0; _}) ->
                  sprintf "(%s)_vars->%s" (CType.dump at) s
              | Label _ ->
                  Warn.fatal "PreSi mode cannot handle code labels (yet)"
              | Tag _|Symbolic _ ->
                  Warn.user_error "Litmus cannot handle this initial value %s"
                    (A.V.pp_v v)
              | PteVal _ -> assert false in
            match at with
            | Array (t,sz) ->
                begin match v with
                | Constant.ConcreteVector ws ->
                    let rec init_rec k ws =
                      if k < sz then begin
                          let w,ws = match ws with
                            | [] -> "0",[]
                            | w::ws -> pp_const w,ws in
                          O.fii "%s;"
                            (U.do_store (Base t) (sprintf "%s[%d]" a k) w) ;
                          init_rec (k+1) ws
                        end in
                    init_rec 0 ws
                | _ ->
                    O.fii "for (int _j = 0 ; _j < %i ; _j++) {" sz ;
                    O.fiii "%s;"
                      (U.do_store (Base t)
                         (sprintf "%s[_j]" a) (pp_const v)) ;
                    O.oii "}" ;
                    if Cfg.is_kvm then
                      O.fii "litmus_flush_tlb((void *)%s);" a
                end
            | _ ->
                O.fii "%s;" (U.do_store at (sprintf "*%s" a) (pp_const v)) ;
                if Cfg.is_kvm then O.fii "litmus_flush_tlb((void *)%s);" a)
          inits ;
(*        eprintf "%i: INIT {%s}\n" proc (String.concat "," inits) ; *)
        (* And cache-instruct them *)
        if have_cache then begin
          O.oii "barrier_wait(_b);" ;
          List.iter
            (fun addr ->
              O.fii "if (_p->%s == ctouch) cache_touch((void *)%s);"
                (pctag (proc,addr)) addr ;
              O.fii "else if (_p->%s == cflush) cache_flush((void *)%s);"
                (pctag (proc,addr)) addr)
            addrs
        end ;
        let mem_map =
          let open BellInfo in
          match test.T.bellinfo with
          | None|Some { regions=None;_} -> []
          | Some { regions=Some map;_} -> map in
        begin match pte_init,mem_map with
        | [],[] -> ()
        | bds,_ ->
            O.oii "barrier_wait(_b);" ;
            List.iter
              (fun x ->
                let ok1 = try
                  begin match Misc.Simple.assoc x bds with
                  | P phy ->
                      O.fii
                        "(void)litmus_set_pte(_vars->pte_%s,_vars->saved_pte_%s);"
                        x phy
                  | Z ->
                      O.fii "(void)litmus_set_pte(_vars->pte_%s,litmus_set_pte_invalid(*_vars->pte_%s));" x x
                  | V (o,pteval) ->
                      let is_default = PTEVal.is_default pteval in
                      if not (o = None && is_default) then begin
                        let arg = match o with
                        | None -> sprintf "_vars->saved_pte_%s" x
                        | Some s -> sprintf "_vars->saved_pte_%s" s in
                        O.fii "(void)litmus_set_pte(_vars->pte_%s,%s);"
                          x (SkelUtil.dump_pteval_flags arg pteval);
                        List.iter
                          (fun attr ->
                            let attr = sprintf "attr_%s" (MyName.name_as_symbol attr) in
                            O.fii "litmus_set_pte_attribute(_vars->pte_%s, %s);"
                              x attr) (PTEVal.Attrs.as_list pteval.PTEVal.attrs)
                      end
                  end ;
                  true
                with Not_found ->false in
                let ok2 = try
                  let rs = Misc.Simple.assoc x mem_map in
                  List.iter
                    (fun r ->
                      let r = sprintf "attr_%s" (MyName.name_as_symbol r) in
                      O.fii "litmus_set_pte_attribute(%s,%s);" (OutUtils.fmt_pte_kvm x) r)
                    rs ;
                  true
                with Not_found -> false in
                if ok1 || ok2 then O.fii "litmus_flush_tlb((void *)%s);" x)
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
              (LangUtils.code_fun proc)
              (if user_mode then ["_c->id"] else [])
              (fun _ s -> s)
              O.out (Indent.as_string Indent.indent2)
            my_regs (global_env,[]) envVolatile proc out
        end else begin
          Lang.dump
            O.out (Indent.as_string Indent.indent2)
            my_regs (global_env,[]) envVolatile proc out
        end ;
(* Collect faults *)
        if Cfg.is_kvm then begin
          List.iter
            (fun ((p,_),_ as f) ->
              if Proc.compare proc p = 0 then
                O.fii "_log->%s = _ctx->f.%s?1:0;" (tag_log f) (tag_seen f))
            faults
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
              let pte = OutUtils.fmt_pte_kvm a
              and phy = OutUtils.fmt_phy_kvm a in
              let rhs =
                sprintf "litmus_set_pte(%s,%s)" pte phy in
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
              if U.is_rloc_ptr rloc env then
                O.fii "%s = idx_addr((intmax_t *)%s,_vars);"
                  (OutUtils.fmt_presi_index (dump_rloc_tag_coded rloc))
                  (OutUtils.fmt_presi_ptr_index (A.dump_rloc_tag rloc))
              else if U.is_rloc_pte rloc env then
                let src = OutUtils.fmt_presi_ptr_index (A.dump_rloc_tag rloc) in
                O.fii "%s = pack_pte(idx_physical(%s,_vars),%s);"
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
          O.fx id "hash_add(&_ctx->t,_log%s,1,_cond);"
            (if do_stats then ",_p" else "") ;
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

      let dump_run_def env test some_ptr stats procs_user =
        let faults = U.get_faults test in
        O.o "/*************/" ;
        O.o "/* Test code */" ;
        O.o "/*************/" ;
        O.o "" ;
        if do_ascall then begin
          List.iter
            (dump_thread_code procs_user env)
            test.T.code
        end ;
        O.o "inline static int do_run(thread_ctx_t *_c, param_t *_p,global_t *_g) {" ;
        if not do_ascall then begin match faults with
        | [] -> ()
        | _::_ ->
            O.oi "if (_c->role == 0 && _c->inst == 0) {" ;
            List.iter
              (fun ((p,lbl),_ as f) -> match lbl with
              | None ->
                  O.fii "labels.%s = (ins_t *) &&CODE%d;" (tag_code f) p
              | Some lbl ->
                  let off = U.find_label_offset p lbl test in
                  O.fii "labels.%s = ((ins_t *)&&CODE%d) + %d;" (tag_code f) p off)
              faults ;
            O.oi "}" ;
        end ;
        O.oi "int _ok = 0;" ;
        O.oi "int _role = _c->role;" ;
        O.oi "if (_role < 0) return _ok;" ;
        O.oi "ctx_t *_ctx = _c->ctx;" ;
        O.oi "sense_t *_b = &_ctx->b;" ;
        O.oi "log_t *_log = &_ctx->out;" ;
        if some_ptr then O.oi "log_ptr_t *_log_ptr = &_ctx->out_ptr;" ;
        begin match test.T.globals with
        | [] -> O.o ""
        | globs ->
            O.oi "vars_t *_vars = &_ctx->v;" ;
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
        if Cfg.is_kvm then begin
          let init_rets nop_defined =
            if do_precise then begin
              if not nop_defined then O.oi "ins_t nop = getnop();" ;
              List.iter
                (fun (p,(t,_)) ->
                  let rhs = sprintf
                      "((ins_t *)%s)+find_ins(nop,(ins_t *)%s,%d)"
                      (LangUtils.code_fun p)
                      (LangUtils.code_fun p)
                      (A.Out.get_nnops t-1) in
                  O.fi "label_ret[%d] = %s;" p rhs)
                test.T.code
            end in
          begin match filter_fault_lbls faults with
          | [] ->
              O.o "static void init_labels(void) {" ;
              init_rets false ;
              O.o "}" ;
              O.o ""
          | faults ->
              O.o "static void init_labels(void) {" ;
              O.oi "ins_t nop = getnop();" ;
              List.iter
                (fun ((p,lbl),_ as f) ->
                  let lbl = Misc.as_some lbl in
                  let off = U.find_label_offset p lbl test+1 in (* +1 because of added inital nop *)
                  let lhs = sprintf "labels.%s" (tag_code f)
                  and rhs =
                    sprintf "((ins_t *)%s)+find_ins(nop,(ins_t *)%s,0)+%d"
                      (LangUtils.code_fun p) (LangUtils.code_fun p) off in
                  O.fi "%s = %s;" lhs rhs)
                faults ;
              init_rets true ;
              O.o "}" ;
              O.o ""
          end
        end ;
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
        O.oi "for (int _s=0 ; _s < g->size ; _s++) {" ;
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
        O.oii "(void)do_run(c,&ctx->p,g);" ;
        O.oi "}" ;
        O.o "}" ;
        O.o ""

      let dump_choose_def env test =
        dump_choose_params_def env test ;
        O.o "static void choose(int id,global_t *g) {" ;
        O.oi "param_t *q = g->param;" ;
        O.oi "thread_ctx_t c; c.id = c.seed = id;" ;
        O.oi "st_t seed = 0;" ;
        O.o "" ;
        O.oi "for (int nrun = 0; nrun < g->nruns ; nrun++) {" ;
        O.oii
          "if (g->verbose>1) fprintf(stderr, \"Run %d of %d\\r\", nrun, g->nruns);" ;
        O.oii "int part = q->part >= 0 ? q->part : rand_k(&seed,SCANSZ);" ;
        O.oii "set_role(g,&c,part);";
        O.oii "choose_params(g,&c,part);" ;
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
        if have_fault_handler then begin
            if Misc.consp procs_user then begin
                O.o "/* Fault handlers installation depends on user stacks */"
            end ;
            O.oi "install_fault_handler(id);"
        end ;
        if not Cfg.is_kvm then begin
          O.oi
            (if Cfg.force_affinity then
              sprintf
                "force_one_affinity(id,AVAIL,g->verbose,\"%s\");"
                tname
            else
              "write_one_affinity(id);")
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
        let db = DirtyBit.get test.T.info
        and procs_user = ProcsUser.get test.T.info in
        ObjUtil.insert_lib_file O.o "header.txt" ;
        dump_header test ;
        dump_delay_def () ;
        dump_read_timebase () ;
        dump_mbar_def () ;
        dump_cache_def () ;
        dump_barrier_def () ;
        dump_topology doc test ;
        dump_user_stacks procs_user ;
        let env = U.build_env test in
        let stats = get_stats test in
        let some_ptr = dump_outcomes env test in
        dump_fault_handler doc test ;
        dump_cond_def env test ;
        dump_parameters env test ;
        dump_hash_def doc.Name.name env test ;
        dump_set_feature test db ;
        dump_instance_def procs_user test ;
        dump_run_def env test some_ptr stats procs_user ;
        dump_zyva_def doc.Name.name env test db procs_user ;
        dump_prelude_def doc test ;
        if Cfg.is_kvm then begin
          match db with
          | None ->
             O.o "static void feature_check(void) { }"
          | Some db ->
             let open DirtyBit in
             let to_check,msg  =
               if db.some_hd then Some "0b0010","dirty bit"
               else if db.some_ha then  Some "0b0001","access flag"
               else None,"" in
             O.o "static void feature_check(void) {" ;
             (* Check if hardware features are present *)
             List.iter
               (fun (p,name) ->
                 if T.code_exists p test then
                   O.fi "if (!check_%s()) fatal(\"Test %s, required hardware feature '%s' not available on this system\");" name doc.Name.name name)
               A.features ;
             (* Check ability to enable features *)
             begin match to_check with
             | None -> ()
             | Some b ->
                O.oi "uint64_t v = get_hafdbs();" ;
                O.fi "if (v  >= %s) return;" b ;
                O.oi "printf(\"HAFDBS is %lx\\n\",v);" ;
                O.fi
                  "fatal(\"Test %s, hardware management of %s not available on this system\");"
                  doc.Name.name msg
             end ;
             O.o "}" ;
             O.o ""
          end ;
        dump_main_def doc env test stats ;
        ()

    end
