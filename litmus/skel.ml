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

open Printf


let indent = Indent.indent
and indent2 = Indent.indent2
and indent3 = Indent.indent3
and indent4 = Indent.indent4
and indent5 = Indent.indent5

module type Config = sig
  val verbose : int
  val verbose_prelude : bool
  val verbose_barrier : bool
  val hexa : bool
  val speedcheck : Speedcheck.t
  val driver : Driver.t
  val safer : Safer.t
  val cautious : bool
  val preload : Preload.t
  val word : Word.t
  val barrier : Barrier.t
  val collect : Collect.t
  val syncmacro : int option
  val isync : bool
  val memory : Memory.t
  val contiguous : bool
  val alloc : Alloc.t
  val doublealloc : bool
  val noalign : Align.t option
  val threadstyle : ThreadStyle.t
  val launch : Launch.t
  val affinity : Affinity.t
  val logicalprocs : int list option
  val smt : int
  val nsockets : int
  val smtmode : Smt.t
  val force_affinity : bool
  val kind : bool
  val numeric_labels : bool
  val delay : int
  val syncconst : int
  val morearch : MoreArch.t
  val xy : bool
  val pldw : bool
  val cacheflush : bool
  val c11 : bool
  val c11_fence : bool
  val ascall : bool
  val variant : Variant_litmus.t -> bool
  val stdio : bool
  val limit : bool
  val exit_cond : bool
  include DumpParams.Config
end

let sentinel   = "-239487" (* Susmit's sentinel *)
let sentinel16 =   "39487"
let sentinel8  =      "87"

let sentinel_of t =
  match t with
  | CType.Base "uint8_t"  -> sentinel8
  | CType.Base "uint16_t" -> sentinel16
  | _ -> sentinel

module Make
    (Cfg:sig include Config val sysarch : Archs.System.t end)
    (P:sig type code end)
    (A:Arch_litmus.Base)
    (T:Test_litmus.S with type P.code = P.code and module A = A)
    (O:Indent.S)
    (Lang:Language.S
    with type arch_reg = T.A.reg and type t = A.Out.t
    and module RegMap = T.A.RegMap) : sig
      val dump : Name.t -> T.t -> unit
    end = struct
      module C = T.C
      open Constant
      open CType

(* Options *)
      let do_self = Cfg.variant Variant_litmus.Self
      let do_ascall = Cfg.ascall || do_self

      open Speedcheck
      let do_vp = Cfg.verbose_prelude
      let driver = Cfg.driver
      let do_speedcheck = match Cfg.speedcheck  with
      | NoSpeed -> false
      | SomeSpeed|AllSpeed -> true
      let do_safer = match Cfg.safer with
      | Safer.No -> false
      | Safer.All|Safer.Write -> true
      let do_safer_write = match Cfg.safer with
      | Safer.No -> false
      | Safer.All|Safer.Write -> true

      open Preload
      let do_randompl =
        match Cfg.preload with RandomPL -> true
        | NoPL|CustomPL|StaticPL|StaticNPL _ -> false
      let do_custom =
        match Cfg.preload with CustomPL -> true
        | StaticNPL _|StaticPL|NoPL|RandomPL -> false
      let do_staticpl =
        match Cfg.preload with StaticPL -> true
        | CustomPL|NoPL|RandomPL|StaticNPL _ -> false
      let do_staticNpl =
        match Cfg.preload with StaticNPL _ -> true
        | CustomPL|NoPL|RandomPL|StaticPL -> false

      let ws = Cfg.word

      open Barrier
      let barrier =  Cfg.barrier
      let do_timebase = match barrier with
      | TimeBase -> true
      | _ -> false

      let have_timebase = function
        | `AArch64 -> false (* FIXME: ??? *)
        | `ARM|`MIPS -> false
        | `PPC|`X86 | `X86_64-> true
        | _ -> false

      let have_timebase = have_timebase Cfg.sysarch

      let do_collect_local = match Cfg.collect with
      | Collect.Local|Collect.Both ->
          if do_timebase then true
          else Warn.user_error "Local collect requires timebase barrier"
      | Collect.After -> false

      let do_collect_after = match Cfg.collect with
      | Collect.After|Collect.Both -> true
      | Collect.Local -> false

      let do_collect_both = do_collect_local && do_collect_after

      let do_verbose_barrier = Cfg.verbose_barrier
      let do_verbose_barrier_local =
        do_verbose_barrier && do_collect_local && not do_collect_after

      let do_check_globals = do_collect_after && (do_safer || do_randompl)


      let do_sync_macro = match Cfg.syncmacro with
      | None -> false
      | Some _ -> true

      let sync_macro_n  = match Cfg.syncmacro with
      | None -> 0
      | Some n -> n

      let do_isync = Cfg.isync

      open Memory
      let memory = Cfg.memory
      let indirect_star = match memory with
      | Direct -> ""
      | Indirect -> "*"

      let stride = Cfg.stride
      let do_contiguous = Cfg.contiguous

      let noalign_info t =
        try
          let info = List.assoc "Noalign" t.T.info in
          match Align.parse info with
          | Some _ as r -> r
          | None -> Warn.fatal "Align info parsing failure on '%s'" info
        with Not_found -> None

      let do_noalign test =
        let no = match Cfg.noalign with
        | Some no -> no
        | None -> match noalign_info test with
          | None -> Align.No
          | Some no -> no in
        match no with
        | Align.All -> fun _ -> true
        | Align.No -> fun _ -> false
        | Align.Not s -> fun x -> StringSet.mem x s



      let do_prealloc = match Cfg.alloc with
      | Alloc.Before -> true
      | Alloc.Dynamic|Alloc.Static -> false

      let do_staticalloc =  match Cfg.alloc with
      | Alloc.Static -> true
      | Alloc.Dynamic|Alloc.Before -> false

      let do_dynamicalloc = not do_staticalloc

      open Launch
      let launch = Cfg.launch
      let affinity = Cfg.affinity
      let smt = Cfg.smt
      let smtmode = Cfg.smtmode

      let do_affinity = match affinity with
      | Affinity.No -> false
      | Affinity.Incr _|Affinity.Random|Affinity.Custom|Affinity.Scan -> true

(* Check is custom affinity is possible *)
      let mk_dca test =
        let f test =
          if do_affinity then
            try
              let res = List.assoc "Com" test.T.info in
              let coms =
                try LexAffinity.coms res
                with _ -> assert false in
              let nprocs = T.get_nprocs test in
              if nprocs <> List.length coms then None
              else if smt > 1 then match smtmode with
              | Smt.No -> None
              | Smt.Seq|Smt.End -> Some res
              else None
            with Not_found -> None
          else None in
        match  f test with
        | None -> false,""
        | Some s -> true,s

      let do_cores =
        do_affinity &&
        smt >= 1 &&
        (match smtmode with
        | Smt.No -> false
        | Smt.Seq|Smt.End -> true)

(* Check if scanning affinity is possible *)
      let mk_dsa test =
        let n =  T.get_nprocs test in
        n < 5 &&
        do_cores &&
        (match Cfg.avail with Some a -> a >= n | None -> false)

      let do_force_affinity = Cfg.force_affinity
      let do_numeric_labels = Cfg.numeric_labels


(* Utilities *)
      module U = SkelUtil.Make(Cfg)(P)(A)(T)
      module EPF =
        DoEmitPrintf.Make
          (struct
            let emitprintf = Cfg.stdio
            let ctr = Fmt.I64
          end)(O)
      module UD = U.Dump(O)(EPF)

(* Inserted source *)

      module Insert =
        ObjUtil.Insert
          (struct
            let sysarch = Cfg.sysarch
            let word = ws
          end)

(* Location utilities *)
      let get_global_names t = List.map fst t.T.globals

      let find_index  v =
        let rec find_rec k = function
          | [] -> assert false
          | w::ws ->
              if String.compare v w = 0 then k
              else find_rec (k+1) ws in
        find_rec 0

      let find_global_init a t =
        A.find_in_state (A.Location_global a) t.T.init

      let have_observed_globals t =
        not (StringSet.is_empty (U.get_observed_globals t))


      let dump_loc_name loc =  match loc with
      | A.Location_reg (proc,reg) -> A.Out.dump_out_reg proc reg
      | A.Location_global s -> s
      | A.Location_deref (s,i) -> sprintf "%s_%i" s i


      let dump_loc_copy loc = "_" ^ dump_loc_name loc ^ "_i"
      let dump_loc_param loc = "_" ^ dump_loc_name loc

      let dump_ctx_loc pref loc = match loc with
      | A.Location_reg (proc,reg) ->
          sprintf "%s%s[_i]" pref (A.Out.dump_out_reg  proc reg)
      | A.Location_global s ->
          begin match memory with
          | Direct ->
              sprintf "%s%s[_i]" pref s
          | Indirect ->
              sprintf "*(%s%s[_i])" pref s
          end
      | A.Location_deref (s,idx) ->
          begin match memory with
          | Direct ->
              sprintf "%s%s[_i][%i]" pref s idx
          | Indirect ->
              sprintf "(*(%s%s[_i]))[%i]" pref s idx
          end

      let dump_loc = dump_ctx_loc ""


(* Dump left value *)
      let dump_leftval a = match memory with
      | Direct -> sprintf "%s[_i]" a
      | Indirect -> sprintf "*(%s[_i])" a

(* Dump left & right values when pointer to context is available *)

(* Left value *)
      let dump_a_leftval a = match memory with
      | Direct -> sprintf "_a->%s[_i]" a
      | Indirect -> sprintf "*(_a->%s[_i])" a

(* Right value *)
      let dump_a_addr = match memory with
      | Direct -> sprintf "&(_a->%s[_i])"
      | Indirect -> sprintf "_a->%s[_i]"

(* Right value, casted if pointer *)
      let dump_a_v_casted = function
        | Concrete i ->  A.V.Scalar.pp  Cfg.hexa i
        | Symbolic ((s,None),_) -> sprintf "((int *)%s)" (dump_a_addr s)
        | Symbolic _|Label _|Tag _ -> assert false

(* Dump left & right values when context is available *)

(* Left value *)
      let dump_ctx_tag = match memory with
      | Direct -> sprintf "%s"
      | Indirect -> sprintf "mem_%s"

(* Right value from ctx *)
      let dump_ctx_addr = match memory with
      | Direct -> sprintf "&(ctx.%s[_i])"
      | Indirect -> sprintf "ctx.%s[_i]"

(* Test condition *)

      let dump_header test =
        O.o "/* Parameters */" ;
        let module D = DumpParams.Make(Cfg) in
        D.dump O.o ;
        let n = T.get_nprocs test in
        O.f "#define N %i" n ;
        if do_staticalloc then begin
          let nexe =
            match Cfg.avail  with
            | None -> 1
            | Some a -> if a < n then 1 else a / n in
          O.f "#define NEXE %i" nexe ;
          O.o "#define SIZE_OF_MEM (NEXE * SIZE_OF_TEST)" ;
          O.o "#define SIZE_OF_ALLOC (NEXE * (SIZE_OF_TEST+1))" ;
        end ;
        O.f "#define AFF_INCR (%i)"
          (match affinity with
          | Affinity.Incr i -> i
          | Affinity.Random|Affinity.Custom|Affinity.Scan -> 0
          | Affinity.No -> -1) ;
        if do_timebase then begin
          let delta = sprintf "%i" Cfg.delay in
          if have_timebase then O.f "#define DELTA_TB %s" delta
        end ;
        O.o "/* Includes */" ;
        O.o
          (if Cfg.stdio then "#include <stdio.h>"
          else "#include \"litmus_io.h\"") ;
        O.o "#include <stdlib.h>" ;
        O.o "#include <unistd.h>" ;
        O.o "#include <errno.h>" ;
        O.o "#include <assert.h>" ;
        O.o "#include <time.h>" ;
        O.o "#include <limits.h>" ;
        O.o "#include \"utils.h\"" ;
        if Cfg.c11 then O.o "#include <stdatomic.h>";
        O.o "#include \"outs.h\"" ;
        if do_affinity then begin
          O.o "#include \"affinity.h\""
        end ;
        O.o "" ;
        O.o "/* params */" ;
        O.o "typedef struct {" ;
        O.oi "int verbose;" ;
        O.oi "int size_of_test,max_run;" ;
        if do_verbose_barrier then O.oi "int verbose_barrier;" ;
        begin if Stride.some stride then
          O.oi "int stride;"
        end ;
        if Cfg.timeloop > 0 then O.oi "int max_loop;" ;
        if do_affinity then begin
          O.oi "aff_mode_t aff_mode;";
          O.oi "int ncpus, ncpus_used;"
        end ;
        if do_speedcheck then O.oi "int speedcheck, stop_now;" ;
        begin match memory with
        | Direct -> ()
        | Indirect -> O.oi "int do_shuffle;"
        end ;
        begin match launch with
        | Changing -> O.oi "int do_change;"
        | Fixed -> ()
        end ;
        if do_timebase && have_timebase then O.oi "int *delays;" ;
        if do_custom then O.oi "prfdirs_t *prefetch;" ;
        if do_staticpl then O.oi "unsigned int static_prefetch;" ;
        if do_cores then O.oi "cpus_t *cm;" ;
        if  do_sync_macro then O.oi "int max_idx;" ;
        O.o "} param_t;" ;
        O.o"" ;
        if do_sync_macro then begin
          O.f "#define SYNC_K %i" Cfg.syncconst ;
          O.f "#define SYNC_N %i" sync_macro_n ;
          ()
        end ;
        ()


(*************************************************)
(* Soft barriers: to synchronize test iterations *)
(*************************************************)

(*
  This barrier is very sensitive to machine load.

  1- With unlimited spinning running time becomes huge when
  there are more threads than processors.
  test on conti (2 procs..)
  2- with settings 1000 * 200 for instance on conti, running
  times become erratic. Maybe because a daemon running literally
  stops the test: one proc is runnable while the other is spinning.
  With higher sizes, running times are less variable from one
  run to the other.

  Running the test under low priority (nice) also demonstrates the effect:
  running time doubles.

  However this is the best barrier we have, in normal conditions,
  especially with high SIZE_OF_TEST parameter. A benefit of
  such a high setting resides in pthread costs (lauch/join/fst_barrier) being
  amortized over a larger number of tests. It also often increases outcome
  variety, maybe by introduction of capacity cache misses.

  Limiting spinning is a BAD idea, it hinders outcome variety, even in
  normal load conditions.
 *)

      type fence = No | FenceW | Fence2

      let user_barrier_def fence =
        O.o "/* Barriers macros */" ;
        O.o "inline static void barrier_wait(unsigned int id, unsigned int k, int volatile *b) {" ;
        O.oi "if ((k % N) == id) {" ;
        O.oii "*b = 1 ;" ;
        begin match fence with
        | No -> if Cfg.cautious then O.oii "mcautious();"
        | FenceW|Fence2 -> O.oii "mbar();"
        end ;
        O.oi "} else {" ;
        O.oii "while (*b == 0) ;" ;
        begin match fence with
        | No|FenceW -> if Cfg.cautious then O.oii "mcautious();"
        | Fence2 -> O.oii "mbar();"
        end ;
        O.oi "}" ;
        O.o "}" ;
        O.o ""

      let user2_barrier_def () =
        O.o "/* Barriers macros, compact version */" ;
        O.o "inline static void barrier_wait(unsigned int id, unsigned int k, int volatile *b) {" ;
        O.oi "unsigned int x = k % (2*N);" ;
        O.oi "int free = x < N;" ;
        O.oi "unsigned idx =  k % N;" ;
        O.oi "if (idx == id) {" ;
        O.oii "b[idx] = free;" ;
        O.oi "} else {" ;
        O.oii "while (b[idx] != free);" ;
        O.oi "}" ;
        if Cfg.cautious then O.oi "mcautious();" ;
        O.o "}" ;
        O.o ""

      let dump_read_timebase () =
        if (do_verbose_barrier || do_timebase) && have_timebase then begin
          O.o "/* Read timebase */" ;
          O.o "typedef uint64_t tb_t ;" ;
          O.o "#define PTB PRIu64" ;
          Insert.insert O.o "timebase.c"
        end

      let lab_ext = if do_numeric_labels then "" else "_lab"

      let dump_tb_barrier_def () =
        let fname =
          function
            | `PPCGen
            | `PPC
            | `X86
            | `X86_64
            | `AArch64
            | `MIPS
            | `RISCV
              -> sprintf "barrier%s.c" lab_ext
            | `ARM ->
                begin match Cfg.morearch with
                | MoreArch.ARMv6K ->
                    Warn.fatal
                      "timebase barrier not supported for ARMv6K" ;
                | _ -> ()
                end ;
                sprintf "barrier%s.c" lab_ext
        in
        Insert.insert O.o (fname Cfg.sysarch)

      let dump_user_barrier_vars () = O.oi "int volatile *barrier;"

      let dump_tb_barrier_vars () =
        O.oi "sense_t barrier;" ;
        if have_timebase then O.oi "tb_t volatile next_tb;"


(*******************************************)
(* Pthread utilies, included hard barriers *)
(*******************************************)

      let pthread_barrier_def () =
        O.o DefString.pthread_barrier_def

      let dump_pthread_barrier_vars () = O.oi "barrier_t *barrier;"

      let dump_barrier_vars test =
        O.o "/* Barrier for litmus loop */" ;
        begin match barrier with
        | NoBarrier -> ()
        | Pthread -> dump_pthread_barrier_vars ()
        | User|User2|UserFence|UserFence2 -> dump_user_barrier_vars ()
        | TimeBase -> dump_tb_barrier_vars ()
        end ;
        if do_verbose_barrier then begin
          O.ox indent "/* extra verbosity */" ;
          if do_affinity then O.ox indent "int ecpu[N];" ;
          if mk_dsa test then O.ox indent "char *group;" ;
          if have_timebase then begin
            if do_timebase then
              O.ox indent "int *tb_delta[N],*tb_count[N];"
            else
              O.ox indent "tb_t *tb_start[N];"
          end
        end

      let dump_static_barrier_vars () =  match barrier with
      | NoBarrier |Pthread -> ()
      | User|UserFence|UserFence2 ->
          O.o "static volatile int barrier[SIZE_OF_MEM];"
      | User2 ->
          O.o "static volatile int barrier[NEXE*N];"
      | TimeBase ->  ()

      let barrier_def () =
        begin match barrier with
        | NoBarrier -> ()
        | _ -> O.o ""
        end ;
        begin match barrier with
        | User ->
            user_barrier_def No
        | UserFence ->
            user_barrier_def FenceW
        | UserFence2 ->
            user_barrier_def Fence2
        | User2 ->
            user2_barrier_def ()
        | TimeBase ->
            dump_tb_barrier_def ()
        | Pthread ->
            pthread_barrier_def ()
        | NoBarrier -> ()
        end

      let dump_cache_def () =
        O.o "/*************************/" ;
        O.o "/* cache flush and touch */" ;
        O.o "/*************************/" ;
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
        Insert.insert O.o "cache.c"

      let do_dump_cache_def = match Cfg.preload with
      |  NoPL|RandomPL -> false
      |  CustomPL|StaticPL|StaticNPL _ -> true

      let preload_def = match Cfg.preload with
      | NoPL|RandomPL -> fun _test -> ()
      | CustomPL ->
          fun test ->
            O.o "" ;
            O.o
              (sprintf "static char *global_names[] = {%s};"
                 (String.concat ","
                    (List.map (sprintf "\"%s\"") (get_global_names test)))) ;
            O.o "" ;
            dump_cache_def ()
      |StaticPL|StaticNPL _ ->
          fun _test -> dump_cache_def ()

      let dump_mbar_def () =
        O.o "" ;
        O.o "/* Full memory barrier */" ;
        Insert.insert O.o "mbar.c" ;
        if Cfg.cautious then begin
          O.o "" ;
          O.o "inline static void mcautious(void) { mbar(); }" ;
          O.o ""
        end ;
        if do_self then begin
          Insert.insert O.o "self.c"
        end
(* All of them *)

      let dump_threads test =
        (* mbar *)
        dump_mbar_def () ;
        (* Barrier *)
        barrier_def () ;
        (* Preload *)
        preload_def test ;
        ()

(* Topology *)


      let dump_topology test =
        let n = T.get_nprocs test in
        let module Topo =
          Topology.Make
            (struct
              let verbose = Cfg.verbose
              let nthreads = n
              let avail = match Cfg.avail with
              | None -> 0
              | Some a -> a

              let smt = Cfg.smt
              let nsockets = Cfg.nsockets
              let smtmode = Cfg.smtmode
              let mode = Mode.Std
            end) (O) in
        Topo.dump_alloc []

(*************)
(* Variables *)
(*************)

      let dump_global_type = SkelUtil.dump_global_type
      let dump_vars_types = UD.dump_vars_types
      let tag_malloc s = sprintf "malloc_%s" s
      let tag_mem s = sprintf "mem_%s" s

      let dump_vars test =
        O.o "/* Shared variables */" ;
        if do_contiguous then O.oi "void *mem;" ;
        List.iter
          (fun (s,t) ->
            let pp_t =
              let t = CType.strip_volatile t in
              let t = match t with
              | Base "mtx_t" ->
                  begin match memory with
                  | Memory.Direct -> Pointer t
                  | Memory.Indirect -> t
                  end
              | _ -> t in
              dump_global_type s t in

            match memory,t,do_staticalloc with
            | Direct,Array _,false ->
                O.fi "%s *%s;" pp_t (tag_malloc s) ;
                O.fi "%s *%s;" pp_t s
            | Direct,_,_ ->
                if do_noalign test s then  O.fi "%s *%s;" pp_t (tag_malloc s) ;
                O.fi "%s%s *%s;" pp_t indirect_star s
            | Indirect,_,_->
                O.fi "%s%s *%s;" pp_t indirect_star s)
          test.T.globals ;
        begin match memory with
        | Direct -> []
        | Indirect ->
            let r =
              List.fold_right
                (fun (b,t) k -> match t with
                | Base "mtx_t" -> k
                | _ ->
                    let pp_t = dump_global_type b (CType.strip_volatile t) in
                    let a = tag_mem b in
                    begin match t,do_staticalloc with
                    | Array _,false ->
                        O.fi "%s *%s;" pp_t (tag_malloc a) ;
                        O.fi "%s *%s;" pp_t a
                    | _,_ ->
                        if do_noalign test b then
                          O.fi "%s *%s;" pp_t (tag_malloc a) ;
                        O.fi "%s *%s;" pp_t a
                    end ;
                    if Cfg.cautious then
                      List.fold_right
                        (fun (loc,v) k -> match loc,v with
                        | A.Location_reg(p,_),Symbolic ((s,_),_) when s = a ->
                            let cpy = A.Out.addr_cpy_name a p in
                            O.fi "%s* *%s ;" (CType.dump t) cpy ;
                            (cpy,a)::k
                        | _,_ -> k)
                        test.T.init k
                    else k)
                test.T.globals [] in
            O.oi "int *_idx;" ;
            r
        end

      let dump_static_vars test =
        List.iter
          (fun (s,t) -> match t,memory with
          | Array _,Direct ->
              O.f "static %s%s %s[SIZE_OF_ALLOC];"
                (dump_global_type s t) indirect_star s
          | _,_ ->
              O.f "static %s%s %s[SIZE_OF_MEM];"
                (dump_global_type s t) indirect_star s)
          test.T.globals ;
        begin match memory with
        | Direct -> ()
        | Indirect ->
            List.iter
              (fun (a,t) ->
                O.f "static %s mem_%s[%s];"
                  (dump_global_type a t) a
                  (match t with Array _ -> "SIZE_OF_ALLOC" | _ -> "SIZE_OF_MEM");
                if Cfg.cautious then
                  List.iter
                    (fun (loc,v) -> match loc,v with
                    | A.Location_reg(p,_),Symbolic ((s,_),_)
                      when Misc.string_eq s a ->
                        let cpy = A.Out.addr_cpy_name a p in
                        O.f "static %s* %s[SIZE_OF_ALLOC];"
                          (CType.dump t) cpy ;
                        ()
                    | _,_ -> ())
                    test.T.init)
              test.T.globals ;
            O.o "static int _idx[SIZE_OF_MEM];" ;
            ()
        end ;
        ()


      let iter_outs f proc = List.iter (f proc)

      let iter_all_outs f test =
        List.iter
          (fun (proc,(_,(outs,_))) -> iter_outs f proc outs)
          test.T.code

      let dump_out_vars test =
        O.o "/* Final content of observed  registers */" ;
        iter_all_outs
          (fun proc (reg,t) ->
            O.fi "%s *%s;"
              (CType.dump t)
              (A.Out.dump_out_reg proc reg))
          test

      let dump_static_out_vars test =
        iter_all_outs
          (fun proc (reg,t) ->
            O.f "static %s %s[SIZE_OF_MEM];"
              (CType.dump t)
              (A.Out.dump_out_reg proc reg))
          test

      let fmt_outcome test locs env =
        U.fmt_outcome test
          (fun t -> match Compile.get_fmt Cfg.hexa t with
          | CType.Direct fmt ->
              if Cfg.hexa then "0x%" ^ fmt else "%" ^ fmt
          | CType.Macro fmt ->
              (if Cfg.hexa then "0x%\"" else "%\"") ^ fmt ^ "\"")
          locs env

      let get_xys =
        let rec do_rec k n = function
          | [] -> k
          | (x,_)::rem ->
              let rec do_rem k n = function
                | [] -> do_rec k n rem
                | (y,_)::rem -> do_rem ((n,(x,y))::k) (n+1) rem in
              do_rem k n rem in
        do_rec [] 0


      module DC =
        CompCond.Make(O)
          (struct
            let with_ok = true
            module C = C
            module Loc = struct
              type t = A.location
              let compare = A.location_compare
              let dump = dump_loc_param
            end
          end)

      let do_dump_cond_fun env cond =
        let find_type loc =
          let t = U.find_type loc env in
          CType.dump (CType.strip_atomic t),CType.is_ptr t in
        DC.fundef find_type cond

      let dump_cond_fun env test = do_dump_cond_fun env test.T.condition

      let dump_filter env test = match test.T.filter with
      | None -> ()
      | Some f ->
          let find_type loc =
            let t = U.find_type loc env in
            CType.dump (CType.strip_atomic t),CType.is_ptr t in
          DC.fundef_prop "filter_cond" find_type f


      let dump_cond_fun_call test dump_loc dump_val =
        DC.funcall test.T.condition dump_loc dump_val

      let register_type loc t =
          if A.arch = `X86_64 then
             match loc with
             | A.Location_reg (proc,r) -> A.typeof r
             | _ -> t
          else t

      let dump_defs_outs doc env test =
        (* If some of the output registers is of pointer type,
           we need a special function to print addresses *)
        if U.ptr_in_outs env test then begin
(*  Translation to indices *)
          let dump_test k = match memory with
          | Direct ->
              fun s ->
                O.fi "else if (v_addr == (void *)&(_a->%s[_i])) return %i;"
                  s k
          | Indirect ->
              fun s ->
                O.fi "else if (v_addr == (void *)_a->%s[_i]) return %i;"
                  s k in
          O.o "static int idx_addr(ctx_t *_a,int _i,void *v_addr) {" ;
          O.oi "if (v_addr == NULL) { return 0;}" ;
          Misc.iteri (fun k (s,_) -> dump_test (k+1) s) test.T.globals ;
          O.fi "else { fatal(\"%s, ???\"); return -1;}" doc.Name.name ;
          O.o "}" ;
          O.o "" ;
(* Pretty-print indices *)
          let naddrs = List.length test.T.globals in
          O.f "static char *pretty_addr[%i] = {\"0\",%s};"
            (naddrs+1)
            (String.concat ""
               (List.map (fun (s,_) -> sprintf "\"%s\"," s) test.T.globals)) ;
          O.o "" ;
        end ;
(* Outcome collection *)
        O.o "/**********************/" ;
        O.o "/* Outcome collection */" ;
        O.o "/**********************/" ;
(* Outcome type definition *)
        let outs = U.get_displayed_locs test in
        let nitems =
          let map =
            A.LocSet.fold
              (fun loc ->
                A.LocMap.add loc (SkelUtil.nitems (U.find_type loc env)))
              outs A.LocMap.empty in
          fun loc ->
            try A.LocMap.find loc map
            with Not_found -> assert false in
        let nouts =
          A.LocSet.fold
            (fun loc k -> nitems loc + k)
            outs 0 in
        O.f "#define NOUTS %i" nouts ;
        O.o "typedef intmax_t outcome_t[NOUTS];" ;
        O.o "" ;
        let _ =
          A.LocSet.fold
            (fun loc pos ->
              O.f "static const int %s_f = %i ;" (dump_loc_name loc) pos ;
              pos+nitems loc)
            outs 0 in
        O.o "" ;
(* Constant wrappers *)
        O.o DefString.hist_defs ;
(* Checking *)
        if do_collect_local && do_safer then begin
          O.o "static int same_hist(hist_t *h0,hist_t*h1) {" ;
          O.oi "return" ;
          O.oii "h0->n_pos == h1->n_pos && h0->n_neg == h1->n_neg &&" ;
          O.oii "same_outs(h0->outcomes,h1->outcomes);" ;
          O.o "}" ;
          O.o "" ;
        end ;
        if do_verbose_barrier then begin
          O.o "" ;
          O.o "static void pp_tb_log(pm_t *_m,ctx_t *p,int _i,int cond) {";
          O.oi "param_t *_b = p->_p;" ;
          O.oi "if (_b->verbose_barrier <= 1 && !cond) return ;";
(* Show address diffs *)
          let do_diff = Cfg.xy in
          if do_diff then begin
            let xys = get_xys test.T.globals in
            List.iter
              (fun (d,(x,y)) ->
                (match memory with
                | Indirect ->
                    O.fi "int d_%02i = abs(((void *)p->%s[_i]-(void *)p->%s[_i]));"
                | Direct ->
                    O.fi "int d_%02i = abs(((void *)&p->%s[_i]-(void *)&p->%s[_i]));")
                  d x y)
              xys
          end ;
(* Show timebase delays *)
          if not do_timebase && have_timebase then
            O.oi "tb_t _start0 = p->tb_start[0][_i];" ;
          O.oi "pm_lock(_m);" ;
          O.oi "log_error(\"%04i:\",_i);" ;
          O.oi "putc(cond ? '*' : ' ',stderr) ;" ;
          if do_diff then begin
            let xys = get_xys test.T.globals in
            List.iter
              (fun (d,(x,y)) ->
                let fmt = sprintf "%s,%s=%%i " x y in
                O.fi "log_error(\"%s\",d_%02i);" fmt d)
              xys
          end ;
          if have_timebase then begin
            O.oi "int some = 0;" ;
            O.fi  "for (int _p = %s ; _p < N ; _p++) {"
              (if do_timebase then "0" else "1") ;
            O.oii "if (some) putc(' ',stderr); else some = 1;" ;
            if do_timebase then begin
              let fmt = "%5i[%i]" in
              O.fii "log_error(\"%s\",p->tb_delta[_p][_i],p->tb_count[_p][_i]);" fmt
            end else begin
              O.oii "log_error(\"%6\"PRIi64,p->tb_start[_p][_i]-_start0);"
            end ;
            O.oi "}"
          end ;
          if do_affinity then begin
            O.oi "int *cpu = p->ecpu;" ;
            O.oi "if (cpu[0] >= 0) {" ;
            O.oii "putc(' ',stderr); putc('[',stderr);" ;
            O.oii "pp_ints(stderr,cpu,N);" ;
            O.oii "putc(']',stderr);" ;
            if do_cores then begin
              O.oii "int t[N];" ;
              O.oii
                "for (int _k = 0 ; _k < N ; _k++) t[_k] = _b->cm->cpu[cpu[_k]];" ;
              O.oii "putc(' ',stderr); putc('{',stderr);" ;
              O.oii "pp_ints(stderr,t,N);" ;
              O.oii "putc('}',stderr);" ;
              if mk_dsa test then begin
                O.oii "if (_b->aff_mode == aff_scan) {" ;
                O.oiii "log_error(\" %s\",p->group);" ;
                O.oii "}"
              end
            end ;
            O.oi "}"
          end ;
          O.oi "putc('\\n',stderr);" ;
          O.oi "pm_unlock(_m);" ;
          O.o "}" ;
          O.o ""
        end ;
(* Dumping *)
        O.o
          "static void do_dump_outcome(FILE *fhist, intmax_t *o, count_t c, int show) {" ;
        let fmt_var = fmt_outcome test outs env in
        let fmt ="\"%-6\"PCTR\"%c>" ^ fmt_var ^ "\\n\"" in
        let args =
          String.concat ","
            ("c"::
             (if ConstrGen.is_true test.T.condition then "':'"
             else "show ? '*' : ':'")::
             List.map
               (fun loc ->
                 let sloc = dump_loc_name loc in
                 match U.find_type loc env with
                 | Pointer _ -> sprintf "pretty_addr[o[%s_f]]" sloc
                 | Array (t,sz) ->
                     let rec pp_rec k =
                       if k >= sz then []
                       else sprintf "(%s)o[%s_f+%i]"
                              (CType.dump (register_type loc (CType.Base t)))
                              sloc k::pp_rec (k+1) in
                     String.concat "," (pp_rec 0)
                 | t -> sprintf "(%s)o[%s_f]" (CType.dump (register_type loc t)) sloc)
               (A.LocSet.elements outs)) in
        O.fi "fprintf(fhist,%s,%s);" fmt args ;
        O.o "}" ;
        O.o "" ;
        O.o "static void just_dump_outcomes(FILE *fhist, hist_t *h) {" ;
        O.oi "outcome_t buff ;" ;
        O.oi "dump_outs(fhist,do_dump_outcome,h->outcomes,buff,NOUTS) ;" ;
        O.o "}" ;
        O.o "" ;
        ()

(* Loops *)
      let loop_test_prelude indent ctx =
        O.fx indent "for (int _i = %ssize_of_test-1 ; _i >= 0 ; _i--) {" ctx

      and loop_test_postlude indent = O.ox indent "}"

      let loop_proc_prelude indent =
        O.ox indent "for (int _p = N-1 ; _p >= 0 ; _p--) {"

      and loop_proc_postlude indent = O.ox indent "}"

      and choose_proc_prelude indent =
        O.ox indent "for (int _p = NT-1 ; _p >= 0 ; _p--) {"

(* Safer more, checking globals *)
      let dump_check_vars env test =
        if do_check_globals then begin
          O.o "/* Check data */" ;
          O.oi "pb_t *fst_barrier;" ;
          if do_safer_write then begin
            let locs = U.get_observed_globals test in
            if not (StringSet.is_empty locs) then begin
              O.oi "po_t *s_or;" ;
              StringSet.iter
                (fun a ->
                  let loc = A.Location_global a in
                  O.fi "%s* cpy_%s[N] ;"
                    (dump_global_type a (U.find_type loc env))
                    (dump_loc_name loc))
                locs
            end
          end
        end

      let dump_static_check_vars env test =
        if do_check_globals && do_safer_write then begin
          let locs = U.get_observed_globals test in
          StringSet.iter
            (fun a ->
              let loc = A.Location_global a in
              O.f "static %s cpy_%s[N*SIZE_OF_MEM];"
                (dump_global_type a (U.find_type loc env)) (dump_loc_name loc))
            locs
        end

      let do_copy t loc1 loc2 = U.do_store t loc1 (U.do_load t loc2)

      let dump_check_globals env doc test =
        if do_check_globals then begin
(* CHECKGLOBALS *)
          O.f "/**************************************/" ;
          O.f "/* Prefetch (and check) global values */" ;
          O.f "/**************************************/" ;
          O.f "" ;
          O.f "static void check_globals(ctx_t *_a) {" ;
(* LOCALS *)
          List.iter
            (fun (a,t) ->
              let t = CType.strip_volatile t in
              match t with
              | Base "mtx_t" -> ()
              | _ ->
                  match memory with
                  | Indirect ->
                      begin match t with
                      | Pointer _ -> ()
                      | _ ->
                          O.fi "%s *mem_%s = _a->mem_%s;"
                            (dump_global_type a t) a a
                      end
                  | Direct ->
                      O.fi "%s *%s = _a->%s;" (dump_global_type a t) a a)
            test.T.globals ;
(* LOOPS *)
          loop_test_prelude indent "_a->_p->" ;

          let dump_test wrap (a,t) =
            let v = find_global_init a test in
            match memory,t with
            | (Indirect,Pointer _) ->
                let load = U.do_load t (wrap (dump_a_leftval a)) in
                sprintf "%s != %s"
                  load (dump_a_v_casted v)
            | Indirect,_ ->
                let load = U.do_load t (wrap (sprintf "mem_%s[_i]" a)) in
                sprintf "%s != %s" load (A.Out.dump_v v)
            | (Direct,_) ->
                let load = U.do_load t (wrap (dump_leftval a)) in
                sprintf "%s != %s"
                  load (dump_a_v_casted v) in
          List.iter
            (fun (s,t as x) -> match t with
            | Base "mtx_t" -> ()
            | Array (t,sz) ->
                O.fii "for (int _j = 0 ; _j < %i ; _j++) {" sz ;
                O.fiii "if (%s%s) fatal(\"%s, check_globals failed\");"
                  (if do_randompl then "rand_bit(&(_a->seed)) && " else "")
                  (dump_test (sprintf "(%s)[_j]") (s,Base t))
                  doc.Name.name ;
                O.fii "}"
            | _ ->
                O.fii "if (%s%s) fatal(\"%s, check_globals failed\");"
                  (if do_randompl then "rand_bit(&(_a->seed)) && " else "")
                  (dump_test (fun s -> s) x) doc.Name.name)
            test.T.globals ;
(* Check locals *)
          if Cfg.cautious then begin
            List.iter
              (fun (proc,(_,(outs,_))) ->
                List.iter
                  (fun (reg,t) ->
                    O.fii "if (%s%s)  fatal(\"%s, check_globals failed\");"
                      (if do_randompl then "rand_bit(&(_a->seed)) && " else "")
                      (sprintf "_a->%s[_i] != %s"
                         (A.Out.dump_out_reg proc reg)
                         (match CType.is_ptr t with
                         | false -> sentinel_of t
                         | true -> "NULL"))
                      doc.Name.name)
                  outs)
              test.T.code ;
          end ;
          loop_test_postlude indent ;
(*END LOOP*)
          O.fi "pb_wait(_a->fst_barrier);" ;
          O.f "}\n" ;

(* STABILIZE *)
          if  do_safer_write then begin
            let locs = U.get_observed_globals test in
            if not (StringSet.is_empty locs) then begin
              O.f "" ;
              O.f "static void stabilize_globals(int _id, ctx_t *_a) {" ;
              O.fi "int size_of_test = _a->_p->size_of_test;" ;
              O.f "" ;
              StringSet.iter
                (fun loc ->
                  let loc = A.Location_global loc in
                  let a = dump_loc_name loc
                  and t = U.find_type loc env in
                  O.fi "%s%s *%s = _a->%s;" (dump_global_type a t)
                    indirect_star a a ;
                  O.fi "%s **cpy_%s = _a->cpy_%s;" (dump_global_type a t) a a)
                locs ;
              O.f "" ;
              O.fi "pb_wait(_a->fst_barrier); " ;
              O.fi "for ( ; ; ) {" ;
              loop_test_prelude indent2 "" ;
              StringSet.iter
                (fun loc ->
                  let loc = A.Location_global loc in
                  let t = U.find_type loc env in
                  match t with
                  | Array (t,sz) ->
                      let ins =
                        do_copy (Base t)
                          (sprintf "cpy_%s[_id][_i][_j]" (dump_loc_name loc))
                          (sprintf "(%s)[_j]" (dump_loc loc)) in
                      O.fiii "for (int _j = 0 ; _j < %i ; _j++) %s;" sz ins
                  | _ ->
                      let ins =
                        do_copy t
                          (sprintf "cpy_%s[_id][_i]" (dump_loc_name loc))
                          (dump_loc loc) in
                      O.fiii "%s;" ins)
                locs ;
              loop_test_postlude indent2 ;
              O.fii "po_reinit(_a->s_or);" ;
              O.fii "int _found;" ;
              O.fii "%s" "int _nxt_id = (_id+1) % N;" ;
              O.fii "_found = 0;" ;
              O.fii "for (int _i = size_of_test-1 ; _i >= 0 && !_found ; _i--) {" ;
              StringSet.iter
                (fun loc ->
                  let loc = A.Location_global loc in
                  let a = dump_loc_name loc in
                  let t = U.find_type loc env in
                  let do_load = match t with
                  | Array (t,_) ->
                      fun _t a -> U.do_load (Base t) (sprintf "%s[_j]" a)
                  | _ -> U.do_load in
                  let load1 =
                    do_load t (sprintf "cpy_%s[_id][_i]" a)
                  and load2 =
                    do_load t (sprintf "cpy_%s[_nxt_id][_i]" a) in
                  match t with
                  | Array (_,sz) ->
                      O.fiii "for (int _j = 0 ; _j < %i && !_found ; _j++) {" sz ;
                      O.fiv "if (%s != %s) { _found = 1; }" load1 load2 ;
                      O.fiii "}"
                  | _ ->
                      O.fiii "if (%s != %s) { _found = 1; }" load1 load2)
                locs ;
              O.oii "}" ;
              let fmt = "%i: Stabilizing final state!\\n" in
              O.fii "if (_found) { log_error(\"%s\",_id); }" fmt ;
              O.oii "if (!po_wait(_a->s_or,_found)) return ;" ;
              O.oi "}" ;
              O.o "}" ;
              O.o "" ;
              ()

            end
          end
        end

      let dump_reinit _env test cpys =
        O.o "/*******************************************************/" ;
        O.o "/* Context allocation, freeing and reinitialization    */" ;
        O.o "/*******************************************************/" ;
        O.o "" ;
        begin match memory with
        | Indirect ->
            O.o "static void shuffle(ctx_t *_a) {" ;
            begin match
              List.filter
                (fun (_,t) -> match t with
                | Base "mtx_t" -> false | _ -> true)
                test.T.globals
            with
            | [] -> ()
            | globs ->
                O.oi "int size_of_test = _a->_p->size_of_test;" ;
                O.o "" ;
                List.iter
                  (fun (a,_) ->
                    O.oi "perm_ints(&(_a->seed),_a->_idx,size_of_test);" ;
                    loop_test_prelude indent "" ;
                    if Cfg.cautious then O.oii "mcautious();" ;
                    O.fii "_a->%s[_i] = &(_a->mem_%s[_a->_idx[_i]]);" a a ;
                    loop_test_postlude indent)
                  globs
            end ;
            if Cfg.cautious then O.oi "mcautious();" ;
            O.o "}" ;
            O.o ""
        | Direct -> ()
        end ;

(* Initialization, called once *)
        let malloc_gen sz indent name =
          O.fx  indent "_a->%s = malloc_check(%s*sizeof(*(_a->%s)));"
            name sz name

        and set_mem_gen sz indent name =
          O.fx indent "_a->%s = &%s[id*%s];" name name sz

        and set_mem indent name =
          O.fx indent "_a->%s = &%s[fst];" name name in

        let malloc  = malloc_gen "size_of_test"
        and malloc2 = malloc_gen "N"
        and malloc3  = malloc_gen "(size_of_test+1)" in

        let set_or_malloc = if do_staticalloc then set_mem else malloc in
        let set_or_malloc2 = if do_staticalloc then set_mem_gen "N" else malloc2 in

        let align_or_noalign a =
          if do_noalign test a then "do_noalign" else "do_align" in

        let set_or_malloc3 a =
          let alg = align_or_noalign a in
          if do_staticalloc then
            fun indent a ->
              O.fx indent "_a->%s = %s(&%s[fst],sizeof(*%s));" a alg a a
          else fun indent a ->
            malloc3 indent a ;
            O.fx indent "_a->%s = _a->%s;" (tag_malloc a) a ;
            O.fx indent "_a->%s = %s(_a->%s,sizeof(*_a->%s));" a alg a a
        in
        if do_self then begin
          O.o "static size_t find_ins(ins_t opcode,ins_t *p,int skip) {" ;
          O.oi "ins_t *q = p;" ;
          O.o "" ;
          O.oi "for  ( ; *q != opcode || (skip-- > 0); q++);" ;
          O.oi "return q-p+1;" ;
          O.o "}" ;
          O.o "" ;
          O.o "static size_t code_size(ins_t *p,int skip) { return find_ins(getret(),p,skip); }" ;
          O.o "" ;
          O.o "static size_t prelude_size(ins_t *p) { return find_ins(getnop(),p,0); }" ;
          O.o ""
        end ;
        O.f "static void init(ctx_t *_a%s) {"
          (if do_staticalloc then ",int id" else "") ;
        O.oi "int size_of_test = _a->_p->size_of_test;" ;
        if do_staticalloc then O.oi "int fst = id * size_of_test;" ;
        O.o "" ;
        O.oi "_a->seed = rand();" ;
        iter_all_outs
          (fun proc (reg,_) -> set_or_malloc indent (A.Out.dump_out_reg proc reg))
          test ;
(* Shared locations *)
        if do_contiguous then begin
          let sz =
            String.concat "+"
              (List.map
                 (fun (a,_) -> sprintf "sizeof(_a->%s[0])" (dump_ctx_tag a))
                 test.T.globals) in
          (* +1 for alignement *)
          let sz = sprintf "(size_of_test+1) * (%s)" sz in
          O.fx indent "_a->mem = malloc_check(%s);" sz ;
          O.ox indent "void * _am = _a->mem;" ;
          List.iter
            (fun (a,t) ->
              O.fx indent "_a->%s = do_align(_am,sizeof(%s));"
                (dump_ctx_tag a) (CType.dump t) ;
              O.fx indent "_a->%s = (%s *)_am;" (dump_ctx_tag a) (CType.dump t) ;
              O.fx indent "_am += sizeof(%s)*size_of_test;"  (CType.dump t))
            test.T.globals ;
          begin match memory with
          | Direct -> ()
          | Indirect ->
              List.iter (fun (a,_) -> malloc indent a) test.T.globals ;
              ()
          end
        end else begin
          List.iter (fun (a,t) -> match memory,t with
          | Direct,Array _ -> set_or_malloc3 a indent a
          | (Direct|Indirect),Base "mtx_t" ->
              set_or_malloc indent a ;
              O.fx indent
                "for (int _i=0 ; _i < size_of_test ; _i++) _a->%s[_i] = pm_create();" a
          | Direct,_ ->
              (if do_noalign test a then set_or_malloc3 a else set_or_malloc)
                indent a
          | Indirect,_ -> set_or_malloc indent a)
            test.T.globals ;
          begin match memory with
          | Direct -> ()
          | Indirect ->
              List.iter
                (fun (a,t) -> match t with
                | Array _ -> set_or_malloc3 a indent (sprintf "mem_%s" a)
                | Base "mtx_t" -> ()
                | _ ->
                    (if do_noalign test a then set_or_malloc3 a else set_or_malloc)
                      indent (sprintf "mem_%s" a))
                test.T.globals ;
              ()
          end
        end ;
        begin match memory with
        | Direct -> ()
        | Indirect ->
            O.oi "if (_a->_p->do_shuffle) {" ;
            set_or_malloc indent2 "_idx" ;
            loop_test_prelude indent2 "" ;
            O.oiii "_a->_idx[_i] = _i;" ;
            loop_test_postlude indent2 ;
            O.oi "}" ;
            loop_test_prelude indent "" ;
            List.iter
              (fun (a,t) -> match t with
              | Base "mtx_t" -> ()
              | _ -> O.fii "_a->%s[_i] = &(_a->mem_%s[_i]);" a a)
              test.T.globals ;
            loop_test_postlude indent ;
            ()
        end ;
        List.iter (fun (cpy,_) -> set_or_malloc indent cpy) cpys ;
        if do_check_globals then  begin
          O.oi "_a->fst_barrier = pb_create(N);" ;
        end ;
        if do_safer && do_collect_after then begin
          let locs = U.get_observed_globals test in
          if not (StringSet.is_empty locs) then begin
            O.oi "_a->s_or = po_create(N);" ;
            loop_proc_prelude indent ;
            StringSet.iter
              (fun loc ->
                let loc = A.Location_global loc in
                if do_staticalloc then
                  let loc = dump_loc_name loc in
                  O.fx indent2
                    "_a->cpy_%s[_p] = &cpy_%s[(N*id+_p)*size_of_test];"
                    loc loc
                else
                  malloc indent2 (sprintf "cpy_%s[_p]" (dump_loc_name loc)))
              locs ;
            loop_proc_postlude indent
          end
        end ;
        begin match barrier with
        | NoBarrier -> ()
        | Pthread -> O.oi "_a->barrier = barrier_create();"
        | User|UserFence|UserFence2 -> set_or_malloc indent "barrier"
        | TimeBase -> ()
        | User2 -> set_or_malloc2 indent "barrier"
        end ;
        if do_verbose_barrier && have_timebase then begin
          loop_proc_prelude indent ;
          if do_timebase then begin
            malloc indent2 "tb_delta[_p]" ;
            malloc indent2 "tb_count[_p]"
          end else begin
            malloc indent2 "tb_start[_p]"
          end ;
          loop_proc_postlude indent
        end ;
        if do_sync_macro then begin
          O.oi "_a->_scratch = malloc_check(_a->_p->max_idx*sizeof(*(_a->_scratch)));"
        end ;
        if do_self then begin
          O.oi "size_t _sz;";
          List.iter
            (fun (n,(t,_)) ->
              let open OutUtils in
              O.fi "_a->%s = code_size((ins_t *)code%i,%i);"
                (fmt_code_size n) n (A.Out.get_nrets t) ;
              O.fi "_a->%s = prelude_size((ins_t *)code%i);"
              (fmt_prelude n) n ;
              O.fi "_sz = _a->%s * size_of_test * sizeof(ins_t);"
                (fmt_code_size n) ;
              O.fi "_a->code%i = mmap_exec(_sz);" n)
            test.T.code
        end ;
        O.o "}" ;
        O.o "" ;

(* Finalization, called once *)
        let free indent name = O.fx indent "free((void *)_a->%s);" name
        and pb_free name = O.fi "pb_free(_a->%s);" name
        and po_free name = O.fi "po_free(_a->%s);" name in

        let nop_or_free =
          if do_dynamicalloc then free else fun _ _ -> () in

        O.o "static void finalize(ctx_t *_a) {" ;
        if do_contiguous then
          free indent "mem"
        else
          List.iter
            (fun (a,t) ->
              let tag = dump_ctx_tag a in
              let tag = match t with
              | Array _ -> tag_malloc tag
              | _ -> if do_noalign test a then tag_malloc tag else tag in
              begin match t with
              | Base "mtx_t" ->
                  O.fi "for (int _i = 0 ; _i < _a->_p->size_of_test ; _i++) pm_free(_a->%s[_i]);" a ;
                  nop_or_free indent a
              | _ -> nop_or_free indent tag
              end)
            test.T.globals ;
        begin match memory with
        | Direct -> ()
        | Indirect ->
            List.iter
              (fun (a,t) ->match t with
              | Base "mtx_t" -> ()
              | _ -> nop_or_free indent a)
              test.T.globals ;
            if do_dynamicalloc then begin
              O.oi "if (_a->_p->do_shuffle) {" ;
              free indent2 "_idx" ;
              O.oi "}"
            end
        end ;
        iter_all_outs
          (fun proc (reg,_) -> nop_or_free indent (A.Out.dump_out_reg proc reg))
          test ;
        if do_safer && do_collect_after then  begin
          pb_free "fst_barrier" ;
          let locs = U.get_observed_globals test in
          if not (StringSet.is_empty locs) then begin
            po_free "s_or" ;
            if do_dynamicalloc  then begin
              loop_proc_prelude indent ;
              StringSet.iter
                (fun loc ->
                  let loc = A.Location_global loc in
                  nop_or_free indent2 (sprintf "cpy_%s[_p]" (dump_loc_name loc)))
                locs ;
              loop_proc_postlude indent
            end
          end
        end ;
        begin match barrier with
        | NoBarrier -> ()
        | Pthread -> O.oi "barrier_free(_a->barrier);"
        | User|User2|UserFence|UserFence2 -> nop_or_free indent "barrier"
        | TimeBase -> ()
        end ;
        if do_verbose_barrier && have_timebase then begin
          loop_proc_prelude indent ;
          if do_timebase then begin
            free indent2 "tb_delta[_p]" ;
            free indent2 "tb_count[_p]"
          end else begin
            free indent2 "tb_start[_p]"
          end ;
          loop_proc_postlude indent
        end ;
        if do_sync_macro then free indent "_scratch" ;
        if do_self then begin
          O.oi "size_t  _sz;" ;
          O.oi "int size_of_test = _a->_p->size_of_test;" ;
          for n=0 to T.get_nprocs test-1 do
            O.fi "_sz = _a->%s * size_of_test * sizeof(ins_t);"
              (OutUtils.fmt_code_size n) ;
            O.fi "munmap_exec((void *)_a->code%i,_sz);" n
          done
        end ;
        O.o "}" ;
        O.o "" ;

(* Re-initialization, called once per run *)
        O.o "static void reinit(ctx_t *_a) {" ;
        begin match memory with
        | Indirect ->
            O.oi "if (_a->_p->do_shuffle) shuffle(_a);"
        | Direct -> ()
        end ;
        loop_test_prelude indent "_a->_p->" ;
        List.iter
          (fun (a,t) ->
            let v = A.find_in_state (A.Location_global a) test.T.init in
            if Cfg.cautious then O.oii "mcautious();" ;
            try
              let ins =
                let tag = match memory with
                | Indirect -> sprintf "_a->mem_%s[_i]"
                | Direct ->  dump_a_leftval in
                begin match t with
                | Base "mtx_t" -> raise Exit
                | CType.Array (_,sz) ->
                    let pp_a = tag a
                    and pp_v = dump_a_v_casted v in
                    let ins =
                      U.do_store t
                        (sprintf "(%s)[_j]" pp_a) pp_v in
                    sprintf "for (int _j = 0 ; _j < %i; _j++) %s" sz ins
                | _ ->
                    U.do_store t
                      (dump_a_leftval a) (dump_a_v_casted v)
                end in
              O.fii "%s;" ins
            with Exit -> ())
          test.T.globals ;
        begin if do_safer && do_collect_after then
          List.iter
            (fun (proc,(_,(outs,_))) ->
              List.iter
                (fun (reg,t) ->
                  if Cfg.cautious then O.oii "mcautious();" ;
                  O.fii "_a->%s[_i] = %s;"
                    (A.Out.dump_out_reg proc reg)
                    (match CType.is_ptr t with
                    | false -> sentinel_of t
                    | true -> "NULL"))
                outs)
            test.T.code ;
        end ;
        begin match barrier with
        | User|UserFence|UserFence2 ->
            O.oii "_a->barrier[_i] = 0;"
        | Pthread|NoBarrier|User2|TimeBase -> ()
        end ;
        if do_self then begin
          List.iteri
            (fun n _ ->
              O.fii "ins_t *_dst%i = &_a->code%i[_i*_a->code%i_sz], *_src%i=(ins_t *)code%i;" n n n n n ;
              O.fii "for (int _k = _a->code%i_sz-1 ; _k >= 0 ; _k--) _dst%i[_k] = _src%i[_k];" n n n)
            test.T.code
        end ;
        O.oi "}" ;
        if Cfg.cautious then O.oi "mcautious();" ;
        begin match barrier with
        | User2 ->
            O.o "/* Initialisation is a bit complex, due to decreasing index in litmus loop */" ;
            O.oi "for (int _i = 0 ; _i < N ; _i++) {" ;
            O.oii "_a->barrier[_i] = !(((_a->_p->size_of_test -_i -1) % (2*N)) < N);" ;
            O.oi "}"
        | TimeBase ->
            O.oi "barrier_init(&_a->barrier,N);"
        | NoBarrier|Pthread|User|UserFence|UserFence2 -> ()
        end ;
        if do_self then begin
          O.oi "const int csz = cache_line_size / sizeof(ins_t);" ;
          for n = 0 to T.get_nprocs test-1 do
            O.fi "int _sz%i = _a->%s * _a->_p->size_of_test;" n
              (OutUtils.fmt_code_size n) ;
            O.fi
              "for (int _k = 0 ; _k < _sz%i; _k += csz) selfbar(&_a->code%i[_k]);"
              n n ;
          done ;
          O.oi "isync();"
        end ;
        O.o "}" ;
        O.o ""

      let dump_templates (env:U.env) tname test =
        O.f "/***************/" ;
        O.f "/* Litmus code */" ;
        O.f "/***************/" ;
        O.f "" ;
        O.f "typedef struct {" ;
        O.fi "int th_id; /* I am running on this thread */" ;
        if do_affinity then O.fi "int *cpu; /* On this cpu */" ;
        if do_timebase && have_timebase then
          O.fi "int delay; /* time base delay */" ;
        if do_verbose_barrier_local then O.fi "pm_t *p_mutex;" ;
        O.fi "ctx_t *_a;   /* In this context */" ;
        O.f "} parg_t;" ;
        O.o "" ;
        begin match test.T.global_code with
        | [] -> ()
        | _::_ as cs -> O.f "%s\n\n" (String.concat "\n" cs)
        end ;
        if do_self then begin
          let src = test.T.src in
          let lbls =
            List.fold_left
              (fun k (_,(_,v)) -> match v with
              | Constant.Label (p,lbl) ->
                  if not
                      (List.exists
                         (fun (p0,lbl0) ->
                           Misc.int_eq p p0 && Misc.string_eq lbl lbl0)
                         k)
                  then (p,lbl)::k else k
              | _ -> k)
              [] src.MiscParser.init in
          begin match lbls with
          | [] -> ()
          | _::_ ->
              let find p lbl =
                try
                  T.find_offset src.MiscParser.prog p lbl
                with Not_found ->
                  let v = Constant.Label (p,lbl) in
                  Warn.user_error "Non-existant label %s" (A.V.pp_v v) in
              List.iter
                (fun (p,lbl) ->
                  let off = find p lbl in
                  O.f "static const int %s = %i;"
                    (OutUtils.fmt_lbl_offset p lbl) off)
                lbls ;
              O.o ""
          end
        end else begin
          List.iter
            (fun  (_,(_,v)) -> match v with
            |  Constant.Label _ ->
                Warn.user_error "label values require \"-variant self\" mode"
            | _ ->  ())
            test.T.src.MiscParser.init
        end ;
        List.iter
          (fun (proc,(out,(outregs,envVolatile))) ->
            let myenv = U.select_proc proc env
            and global_env = U.select_global env in
            if do_ascall then begin
              Lang.dump_fun O.out myenv global_env envVolatile proc out
            end ;
            let  do_collect =  do_collect_local && (do_safer || proc=0) in
            O.f "static void *P%i(void *_vb) {" proc ;
            O.fi "mbar();" ;
            if do_collect then O.fi "hist_t *hist = alloc_hist();" ;
            O.fi "parg_t *_b = (parg_t *)_vb;" ;
            O.fi "ctx_t *_a = _b->_a;" ;
            if do_affinity then begin
              O.fi "int _ecpu = _b->cpu[_b->th_id];" ;
              if do_verbose_barrier then O.fi "_a->ecpu[%i] = _ecpu;" proc ;
              let fun_name,arg =
                if do_force_affinity then
                  "force_one_affinity",sprintf ",AVAIL,_a->_p->verbose,\"%s\"" tname
                else
                  "write_one_affinity","" in
              O.fi "%s(_ecpu%s);" fun_name arg
            end ;
            if do_check_globals then begin
              O.fi "check_globals(_a);"
            end ;
            begin match barrier with
            | User|User2|UserFence|UserFence2 ->
                O.fi "int _th_id = _b->th_id;" ;
                O.fi "int volatile *barrier = _a->barrier;"
            | TimeBase ->
                O.fi "sense_t *barrier = &_a->barrier;"
            | Pthread ->
                O.fi "barrier_t *barrier = _a->barrier;"
            | NoBarrier -> ()
            end ;
            O.fi "int _size_of_test = _a->_p->size_of_test;" ;
            let prf =
              List.filter
                (fun (xproc,_,_) -> proc=xproc)
                (Prefetch.parse (U.get_prefetch_info test)) in
            if do_custom then
              O.fi "prfone_t *_prft = _a->_p->prefetch->t[%i].t;" proc ;
            if do_staticpl then begin match prf with
            | [] -> ()
            | _::_ ->
                O.oi "unsigned int _static_prefetch = _a->_p->static_prefetch;"
            end ;
            begin if Stride.some stride then
              O.oi "int _stride = _a->_p->stride;"
            end ;
            let addrs = A.Out.get_addrs out in
(*
  List.iter
  (fun a ->
  let t = find_global_type a env in
  O.fi "%s *%s = _a->%s;" (dump_global_type t) a a)
  addrs ;
 *)
            List.iter
              (fun (r,t) ->
                let name = A.Out.dump_out_reg  proc r in
                O.fi "%s *%s = _a->%s;" (CType.dump t) name name)
              outregs ;

            let iloop =
              if Stride.some stride then begin
                O.fi "for (int _j = _stride ; _j > 0 ; _j--) {" ;
                O.fii "for (int _i = _size_of_test-_j ; _i >= 0 ; _i -= _stride) {" ;
                indent3 end else begin
                  loop_test_prelude indent "_" ;
                  indent2 end in
            if do_self then begin
              let id = LangUtils.code_fun_cpy proc
              and ty = LangUtils.code_fun_type proc in
              O.fx iloop "%s %s = (%s)&_a->code%i[_a->code%i_sz*_i];"
                ty id ty proc proc
            end ;
            if do_custom then begin
              let i = iloop in
              begin match addrs with
              | [] -> ()
              | _::_ ->
                  O.fx i "prfdir_t _dir;" ;
                  Misc.iteri
                    (fun k loc ->
                      let addr = dump_a_addr loc in
                      O.fx i "_dir = _prft[%i].dir;" k ;
                      O.fx i "if (_dir == flush) cache_flush(%s);" addr ;
                      O.fx i "else if (_dir == touch) cache_touch(%s);" addr ;
                      O.fx i "else if (_dir == touch_store) cache_touch_store(%s);" addr ;
                      ())
                    addrs
              end
            end else begin
              let vars = get_global_names test in
              let iter pp =
                List.iter
                  (fun (_xproc,loc,t) ->
                    if List.mem loc vars then begin
                      try
                        let f = match t with
                        | Prefetch.Ignore -> raise Exit
                        | Prefetch.Flush -> "cache_flush"
                        | Prefetch.Touch -> "cache_touch"
                        | Prefetch.TouchStore -> "cache_touch_store" in
                        pp f (dump_a_addr loc)
                      with Exit -> ()
                    end else
                      Warn.warn_always
                        "Variable %s from prefetch is absent in test" loc)
                  prf in
              if do_staticNpl then begin
                match Cfg.preload with
                | Preload.StaticNPL Preload.One ->
                    iter
                      (fun f loc ->
                        O.fx iloop "%s(%s);" f loc)
                | Preload.StaticNPL Preload.Two ->
                    iter
                      (fun f loc ->
                        O.fx iloop "if (rand_bit(&(_a->seed))) %s(%s);" f loc)
                | _ -> assert false
              end else if do_staticpl then begin match prf with
              | [] -> ()
              | _::_ ->
                  O.fx iloop "switch (_static_prefetch) {" ;
                  let i = iloop in
                  let j = Indent.tab i in
                  O.fx i "case 0:" ;
                  O.fx j "break;" ;
                  O.o "" ;
                  O.fx i "case 1:" ;
                  iter
                    (fun f loc -> O.fx j "%s(%s);" f loc) ;
                  O.fx j "break;" ;
                  O.o "" ;
                  O.fx i "case 2:" ;
                  iter
                    (fun f loc -> O.fx j "if (rand_bit(&(_a->seed))) %s(%s);" f loc) ;
                  O.fx j "break;" ;
                  O.o "" ;
                  O.fx i "default:" ;
                  iter
                    (fun f loc ->
                      O.fx j "if (rand_k(&(_a->seed),_static_prefetch) == 0) %s(%s);"
                        f loc) ;
                  O.fx j "break;" ;
                  O.fx iloop "}" ;
              end
            end ;
            begin match barrier with
            | User|UserFence|UserFence2 ->
                O.fx iloop "barrier_wait(_th_id,_i,&barrier[_i]);" ;
                ()
            | User2 ->
                O.fx iloop "barrier_wait(_th_id,_i,barrier);"  ;
                ()
            | TimeBase ->
                if have_timebase then begin
                  O.fx iloop "_a->next_tb = read_timebase();" ;
                  O.fx iloop "barrier_wait(barrier);" ;
                  O.fx iloop "tb_t _tb0 = _a->next_tb;"
                end else begin
                  O.fx iloop "barrier_wait(barrier);" ;
                end
            | Pthread ->
                O.fx iloop "barrier_wait(%i,barrier);" proc ;
                if Cfg.cautious then O.fx iloop "mcautious();"
            | NoBarrier ->
                if Cfg.cautious then O.fx iloop "mcautious();"
            end ;

            begin match barrier with
            | TimeBase ->
                if have_timebase then begin
                  if do_verbose_barrier then begin
                    O.fx iloop "int _delta, _count=0;" ;
                    O.fx iloop "do { _delta =  read_timebase() - _tb0; _count++; } while (_delta < _b->delay);"
                  end else begin
                    O.fx iloop "int _delta;" ;
                    O.fx iloop "do { _delta =  read_timebase() - _tb0; } while (_delta < _b->delay);"
                  end
                end ;
                if Cfg.cautious then O.fx iloop "mcautious();"
            | _ ->
                if do_verbose_barrier && have_timebase then begin
                  O.fx iloop "tb_t _start = read_timebase();"
                end
            end ;
            if do_isync then begin match barrier with
            | User | User2 | UserFence | UserFence2 | TimeBase ->
                let aux = function
                  | `PPCGen
                  | `PPC ->
                      O.fx iloop "asm __volatile__ (\"isync\" : : : \"memory\");"
                  | `ARM ->
                      O.fx iloop "asm __volatile__ (\"isb\" : : : \"memory\");"
                  | `AArch64 -> assert false (* FIXME: ??? *)
                  | `X86_64 | `X86|`MIPS|`RISCV -> ()
                  | `GPU_PTX -> assert false
                in
                aux Cfg.sysarch
            | Pthread|NoBarrier -> ()
            end ;
(* Dump real code now *)
            (if do_ascall then
              let f_id =
                if do_self then LangUtils.code_fun_cpy proc else
                LangUtils.code_fun proc in
              Lang.dump_call f_id (fun _ s -> s) else Lang.dump)
              O.out (Indent.as_string iloop) myenv global_env envVolatile proc out ;
            if do_verbose_barrier && have_timebase  then begin
              if do_timebase then begin
                O.fx iloop "_a->tb_delta[%i][_i] = _delta;" proc ;
                O.fx iloop "_a->tb_count[%i][_i] = _count;" proc
              end else begin
                O.fx iloop "_a->tb_start[%i][_i] = _start;" proc
              end
            end ;

            if do_collect then begin
              let locs = U.get_displayed_locs test in
              O.fx iloop "barrier_wait(barrier);" ;
              O.fx iloop "int cond = final_ok(%s);"
                (dump_cond_fun_call test
                   (dump_ctx_loc "_a->") dump_a_addr) ;
              O.ox iloop "if (cond) { hist->n_pos++; } else { hist->n_neg++; }" ;

(* My own private outcome collection *)
              O.fx iloop "outcome_t _o;" ;
              A.LocSet.iter
                (fun loc ->
                  O.fx iloop "_o[%s_f] = %s;"
                    (dump_loc_name loc)
                    (let sloc =  dump_ctx_loc "_a->" loc in
                    match U.is_ptr loc env with
                    | false -> sloc
                    | true -> sprintf "idx_addr(_a,_i,%s)" sloc))
                locs ;
              O.ox iloop "add_outcome(hist,1,_o,cond);" ;
              if do_verbose_barrier_local && proc = 0 then begin
                O.ox iloop "if (_a->_p->verbose_barrier) {" ;
                O.ox (Indent.tab iloop) "pp_tb_log(_b->p_mutex,_a,_i,cond);" ;
                O.ox iloop "}"
              end
            end else if do_collect_local then begin
              O.fx iloop "barrier_wait(barrier);"
            end else if do_timebase && have_timebase then begin
(*          O.fx iloop "barrier_wait(barrier);"
            Problematic 4.2W on squale *)
            end  ;
            if Stride.some stride then begin
              loop_test_postlude indent2 ;
              loop_test_postlude indent
            end else begin
              loop_test_postlude indent
            end ;
            if do_safer && do_collect_after && have_observed_globals test then begin
              O.fi "stabilize_globals(%i,_a);" proc ;
            end ;
            O.oi "mbar();" ;
            let r = if do_collect then "hist" else "NULL" in
            O.fi "return %s;" r ;
            O.o "}" ;
            O.o "")
          test.T.code


      let dump_zyva doc cpys env test =
        let procs = List.map (fun (proc,_) -> proc) test.T.code in

        O.o "typedef struct {" ;
        O.oi "pm_t *p_mutex;" ;
        O.oi "pb_t *p_barrier;" ;
        O.oi "param_t *_p;" ;
        if do_prealloc then O.oi "ctx_t ctx;" ;
        if do_staticalloc || do_affinity then begin
          O.oi "int z_id;" ;
        end ;
        if do_affinity then begin
          O.oi "int *cpus;"
        end ;
        O.o "} zyva_t;" ;
        O.o "" ;
        O.f "#define NT %s" "N" ;
        O.o "" ;
        O.o "static void *zyva(void *_va) {" ;
(* Define local vars *)
        O.oi "zyva_t *_a = (zyva_t *) _va;" ;
        O.oi "param_t *_b = _a->_p;" ;
        O.oi "pb_wait(_a->p_barrier);" ;
        begin let open ThreadStyle in
        match Cfg.threadstyle with
        | Detached|Cached ->
            O.oi "op_t *op[NT];"
        | Std ->
            O.oi "pthread_t thread[NT];"
        end ;
        O.oi "parg_t parg[N];" ;
        O.fi "f_t *fun[] = {%s};"
          (String.concat ","
             (List.map (sprintf "&P%i") procs)) ;
        if do_collect_after then O.oi "hist_t *hist = alloc_hist();" ;
        if do_collect_local then begin
          if do_safer then
            O.oi "hist_t *hist0=alloc_hist(), *phist[N];"
          else
            O.oi "hist_t *hist0=alloc_hist(), *phist = NULL, *_tmp;" ;
        end ;
        begin if do_prealloc then
          O.oi "ctx_t ctx = _a->ctx;"
        else
          O.oi "ctx_t ctx;" ;
          O.oi "ctx._p = _b;"
        end ;
        O.o "" ;

(* Initialize *)
        if not do_prealloc then begin
          let call_init =
            if do_staticalloc then "init(&ctx,_a->z_id)" else "init(&ctx)" in
          if Cfg.doublealloc then O.fi "%s; finalize(&ctx);" call_init ;
          O.fi "%s;" call_init
        end ;
(* Build T preads arguments (which are constant) *)
        loop_proc_prelude indent ;
        O.oii "parg[_p].th_id = _p; parg[_p]._a = &ctx;" ;
        if do_affinity then O.oii "parg[_p].cpu = &(_a->cpus[0]);" ;
        if do_timebase && have_timebase then O.oii "parg[_p].delay = _b->delays[_p];" ;
        if do_verbose_barrier_local then
          O.oii "parg[_p].p_mutex = _a->p_mutex;" ;
        loop_proc_postlude indent ;
(* Launch cached threads, if needed *)

        O.o "" ;
(* Loop max_run times *)
        O.oi "for (int n_run = 0 ; n_run < _b->max_run ; n_run++) {" ;
        let do_break idt =
          if do_speedcheck then begin
            O.ox idt "if (_b->stop_now) {" ;
            let idt2 = Indent.tab idt in
            O.ox idt2 "break;" ;
            O.ox idt "}"
          end in

        if do_affinity then begin
          O.oii "if (_b->aff_mode == aff_random) {" ;
          O.oiii "pb_wait(_a->p_barrier);" ;
          do_break indent3 ;
          O.oiii "if (_a->z_id == 0) perm_prefix_ints(&ctx.seed,_a->cpus,_b->ncpus_used,_b->ncpus);" ;
          O.oiii "pb_wait(_a->p_barrier);" ;
          if mk_dsa test then begin
            O.oii "} else if (_b->aff_mode == aff_scan) {" ;
            O.oiii "pb_wait(_a->p_barrier);" ;
            do_break indent3 ;
            O.oiii "int idx_scan = n_run % SCANSZ;" ;
            if do_verbose_barrier then O.oiii "ctx.group = group[idx_scan];" ;
            O.oiii "int *from =  &cpu_scan[SCANLINE*idx_scan];" ;
            O.oiii "from += N*_a->z_id;" ;
            O.oiii "for (int k = 0 ; k < N ; k++) _a->cpus[k] = from[k];" ;
            O.oiii "pb_wait(_a->p_barrier);"
          end ;
          O.oii "} else {" ;
          do_break indent3 ;
          O.oii "}" ;
          ()
        end else begin
          do_break indent2
        end ;
        O.oii
          "if (_b->verbose>1) fprintf(stderr,\"Run %i of %i\\r\", n_run, _b->max_run);" ;
        O.oii "reinit(&ctx);"  ;
(* Start/join threads *)

        begin match launch with
        | Changing -> O.oii "if (_b->do_change) perm_funs(&ctx.seed,fun,N);"
        | Fixed -> ()
        end ;

        choose_proc_prelude indent2 ;
        if Cfg.cautious then O.oiii "mbar();" ;
        begin let open ThreadStyle in
        match Cfg.threadstyle with
        | Detached ->
            O.oiii "op[_p] = launch_detached(fun[_p],&parg[_p]);"
        | Cached ->
            O.oiii "op[_p] = launch_cached(fun[_p],&parg[_p]);"
        | Std ->
            O.oiii "launch(&thread[_p],fun[_p],&parg[_p]);"
        end ;
        loop_proc_postlude indent2 ;
        if Cfg.cautious then O.oii "mbar();" ;

        begin match launch with
        | Changing ->
            begin let open ThreadStyle in
            match Cfg.threadstyle with
            | Std ->
                O.oii "if (_b->do_change) perm_threads(&ctx.seed,thread,NT);"
            | Detached|Cached -> ()
            end
        | Fixed -> ()
        end ;

        let set_result i _p call =
          if do_collect_local then
            if do_safer then
              O.fx i "phist[%s] = %s" _p call
            else begin
              O.fx i "_tmp = %s" call ;
              O.fx i "if (_tmp) phist = _tmp;" ;
              ()
            end
          else
            O.fx i "%s" call in
        choose_proc_prelude indent2 ;
        if Cfg.cautious then O.oiii "mbar();" ;
        begin let open ThreadStyle in
        match Cfg.threadstyle with
        | Detached|Cached ->
            set_result indent3 "_p" "join_detached(op[_p]);"
        | Std ->
            set_result indent3 "_p" "join(&thread[_p]);"
        end ;
        loop_proc_postlude indent2 ;
        if Cfg.cautious then O.oii "mbar();" ;

(* Check histograms collectect by test threads *)
        if do_collect_local then begin
          if do_safer then begin
            O.oii "/* Check local histograns */" ;
            O.oii "for (int _p = 0 ; _p < N-1 ; _p++) {" ;
            O.fiii
              "if (!same_hist(phist[_p],phist[_p+1])) fatal(\"%s, check hist\") ;"
              doc.Name.name ;
            O.oii "}" ;
            O.oii "merge_hists(hist0,phist[0]);" ;
            O.oii "for (int _p = 0 ; _p < N-1 ; _p++) {" ;
            O.oiii "free_hist(phist[_p]);" ;
            O.oii "}"
          end else
            O.oii "merge_hists(hist0,phist);"
        end ;
(* Log final states *)
        if do_collect_after then begin
          O.oii "/* Log final states */" ;
          loop_test_prelude indent2 "_b->" ;
          let locs = U.get_observed_locs test in
          let loc_arrays =
            A.LocSet.fold
              (fun loc k -> match loc with
              | A.Location_deref (s,_) -> StringSet.add s  k
              | A.Location_global s ->
                  let t = U.find_type loc env in
                  let t = CType.strip_attributes t in
                  begin match t with
                  | CType.Array _ ->  StringSet.add s  k
                  | _ -> k
                  end
              | A.Location_reg _ -> k)
              locs StringSet.empty in
(* Make copies of final locations *)
          if Cfg.cautious && not (A.LocSet.is_empty locs) then begin
            O.oiii "mcautious();"
          end ;
          (* Arrays first (because of deref just below) *)
          StringSet.iter
            (fun s ->
              let loc = A.Location_global s in
              match  CType.strip_attributes (U.find_type loc env) with
              | CType.Array (t,_) ->
                  O.fiii "%s *%s = %s;"
                    t
                    (dump_loc_copy loc)
                    (U.do_load (CType.Base t) (dump_ctx_loc "ctx." loc))
              | t ->
                  Warn.user_error "array type expected for '%s', type %s found"
                    s (CType.dump t))
            loc_arrays ;
          (* Rest of locs *)
          A.LocSet.iter
            (fun loc ->
              let t = U.find_type loc env in
              let t = CType.strip_attributes t in
              begin match t with
              | CType.Array _ -> ()
              | _ ->
                  O.fiii "%s %s = %s;"
                    (CType.dump t)
                    (dump_loc_copy loc)
                    (let loc = match loc with
                    | A.Location_deref (s,idx) ->
                        sprintf "%s[%i]"
                          (dump_loc_copy (A.Location_global s))
                          idx
                    | _ -> dump_ctx_loc "ctx." loc in
                    U.do_load t loc)
              end ;
              if Cfg.cautious then O.oiii "mcautious();")
            locs ;
          O.oiii "outcome_t o;" ;
          O.oiii "int cond;" ;
          O.o "" ;
(* check globals against stabilized value *)
          if do_safer && do_collect_after then begin
            let locs =  U.get_observed_globals test in
            StringSet.iter
              (fun loc ->
                let loc = A.Location_global loc in
                let t = U.find_type loc env in
                loop_proc_prelude indent3 ;
                let pp_test i e1 e2 =
                  O.fx i
                    "if (%s != %s) fatal(\"%s, global %s unstabilized\") ;"
                    e1 e2
                    (doc.Name.name)  (dump_loc_name loc) in
                begin match t with
                | Array (t,sz) ->
                    O.fiv "for (int _j = 0 ; _j < %i ; _j++) {" sz ;
                    pp_test indent5
                      (sprintf "%s[_j]" (dump_loc_copy loc))
                      (U.do_load (Base t)
                         (sprintf "ctx.cpy_%s[_p][_i][_j]" (dump_loc_name loc))) ;
                    O.fiv "}"
                | _ ->
                    pp_test indent4
                      (dump_loc_copy loc)
                      (U.do_load t
                         (sprintf "ctx.cpy_%s[_p][_i]" (dump_loc_name loc)))
                end ;
                loop_proc_postlude indent3)
              locs ;
          end ;
(* Cautious check of indirect mode *)
          List.iter
            (fun (cpy,loc) ->
              O.fiii
                "if (ctx.%s[_i] != ctx.%s[_i]) fatal(\"%s, address copy %s is wrong\") ; "
                cpy loc doc.Name.name cpy)
            cpys ;
(* Check filter *)
          let indent =
            match test.T.filter with
            | None -> indent3
            | Some f ->
                O.fiii "if (%s) {"
                  (DC.funcall_prop "filter_cond" f dump_loc_copy dump_ctx_addr) ;
                indent4 in
(* Compute final condition *)
          O.fx indent "cond = final_ok(%s);"
            (dump_cond_fun_call test dump_loc_copy dump_ctx_addr) ;
(* Save outcome *)
          A.LocSet.iter
            (fun loc ->
              let t = CType.strip_attributes (U.find_type loc env) in
              match t with
              | Array (_,sz) ->
                  let ins =
                    sprintf
                      "o[%s_f+_j] = %s[_j]"
                      (dump_loc_name loc)
                      (dump_loc_copy loc) in
                  O.fx indent "for (int _j = 0 ; _j < %i ; _j++) %s;" sz ins
              | _ ->
                  O.fx indent "o[%s_f] = %s;"
                    (dump_loc_name loc)
                    (if CType.is_ptr t then
                      sprintf "idx_addr(&ctx,_i,%s)" (dump_loc_copy loc)
                    else
                      dump_loc_copy loc))
            (U.get_displayed_locs test) ;
          O.ox indent "add_outcome(hist,1,o,cond);" ;
          if mk_dsa test then begin
            O.ox indent
              "if (_b->aff_mode == aff_scan && _a->cpus[0] >= 0 && cond) {" ;
            let ni = Indent.tab indent in
            O.ox ni "pm_lock(_a->p_mutex);" ;
            O.ox ni "ngroups[n_run % SCANSZ]++;" ;
            O.ox ni "pm_unlock(_a->p_mutex);" ;
            O.ox indent
              "} else if (_b->aff_mode == aff_topo && _a->cpus[0] >= 0 && cond) {" ;
            O.ox ni "pm_lock(_a->p_mutex);" ;
            O.ox ni "ngroups[0]++;" ;
            O.ox ni "pm_unlock(_a->p_mutex);" ;
            O.ox indent "}"
          end ;

(****************)
          O.ox indent "if (cond) { hist->n_pos++; } else { hist->n_neg++; }" ;
          if (do_verbose_barrier) then begin
            O.ox indent "if (_b->verbose_barrier) {" ;
            O.ox (Indent.tab indent) "pp_tb_log(_a->p_mutex,&ctx,_i,cond);" ;
            O.ox indent "}"
          end ;
          begin match test.T.filter with
          | None -> () | Some _ -> O.fiii "}"
          end ;
          loop_test_postlude indent2 ;
          ()
        end ;
(* Cautious check of indirect pointers *)
        if Cfg.cautious then begin match Cfg.memory with
        | Indirect ->
            let locs = get_global_names test in
            List.iter
              (fun loc ->
                O.fii
                  "if (!check_shuffle(ctx.%s,ctx.%s,_b->size_of_test))"
                  loc (dump_ctx_tag loc) ;
                O.fiii
                  "fatal(\"%s, check_shuffle for %s\");"
                  doc.Name.name loc)

              locs
        | Direct -> ()
        end ;
(* Check histogram against sum of local histogram *)
        if do_collect_local && do_collect_after && do_safer then begin
          O.oii "/* Check histogram against sum of local histogram */" ;
          O.oii "if (!same_hist(hist,hist0)) {" ;
          O.oiii "log_error(\"Zyva histogram:\\n\");" ;
          O.oiii "just_dump_outcomes(stderr,hist);" ;
          O.oiii "log_error(\"Local histogram:\\n\");" ;
          O.oiii "just_dump_outcomes(stderr,hist0);" ;
          O.fiii "fatal(\"%s, check summed hist\");" doc.Name.name ;
          O.oii "}"
        end ;
        if do_speedcheck then begin
          let cond =
            let hist =
              if do_collect_after then "hist" else "hist0" in
            if ConstrGen.is_existential test.T.condition then
              sprintf "%s->n_pos > 0" hist
            else
              sprintf "%s->n_neg > 0" hist in
          O.fii "if (_b->speedcheck && %s) {" cond ;
          O.oiii "_b->stop_now = 1;" ;
          O.oii "}"
        end ;
        O.oi "}" ;

        O.o "" ;
        if do_collect_both then O.oi "free_hist(hist0);" ;
        O.oi "finalize(&ctx);" ;
        if do_collect_after || do_collect_both then
          O.oi "return hist;"
        else
          O.oi "return hist0;" ;
        O.o "}" ;
        O.o "" ;
        ()


      let dump_def_ctx env test =
        O.o "/**********************/" ;
        O.o "/* Context definition */" ;
        O.o "/**********************/" ;
        O.o "" ;
        dump_vars_types test ;
        O.o "typedef struct {" ;
        let cpys = dump_vars test in
        dump_out_vars test ;
        dump_check_vars env test ;
        dump_barrier_vars test ;
        O.o "/* Instance seed */" ;
        O.oi "st_t seed;" ;
        if do_sync_macro then O.oi "char *_scratch;" ;
        O.o "/* Parameters */" ;
        O.oi "param_t *_p;" ;
        if do_self then begin
          let nprocs = T.get_nprocs test in
          O.o "/* Code memory */" ;
          for n = 0 to nprocs-1 do
            O.fi "size_t code%i_sz,%s;" n (OutUtils.fmt_prelude n) ;
            O.fi "ins_t *code%i;" n
          done
        end ;
        O.o "} ctx_t;" ;
        O.o "" ;
        if do_staticalloc then begin
          O.o "/* Statically allocated memory */" ;
          dump_static_vars test ;
          dump_static_out_vars test ;
          dump_static_barrier_vars () ;
          dump_static_check_vars env test ;
          O.o ""
        end ;
        cpys

      let check_speedcheck i f =
        if do_speedcheck then begin
          O.fx i "%s" "if (!prm.speedcheck) {" ;
          f (Indent.tab i) ;
          O.fx i "}"
        end else f i

      let check_speedcheck_filter test i f = match test.T.filter with
      | Some _ -> ()
      | None -> check_speedcheck i f

      let dump_run doc _env test =
(* Custom affinity information *)
        let dca,ca = mk_dca test in
        let affi =
          if dca then begin
            let cs,ne =
              try Affi.compute (LexAffinity.coms ca)
              with LexAffinity.Error msg -> Warn.user_error "%s" msg in
            Misc.iteri
              (fun k c ->
                O.f "static int color_%i[] = {%s, -1};" k
                  (String.concat ", " (List.map (sprintf "%i") c)))
              cs ;
            O.f "static int *color[] = {%s, NULL};"
              (String.concat ", "
                 (Misc.mapi
                    (fun k _c -> sprintf "color_%i" k)
                    cs)) ;
            begin match ne with
            | [] -> O.o "static int diff[] = {-1};"
            | _ ->
                O.f "static int diff[] = {%s, -1};"
                  (String.concat ", "
                     (List.map (fun (x,y) -> sprintf "%i, %i" x y) ne))
            end ;
            O.o "" ;
            Some (cs,ne)
          end else
            None in
        UD.postlude doc test affi (mk_dsa test) [] ;
        O.f "static %s run(cmd_t *cmd,cpus_t *def_all_cpus,FILE *out) {"
          (if Cfg.exit_cond then "int" else "void") ;
(* Prelude *)
        if do_vp then O.oi "if (cmd->prelude) prelude(out);" ;
(* Void cache operation to avoid warnings *)
        if do_dump_cache_def then begin
          O.o "/* Void cache operation to avoid warnings */" ;
          O.oi "cache_flush(cmd);" ;
          O.oi "cache_touch(cmd);" ;
          O.oi "cache_touch_store(cmd);" ;
        end ;
(* Starting time *)
        O.oi "tsc_t start = timeofday();" ;
(* Parameters recorded in param_t structure *)
        O.oi "param_t prm ;" ;
        O.o "/* Set some parameters */" ;
        O.oi "prm.verbose = cmd->verbose;" ;
        if do_verbose_barrier then
          O.oi "prm.verbose_barrier = cmd->verbose_barrier;" ;
        O.oi "prm.size_of_test = cmd->size_of_test;" ;
        O.oi "prm.max_run = cmd->max_run;" ;
        begin if Stride.some stride then
          O.oi "prm.stride = cmd->stride > 0 ? cmd->stride : N ;"
        end ;
        if do_speedcheck then
          O.oi "prm.speedcheck = cmd->speedcheck; prm.stop_now = 0;" ;
        begin match memory with
        | Direct -> ()
        | Indirect -> O.oi "prm.do_shuffle = cmd->shuffle;"
        end ;
        begin match launch with
        | Fixed -> ()
        | Changing ->
            if mk_dsa test then begin
              O.oi "int ntopo = -1;" ;
              O.oi "if (cmd->aff_mode == aff_topo) {" ;
              O.oii "ntopo = find_string(group,SCANSZ,cmd->aff_topo);" ;
              O.oii "if (ntopo < 0) {" ;
              O.oiii "log_error(\"Bad topology %s, reverting to scan affinity\\n\",cmd->aff_topo);" ;
              O.oiii "cmd->aff_mode = aff_scan; cmd->aff_topo = NULL;" ;
              O.oii "}" ;
              O.oi "}"
            end ;
            if dca || mk_dsa test then begin
              O.oi "prm.do_change = cmd->aff_mode != aff_custom && cmd->aff_mode != aff_scan && cmd->aff_mode != aff_topo;"
            end else begin
              O.oi "prm.do_change = 1;"
            end ;
            O.oi "if (cmd->fix) prm.do_change = 0;"
        end ;
        if do_timebase && have_timebase then
          O.oi "prm.delays = cmd->delta_tb->t;" ;
        if do_custom then  O.oi "prm.prefetch = cmd->prefetch;" ;
        if do_staticpl then  O.oi "prm.static_prefetch = (unsigned int)cmd->static_prefetch;" ;
        if do_cores then begin
          O.fi "prm.cm = coremap_%s(def_all_cpus->sz,%i);"
            (Smt.pp smtmode) smt
        end ;
        if Cfg.timeloop > 0 then O.oi "prm.max_loop = cmd->max_loop;" ;
        if do_sync_macro then O.oi "prm.max_idx = SYNC_K * cmd->sync_n;\n" ;
        O.o "/* Computes number of test concurrent instances */" ;
        if do_affinity then begin
          O.oi "int n_avail = cmd->avail > 0 ? cmd->avail : cmd->aff_cpus->sz;";
          let fmt = "Warning: avail=%i, available=%i\\n" in
          O.fi "if (n_avail >  cmd->aff_cpus->sz) log_error(\"%s\",n_avail, cmd->aff_cpus->sz);" fmt
        end else begin
          O.oi "int n_avail = cmd->avail;"
        end ;
        O.oi "int n_exe;" ;
        O.oi "if (cmd->n_exe > 0) {" ;
        O.oii "n_exe = cmd->n_exe;" ;
        O.oi "} else {" ;
        O.oii "n_exe = n_avail < N ? 1 : n_avail / N;" ;
        O.oi "}" ;
        if do_affinity then begin
          O.o "/* Set affinity parameters */" ;
          O.oi "cpus_t *all_cpus = cmd->aff_cpus;" ;
          O.oi "int aff_cpus_sz = cmd->aff_mode == aff_random ? max(all_cpus->sz,N*n_exe) : N*n_exe;" ;
          O.oi "int aff_cpus[aff_cpus_sz];" ;
          O.oi "prm.aff_mode = cmd->aff_mode;" ;
          O.oi "prm.ncpus = aff_cpus_sz;" ;
          O.oi "prm.ncpus_used = N*n_exe;"
        end ;
        O.o "/* Show parameters to user */" ;
        O.oi "if (prm.verbose) {" ;
        let fmt = doc.Name.name ^ ": n=%i, r=%i, s=%i" in
        O.fii "log_error( \"%s\",n_exe,prm.max_run,prm.size_of_test);" fmt ;
        if Stride.some stride then begin
          let fmt = ", st=%i" in
          O.fii "log_error(\"%s\",prm.stride);" fmt
        end ;
        begin match memory with
        | Direct -> ()
        | Indirect ->
            let fmt = ", %crm" in
            O.fii "log_error(\"%s\",prm.do_shuffle?'+':'-');" fmt
        end ;
        if do_affinity then begin
          O.oii "if (cmd->aff_mode == aff_incr) {" ;
          let fmt = ", i=%i" in
          O.fiii "log_error( \"%s\",cmd->aff_incr);" fmt ;
          O.oii "} else if (cmd->aff_mode == aff_random) {" ;
          O.oiii "log_error(\", +ra\");" ;
          O.oii "} else if (cmd->aff_mode == aff_custom) {" ;
          O.oiii "log_error(\", +ca\");" ;
          O.oii "} else if (cmd->aff_mode == aff_scan) {" ;
          O.oiii "log_error(\", +sa\");" ;
          O.oii "}" ;
          O.oii "log_error(\", p='\");" ;
          O.oii "cpus_dump(stderr,cmd->aff_cpus);" ;
          O.oii "log_error(\"'\");"
        end ;
        if do_timebase && have_timebase then begin
          O.oii "log_error(\", tb=\");" ;
          O.oii "ints_dump(stderr,cmd->delta_tb);" ;
          O.oii "log_error(\"'\");"
        end ;
        if do_custom then begin
          O.oii "log_error(\", prf='\");" ;
          O.oii "prefetch_dump(stderr,cmd->prefetch);" ;
          O.oii "log_error(\"'\");"
        end ;
        if do_staticpl then begin
          O.oii "log_error(\", prs=%i\",cmd->static_prefetch);"
        end ;
        if Cfg.timeloop > 0 then begin
          let fmt = ", l=%i" in
          O.fii "log_error(\"%s\",prm.max_loop);" fmt
        end ;
        O.oii "log_error(\"\\n\");" ;
        if dca then begin
          O.oii "if (prm.verbose > 1 && prm.cm) {" ;
          O.oiii"log_error(\"logical proc -> core: \");" ;
          O.oiii "cpus_dump(stderr,prm.cm);" ;
          O.oiii "log_error(\"\\n\");" ;
          O.oii "}"
        end ;
        O.oi "}" ;
(* check there is enough static space *)
        if do_staticalloc then begin
          O.oi "if (n_exe * prm.size_of_test > SIZE_OF_MEM) {" ;
          O.oii "log_error(\"static memory is too small for  parameters n=%i and s=%i\\n\",n_exe,prm.size_of_test);" ;
          O.oii "exit(2);" ;
          O.oi "}"
        end ;
        if do_affinity then begin
          O.oi "if (cmd->aff_mode == aff_random) {" ;
          O.oii "for (int k = 0 ; k < aff_cpus_sz ; k++) {" ;
          O.oiii "aff_cpus[k] = all_cpus->cpu[k % all_cpus->sz];" ;
          O.oii "}" ;
          if dca then begin
            O.oi "} else if (cmd->aff_mode == aff_custom) {" ;
            O.oii "st_t seed = 0;" ;
            O.oii "custom_affinity(&seed,prm.cm,color,diff,all_cpus,n_exe,aff_cpus);" ;
            O.oii "if (prm.verbose) {" ;
            O.oiii"log_error(\"thread allocation: \\n\");" ;
            O.oiii "cpus_dump_test(stderr,aff_cpus,aff_cpus_sz,prm.cm,N);" ;
            O.oii "}"
          end ;
          if mk_dsa test then begin
            O.oi "} else if (cmd->aff_mode == aff_topo) {" ;
            O.oii "int *from = &cpu_scan[ntopo * SCANLINE];" ;
            O.oii "for (int k = 0 ; k < aff_cpus_sz ; k++) {" ;
            O.oiii "aff_cpus[k] = *from++;" ;
            O.oii "}" ;
          end ;
          O.oi "}" ;
        end ;

(*********************)
(* Spawn experiments *)
(*********************)
        O.oi "hist_t *hist = NULL;" ; (* Place holder for last zyva call result *)
        O.oi "int n_th = n_exe-1;" ;
        begin let open ThreadStyle in
        match Cfg.threadstyle with
        | Detached|Cached ->
            O.oi "op_t *op[n_th];"
        | Std ->
            O.oi "pthread_t th[n_th];"
        end ;
        O.oi "zyva_t zarg[n_exe];" ;
        O.oi "pm_t *p_mutex = pm_create();" ;
        O.oi "pb_t *p_barrier = pb_create(n_exe);" ;
(* Compute affinity settings *)
        if do_affinity then begin
          O.oi "int next_cpu = 0;" ;
          O.oi "int delta = cmd->aff_incr;" ;
          O.oi "if (delta <= 0) {" ;
          O.oii "for (int k=0 ; k < all_cpus->sz ; k++) all_cpus->cpu[k] = -1;" ;
          O.oii "delta = 1;" ;
          O.oi "} else {" ;
          O.oii "delta %= all_cpus->sz;" ;
          O.oi"}" ;
          O.oi "int start_scan=0, max_start=gcd(delta,all_cpus->sz);" ;
          O.oi "int *aff_p = aff_cpus;"
        end ;
(* launching loop *)
        O.oi "for (int k=0 ; k < n_exe ; k++) {" ;
        O.oii "zyva_t *p = &zarg[k];" ;
        O.oii "p->_p = &prm;" ;
        O.oii "p->p_mutex = p_mutex; p->p_barrier = p_barrier; " ;
        if do_prealloc then begin
          O.oii "p->ctx._p = &prm;" ;
          if Cfg.doublealloc then O.oi "init(&p->ctx); finalize(&p->ctx);" ;
          O.oii "init(&p->ctx);"
        end ;
        if do_staticalloc || do_affinity then begin
          O.oii "p->z_id = k;"
        end ;
        if do_affinity then begin
          O.oii "p->cpus = aff_p;" ;
          O.oii "if (cmd->aff_mode != aff_incr) {" ;
          O.oiii "aff_p += N;" ;
          O.oii "} else {" ;
          O.oiii "for (int i=0 ; i < N ; i++) {" ;
          O.oiv "*aff_p = all_cpus->cpu[next_cpu]; aff_p++;" ;
          O.oiv "next_cpu += delta; next_cpu %= all_cpus->sz;" ;
          O.oiv "if (next_cpu == start_scan) {" ;
          O.ov "start_scan++ ; start_scan %= max_start;" ;
          O.ov  "next_cpu = start_scan;" ;
          O.oiv "}" ;
          O.oiii "}" ;
          O.oii "}"
        end ;
        O.oii "if (k < n_th) {" ;
        begin let open ThreadStyle in
        match Cfg.threadstyle with
        | Detached ->
            O.oiii "op[k] = launch_detached(zyva,p);"
        | Cached ->
            O.oiii "op[k] = launch_cached(zyva,p);"
        | Std ->
            O.oiii "launch(&th[k],zyva,p);"
        end ;
        O.oii "} else {" ;
        O.oiii "hist = (hist_t *)zyva(p);" ;
        O.oii "}" ;
        O.oi "}" ;
(* end of loop *)

(********)
(* Join *)
(********)
        O.o "" ;
        O.oi "count_t n_outs = prm.size_of_test; n_outs *= prm.max_run;" ;
(* join loop *)
        O.oi "for (int k=0 ; k < n_th ; k++) {" ;
        begin let open ThreadStyle in
        match Cfg.threadstyle with
        | Detached|Cached ->
            O.oii "hist_t *hk = (hist_t *)join_detached(op[k]);"
        | Std ->
            O.oii "hist_t *hk = (hist_t *)join(&th[k]);"
        end ;
        check_speedcheck_filter test indent2
          (fun i ->
            O.ox i
              "if (sum_outs(hk->outcomes) != n_outs || hk->n_pos + hk->n_neg != n_outs) {" ;
            O.oy i (sprintf "fatal(\"%s, sum_hist\");" doc.Name.name) ;
            O.ox i "}") ;
        O.oii "merge_hists(hist,hk);" ;
        O.oii "free_hist(hk);" ;
        O.oi "}" ;
(* end of join loop *)
        if do_affinity then O.oi"cpus_free(all_cpus);" ;
        O.oi "tsc_t total = timeofday() - start;" ;
        O.oi "pm_free(p_mutex);" ;
        O.oi "pb_free(p_barrier);" ;
        O.o "" ;
        O.oi "n_outs *= n_exe ;" ;
        check_speedcheck_filter test indent
          (fun i ->
            O.ox i
              "if (sum_outs(hist->outcomes) != n_outs || hist->n_pos + hist->n_neg != n_outs) {"  ;
            O.oy i (sprintf "fatal(\"%s, sum_hist\") ;" doc.Name.name);
            O.ox i "}") ;
        O.oi "count_t p_true = hist->n_pos, p_false = hist->n_neg;" ;
(* Print results *)
        let call_post = "postlude(out,cmd,hist,p_true,p_false,total)" in
        begin if Cfg.exit_cond then
          O.fi "int cond = %s;"  call_post
        else
          O.fi "%s;" call_post
        end ;
        O.oi "free_hist(hist);" ;
        if do_cores then  O.oi "cpus_free(prm.cm);" ;
        if Cfg.exit_cond then O.oi "return cond;" ;
        O.o "}" ;
        O.o "" ;
        ()

(* Main *)

      let dump_main doc _env test =
        let dca,_ca = mk_dca test in
        O.o "" ;
(* Static list of logical processors *)
        begin match Cfg.logicalprocs with
        | Some procs when do_affinity ->
            O.f "static int logical_procs[] = {%s};" (LexSplit.pp_ints procs) ;
            O.o "" ;
            ()
        | None|Some _ -> ()
        end ;
        let outchan =
          match driver with
          | Driver.Shell ->
              O.o "int main(int argc, char **argv) {" ;
              "stdout"
          | Driver.C|Driver.XCode ->
              O.f "int %s(int argc, char **argv, FILE *out) {"
                (MyName.as_symbol doc) ;
              "out" in
        let alloc_def_all_cpus =
          if do_affinity then begin
            begin match Cfg.logicalprocs with
            | None ->
                if do_force_affinity then
                  O.oi "cpus_t *def_all_cpus = read_force_affinity(AVAIL,0);"
                else
                  O.oi "cpus_t *def_all_cpus = read_affinity();"
            | Some procs ->
                O.fi "cpus_t *def_all_cpus = cpus_create_init(%i,logical_procs);"
                  (List.length procs)
            end ;
            true
          end else begin
            O.oi "cpus_t *def_all_cpus = NULL;" ;
            false
          end in
        if do_self then begin
          O.oi "cache_line_size = getcachelinesize();"
        end ;
        if alloc_def_all_cpus then begin
          O.oi "if (def_all_cpus->sz < N) {" ;
          if Cfg.limit then begin
            O.oii "cpus_free(def_all_cpus);" ;
            O.oii "return EXIT_SUCCESS;" ;
          end else begin
            let fmt = "Warning: ncores found=%i, nthreads=%i\\n" in
            O.fii "log_error(\"%s\",def_all_cpus->sz,N);" fmt
          end ;
          O.oi "}"
        end ;
        if do_timebase && have_timebase then begin
          O.fi "int delta_t[] = {%s};"
            (String.concat ","
               (List.map (fun _ -> "DELTA_TB") test.T.code)) ;
          O.oi "ints_t delta_tb = { N, delta_t };" ;
        end ;
        let vars = get_global_names test
        and nprocs = T.get_nprocs test in
        if do_custom then begin
          List.iter
            (fun (i,(out,_)) ->
              let addrs = A.Out.get_addrs out in
              O.fi "prfone_t _prf_t_%i[] = { %s };" i
                (String.concat ", "
                   (List.map
                      (fun loc ->
                        sprintf "{ global_names[%i], none, }"
                          (find_index loc vars))
                      addrs)) ;
              O.fi "prfproc_t _prf_%i = { %i, _prf_t_%i}; "
                i (List.length addrs) i)
            test.T.code ;
          O.fi "prfproc_t _prf_procs_t[] = { %s };"
            (String.concat ", "
               (List.map
                  (fun k -> sprintf "_prf_%i" k)
                  (Misc.interval 0 nprocs))) ;
          O.fi "prfdirs_t _prefetch = { %i, _prf_procs_t }; " nprocs ;
          begin
            try
              let prf = List.assoc "Prefetch" test.T.info in
              O.fi "char _prefetch_txt[] = {%s};"
                (String.concat ","
                   (List.map
                      (sprintf "'%c'")
                      (Misc.explode prf)@["'\\0'"])) ;
              O.fi
                "if (!parse_prefetch(_prefetch_txt,&_prefetch)) fatal(\"%s, parse_prefetch\");" doc.Name.name ;
              ()
            with Not_found -> ()
          end
        end ;
        O.fi "cmd_t def = { 0, NUMBER_OF_RUN, SIZE_OF_TEST, STRIDE, AVAIL, 0, %s, %s, %i, %i, AFF_INCR, def_all_cpus, NULL, %i, %s, %s, %s, %s, %s, %s, %s, %s};"
          (if do_sync_macro then "SYNC_N" else "0")
          (match affinity with
          | Affinity.No -> "aff_none"
          | Affinity.Incr _ -> "aff_incr"
          | Affinity.Random -> "aff_random"
          | Affinity.Scan ->
              if mk_dsa test then "aff_scan"
              else begin
                Warn.warn_always
                  "%s: scanning affinity degraded to random affinity"
                  doc.Name.name ;
                "aff_random"
              end
          | Affinity.Custom ->
              if dca then "aff_custom"
              else begin
                Warn.warn_always
                  "%s: custom affinity degraded to random affinity"
                  doc.Name.name ;
                "aff_random"
              end)
          (if dca then 1 else 0)
          (if mk_dsa test then 1 else 0)
          (match memory with | Direct -> -1 | Indirect -> 1)
          "MAX_LOOP"
          (if do_timebase && have_timebase then "&delta_tb" else "NULL")
          (if do_custom then "&_prefetch" else "NULL")
          (if do_staticpl then "1" else "-1")
          (if do_verbose_barrier then "1" else "-1")
          (if do_speedcheck then "1" else "-1")
          (match launch with | Fixed -> "1" | Changing -> "0")
          (if do_vp then "1" else "0") ;
        O.oi "cmd_t cmd = def;" ;
(* Parse command line *)
        O.oi "parse_cmd(argc,argv,&def,&cmd);" ;
        begin match driver,Cfg.threadstyle with
        | Driver.Shell,ThreadStyle.Cached -> O.oi "set_pool();"
        | _,_ -> ()
        end ;
        begin if Cfg.exit_cond then
          O.fi "int cond = run(&cmd,def_all_cpus,%s);" outchan
        else
          O.fi "run(&cmd,def_all_cpus,%s);" outchan
        end ;
        if alloc_def_all_cpus then begin
          O.oi "if (def_all_cpus != cmd.aff_cpus) cpus_free(def_all_cpus);"
        end ;
        begin if Cfg.exit_cond then
          O.oi "return cond ? EXIT_SUCCESS : EXIT_FAILURE;"
        else
          O.oi "return EXIT_SUCCESS;"
        end ;
        O.o "}" ;
        ()

      let dump doc test =
        ObjUtil.insert_lib_file O.o "header.txt" ;
(* Minimal type environemnt *)
        let env = U.build_env test in
        dump_header test ;
        dump_read_timebase () ;
        dump_threads test ;
        if mk_dsa test then dump_topology test ;
        let cpys = dump_def_ctx env test in
        dump_filter env test ;
        dump_cond_fun env test ;
        dump_defs_outs doc env test ;
        dump_check_globals env doc test ;
        dump_templates env doc.Name.name test ;
        dump_reinit env test cpys ;
        dump_zyva doc cpys env test ;
        if do_vp then UD.prelude doc test ;
        dump_run doc env test ;
        dump_main doc env test
    end
