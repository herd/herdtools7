(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
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
  val hexa : bool
  val litmus : string
  val size : int
  val runs : int
  val avail : int option
  val stride : KStride.t
  val barrier : KBarrier.t
  val affinity : KAffinity.t
  val sharelocks : int option
  val delay : int
  val sysarch : Archs.System.t
end

module Make
    (Cfg:Config)
    (P:sig type code end)
    (A:Arch_litmus.Base)
    (MemType:MemoryType.S)
    (T:Test_litmus.S with
     type instruction = A.instruction and
     type P.code = P.code and module A = A and module FaultType = A.FaultType)
    (O:Indent.S)
    (Lang:Language.S with type t = A.Out.t)
    : sig
      val dump : Name.t -> T.t -> unit
    end =
  struct

(******************)
(* Arch dependant *)
(******************)

    module Insert = ObjUtil.Insert(Cfg)

    let memtype_possible = Insert.exists "klitmus_memory_type.c"
    let timebase_possible = Cfg.delay > 0 && Insert.exists "timebase.c"

(*************)
(* Utilities *)
(*************)

    let spinsize = "spinsize"

    let do_spinsize sz = match Cfg.sharelocks with
    | None -> sz
    | Some _ -> spinsize

    module UCfg = struct
      let memory = Memory.Direct
      let preload = Preload.NoPL
      let mode = Mode.Std
      let kind = true
      let hexa = Cfg.hexa
      let exit_cond = false
      let have_fault_handler = false
      let do_stats = false
      let sysarch = Cfg.sysarch
      let c11 = false
      let variant _ = false (* No variant (yet ?) *)
      let ascall = true
    end

    module U = SkelUtil.Make(UCfg)(P)(A)(T)
    module EPF =
      DoEmitPrintf.Make
        (struct
          let emitprintf = true
          let ctr = Fmt.I64
          let no_file = false
          let brittle = false
        end)(O)
    module UD = U.Dump(O)(EPF)

    let tag_malloc s = sprintf "malloc_%s" s

    let iter_outs f proc = List.iter (f proc)

    let iter_all_outs f test =
      List.iter
        (fun (proc,(_,(outs,_))) -> iter_outs f proc outs)
        test.T.code

    module DC =
      CompCond.Make(O)
        (struct
          let with_ok = false
          module C = T.C
          let dump_value _loc (* ignored, see preSi.ml *) = C.V.pp O.hexa
          module Loc = struct
            type location = A.location
            type t = A.rlocation
            let compare = A.rlocation_compare
            let dump loc = "_" ^ A.dump_rloc_tag loc
            let dump_fatom d a = "_" ^ SkelUtil.dump_fatom_tag d a
          end
        end)

    let do_dump_cond_fun env cond =
      let find_type loc =
        let t = match CType.strip_atomic (U.find_rloc_type loc env) with
        | CType.Base "atomic_t" ->  CType.Base "int"
        | t -> t in
        CType.dump (CType.strip_atomic t),CType.is_ptr t in
      DC.fundef (U.cast_constant env) find_type cond

    let dump_cond_fun env test = do_dump_cond_fun env test.T.condition

    let dump_cond_fun_call test dump_loc dump_val =
      DC.funcall test.T.condition dump_loc dump_val

    let dump_filter env test = match test.T.filter with
    | None -> ()
    | Some f ->
        let find_type loc =
          let t = U.find_rloc_type loc env in
          CType.dump (CType.strip_atomic t),CType.is_ptr t in
        DC.fundef_prop "filter_cond" (U.cast_constant env) find_type f


    let is_srcu_struct t = match t with
    | CType.Base "struct srcu_struct" -> true
    | _ -> false

    and is_spinlock_t t = match t with
    | CType.Base "spinlock_t" -> true
    | _ -> false

(***********)
(* Headers *)
(***********)

    let dump_header () =
      ObjUtil.insert_lib_file O.o "kincludes.txt" ;
      ()

(********)
(* Outs *)
(********)

    let dump_rloc_idx loc = sprintf "%s_f" (A.dump_rloc_tag loc)

    let fmt_outcome test locs env =
      U.fmt_outcome test
        (fun t -> match Compile.get_fmt Cfg.hexa t with
        | CType.Direct fmt ->
            if Cfg.hexa then "0x%" ^ fmt else "%" ^ fmt
        | CType.Macro fmt ->
            (if Cfg.hexa then "0x%\"" else "%\"") ^ fmt ^ "\"")
        locs env

    let dump_outs env test =
      O.o "/************/" ;
      O.o "/* Outcomes */" ;
      O.o "/************/" ;
      O.o "" ;
      if U.ptr_in_outs env test then begin
(* Pretty-print indices *)
        let naddrs = List.length test.T.globals in
        O.f "static char *pretty_addr[%i] = {\"NULL\",%s,\"???\",};"
          (naddrs+2)
          (String.concat ","
             (List.map (fun (s,_) -> sprintf "\"%s\"" s) test.T.globals)) ;
        O.o "" ;
      end ;
      let outs = U.get_displayed_locs test in
      let nitems =
        let map =
          A.RLocSet.fold
            (fun loc ->
              A.RLocMap.add loc (SkelUtil.nitems (U.find_rloc_type loc env)))
            outs A.RLocMap.empty in
        fun loc ->
          try A.RLocMap.find loc map
          with Not_found -> assert false in
      let nouts =
        A.RLocSet.fold
          (fun loc k -> nitems loc + k)
          outs 0 in

      O.f "#define NOUTS %i" nouts ;
      O.o "typedef u64 outcome_t[NOUTS];" ;
      O.o "" ;
      let _ =
        A.RLocSet.fold
          (fun loc pos ->
            O.f "static const int %s = %i ;" (dump_rloc_idx loc) pos ;
            pos+nitems loc)
          outs 0 in
      O.o "" ;
      ObjUtil.insert_lib_file O.o "outs.txt" ;
      O.o "" ;
      O.o "static outs_t *add_outcome_outs(outs_t *p,u64 *k,int show) {" ;
      O.oi "return loop_add_outcome_outs(p,k,NOUTS-1,1,show);" ;
      O.o "}" ;
      O.o "" ;
(* Only dump depends on test... *)
      O.o "static void do_dump_outs (struct seq_file *m,outs_t *p,u64 *o,int sz) {" ;
      O.oi "for ( ; p ; p = p->next) {" ;
      O.oii "o[sz-1] = p->k;" ;
      O.oii "if (p->c > 0) {" ;
      let fmt_var = fmt_outcome test outs env in
      let fmt ="\"%-8\"PCTR\"%c>" ^ fmt_var ^ "\\n\"" in
      let args =
        String.concat ","
          ("p->c"::
           (let tst =
             let open ConstrGen in
             match test.T.condition with
             | ForallStates _ -> "!p->show"
             | ExistsState _|NotExistsState _ -> "p->show" in
            tst ^ " ? '*' : ':'")::
           (List.map
              (fun loc ->
                let sloc = A.dump_rloc_tag loc in
                match U.find_rloc_type loc env with
                | CType.Pointer _ ->
                    sprintf "pretty_addr[(int)o[%s_f]]" sloc
                | CType.Base "atomic_t" ->
                     sprintf "(int)o[%s_f]" sloc
                | CType.Array _ -> assert false
                | t -> sprintf "(%s)o[%s_f]" (CType.dump t) sloc)
              (A.RLocSet.elements outs))) in
      O.fiii "seq_printf(m,%s,%s);" fmt args ;
      O.oii "} else {" ;
      O.oiii "do_dump_outs(m,p->down,o,sz-1);" ;
      O.oii "}" ;
      O.oi "}" ;
      O.o "}" ;
      O.o "" ;
      O.o "static void dump_outs(struct seq_file *m,outs_t *p) {" ;
      O.oi "outcome_t buff;" ;
      O.oi "do_dump_outs(m,p,buff,NOUTS);" ;
      O.o "}" ;
      O.o "" ;
      ()

(**************)
(* Parameters *)
(**************)

    let dump_params tname test =
      O.o "/**************/" ;
      O.o "/* Parameters */" ;
      O.o "/**************/" ;
      O.o "" ;
      O.f "static const int nthreads = %i;" (T.get_nprocs test) ;
      O.f "static unsigned int nruns = %i;" Cfg.runs ;
      O.f "static unsigned int size = %i;" Cfg.size ;
      begin match Cfg.sharelocks with
      | None -> ()
      | Some sz -> O.f "const static unsigned int %s = %i;" spinsize sz;
      end ;
      O.f "static unsigned int stride = %i;"
        (let open KStride in
        match Cfg.stride with
        | St i -> i
        | Adapt -> List.length test.T.code) ;
      O.f "static unsigned int avail = %i;"
        (match Cfg.avail with None -> 0 | Some a -> a) ;
      O.o "static unsigned int ninst = 0;" ;
      O.f "static int affincr = %i;"
        (let open KAffinity in
        match Cfg.affinity with
        | No -> 0
        | Incr i -> i
        | Random -> -1) ;
      if timebase_possible then
        O.f "static unsigned int delay_tb = %i;" Cfg.delay ;
      O.o "" ;
      let do_module_param t v = O.f "module_param(%s,%s,0644);" v t in
      let module_param = do_module_param "uint" in
      module_param "nruns" ;
      module_param "size" ;
      module_param "stride" ;
      module_param "avail" ;
      module_param "ninst" ;
      do_module_param "int" "affincr" ;
      if timebase_possible then
        module_param "delay_tb" ;
      O.o "" ;
      O.f "static char *name = %S;" tname ;
      O.o "module_param(name,charp,0444);" ;
      O.o "" ;
      O.o "static wait_queue_head_t *wq;" ;
      O.o "static atomic_t done = ATOMIC_INIT(0);" ;
      O.o "" ;
      ()


(***********)
(* Context *)
(***********)
(* Dump left & right values when pointer to context is available *)

(* Left value *)
    let dump_a_leftval a = sprintf "_a->%s[_i]" a

(* Right value *)
    let dump_a_addr s = sprintf "&(_a->%s[_i])" s

    let rec dump_a_v v =
      let open Constant in
      match v with
      | Concrete i -> A.V.Scalar.pp Cfg.hexa i
      | ConcreteVector vs->
          let pp_vs = List.map dump_a_v vs in
          sprintf "{%s}" (String.concat "," pp_vs) (* list initializer syntax *)
      | ConcreteRecord _ ->
          Warn.user_error "No record value for klitmus"
      | Symbolic _ as v when is_label v ->
          Warn.user_error "No label value for klitmus"
      | Symbolic (Virtual {name=s;tag=None;cap=0L;offset=0;_}) -> dump_a_addr (Constant.Symbol.pp s)
      | Symbolic _|Tag _| PteVal _| AddrReg _ ->
          Warn.user_error "No tag, indexed access, pteval nor addrreg for klitmus"
      | Instruction _ ->
          Warn.fatal "FIXME: dump_a_v functionality for -variant self"
      | Frozen _ -> assert false

    let is_align_effective mts env s =
      U.is_aligned s env && Misc.is_none (Misc.Simple.assoc_opt s mts)

    let is_rloc_align env rloc =
      let open ConstrGen in
      match rloc with
      | Loc (A.Location_global s) ->
          U.is_aligned (Global_litmus.as_addr s) env
      | Loc (A.Location_reg _)|Deref _ -> false

let dump_ctx mts env test =
      O.o "/****************/" ;
      O.o "/* Affinity     */" ;
      O.o "/****************/" ;
      O.o "" ;
      O.o "static int *online;" ;
      O.o "static int nonline;" ;
      O.o "" ;
      O.o "/****************/" ;
      O.o "/* Test Context */" ;
      O.o "/****************/" ;
      O.o "" ;
      let need_align =
        List.exists
          (fun (s,_) -> is_align_effective mts env s)
          test.T.globals in
      if need_align then
        begin
          O.o "static void *do_align(void *p,size_t sz) {" ;
          O.oi "uintptr_t x = (uintptr_t)p;" ;
          O.oi "x += sz-1;" ;
          O.oi  "x /= sz;" ;
          O.oi  "x *= sz;" ;
          O.oi "return (void *)x;" ;
          O.o "}" ;
          O.o ""
        end ;
      UD.dump_vars_types true test ;
      O.o "typedef struct {" ;
      O.o "/* Shared locations */" ;
      List.iter
        (fun (s,t) ->
          let t = CType.strip_const t in
          let t = CType.strip_volatile t in
          if U.is_aligned s env then
            let pp_t = SkelUtil.type_name s in
            match Misc.Simple.assoc_opt s mts with
            | None ->
                O.fi "%s *%s,*%s;" pp_t (tag_malloc s) s
            | Some _ ->
                O.fi "%s *%s;" pp_t s
          else
            let pp_t = SkelUtil.dump_global_type s t in
            O.fi "%s *%s;" pp_t s)
        test.T.globals ;
      O.o "/* Final contents of observed registers */" ;
      iter_all_outs
        (fun proc (reg,t) ->
          O.fi "%s *%s;"
            (CType.dump t)
            (A.Out.dump_out_reg proc reg))
        test ;
      O.o "/* For synchronisation */" ;
      O.oi "sense_t *sync_barrier;" ;
      begin let open KBarrier in
      match Cfg.barrier with
      | User ->
          O.oi "int *barrier;"
      | TimeBase ->
          O.oi "sense_t *barrier;"
      | No -> ()
      end ;
      O.o "} ctx_t ;" ;
      O.o "" ;
      if U.ptr_in_outs env test then begin
        (*  Translation to indices *)
        let dump_test k s =
          O.fi "else if (v_addr == (void *)&(_a->%s[_i])) return %i;"
            s k in
        O.o "static int idx_addr(ctx_t *_a,int _i,void *v_addr) {" ;
        O.oi "if (v_addr == NULL) { return 0;}" ;
        Misc.iteri (fun k (s,_) -> dump_test (k+1) s) test.T.globals ;
        O.fi "else { return %i;}" (List.length test.T.globals+1) ;
        O.o "}" ;
        O.o "" ;
      end ;
      O.o "static ctx_t **ctx;" ;
      O.o "" ;
      begin match mts with
      | _::_ ->
        Insert.insert O.o "klitmus_memory_type.c" ;
        O.o ""
      | _ -> ()
      end ;
      if List.exists (fun (_,t) -> is_srcu_struct(t)) test.T.globals
      then begin
        O.o "static void cleanup_srcu_structs(struct srcu_struct *p,int sz) {" ;
        O.oi "for (int _i = 0 ; _i < sz ; _i++) cleanup_srcu_struct(&p[_i]);" ;
        O.o "}" ;
        O.o ""
      end ;
      let sz = if Misc.consp mts then ",size_t sz" else "" in
      O.f "static void free_ctx(ctx_t *p%s) {" sz ;
      O.oi "if (p == NULL) return;" ;
      let free tag = O.fi "if (p->%s) kfree(p->%s);" tag tag
      and free_mt tag =
        O.fi "if (p->%s) klitmus_free_pat(p->%s,sizeof(p->%s[0])*sz);"
          tag tag tag in
      List.iter
        (fun (s,t) ->
          if is_srcu_struct t then begin
            O.fi "if (p->%s) {" s ;
            let sz = do_spinsize "size" in
            O.fii "cleanup_srcu_structs(p->%s,%s);" s sz ;
            O.fii "kfree(p->%s);" s ;
            O.oi "}"
          end else begin
            match Misc.Simple.assoc_opt s mts with
            | None ->
                free (if U.is_aligned s env then tag_malloc s else s)
            | Some _ -> free_mt s
          end)
        test.T.globals ;
      iter_all_outs
        (fun proc (reg,_) ->
          let tag = A.Out.dump_out_reg proc reg in
          free tag) test ;
      begin
        O.oi "free_sense(p->sync_barrier);" ;
        let open KBarrier in
        match Cfg.barrier with
        | User -> free "barrier"
        | TimeBase -> O.oi "free_sense(p->barrier);"
        | No -> ()
      end ;
      O.oi "kfree(p);" ;
      O.o "}" ;
      O.o "" ;
      O.o "static ctx_t *alloc_ctx(size_t sz) {" ;
      O.oi "ctx_t *r = kzalloc(sizeof(*r),GFP_KERNEL);" ;
      O.oi "if (!r) { return NULL; }" ;
      let do_alloc sz tag =
        O.fi "r->%s = kmalloc(sizeof(r->%s[0])*%s,GFP_KERNEL);" tag tag sz;
        O.fi "if (!r->%s) { return NULL; }" tag in
      let alloc sz s =
        if U.is_aligned s env then
          begin
            do_alloc (sprintf "(%s+1)" sz) (tag_malloc s) ;
            O.fi
              "r->%s = do_align(r->%s,sizeof(r->%s[0]));"
              s (tag_malloc s) s
          end
        else
          do_alloc sz s
      and alloc_mt mt sz tag =
        let mt = MemType.emit mt in
        O.fi "r->%s = klitmus_alloc_pat(%s,sizeof(r->%s[0])*%s);"
          tag mt tag sz;
        O.fi "if (!r->%s) { return NULL; }" tag in
      List.iter
        (fun (s,t) ->
          match t with
          | CType.Base "spinlock_t" ->
              let sz = do_spinsize "sz" in
              alloc sz s ;
              O.fi
                "for (int _i=0 ; _i < %s ; _i++) spin_lock_init(&r->%s[_i]);"
                sz s
          | _ ->
              if is_srcu_struct t then begin
                let sz = do_spinsize "sz" in
                alloc sz s ;
                O.fi "for (int _i=0 ; _i < %s ; _i++) {" sz ;
                O.fii "if (init_srcu_struct(&r->%s[_i])) {" s ;
                O.fiii "cleanup_srcu_structs(r->%s,_i);" s ;
                O.fiii "kfree(r->%s);" s ;
                O.fiii "r->%s = NULL;" s ;
                O.oiii "return NULL;" ;
                O.oii "}" ;
                O.oi "}"
              end else if is_spinlock_t t then begin
                let sz = do_spinsize "sz" in
                alloc sz s
              end else
                match Misc.Simple.assoc_opt s mts with
                | None -> alloc "sz" s
                | Some mt -> alloc_mt mt "sz" s)
        test.T.globals ;
      iter_all_outs
        (fun proc (reg,_) ->
          let tag = A.Out.dump_out_reg proc reg in
          alloc "sz" tag) test ;
      begin let open KBarrier in
      O.oi "r->sync_barrier = alloc_sense();";
      O.oi "if (!r->sync_barrier) { return NULL; }" ;
      match Cfg.barrier with
      | User -> alloc "sz" "barrier"
      | TimeBase ->
          O.oi "r->barrier = alloc_sense();";
          O.oi "if (!r->barrier) { return NULL; }"
      | No -> ()
      end ;
      O.oi "return r;" ;
      O.o "}" ;
      O.o "" ;
      O.o "static void init_ctx(ctx_t *_a,size_t sz) {" ;
      O.oi "for (int _i = 0 ; _i < sz ; _i++) {" ;
      List.iter
        (fun (s,_) ->
          let loc = A.location_of_addr s in
          let v = A.find_in_state loc test.T.init in
          let ty = U.find_type loc env in
          match ty with
          | CType.Base "spinlock_t"
              -> ()
          | CType.Base "atomic_t"
            ->
              O.fii "atomic_set(&%s,%s);"
                (dump_a_leftval s)
                (dump_a_v v)
          | _ ->
              if U.is_aligned s env then
                begin
                  let pp_t = CType.dump ty in
                  O.fii "%s *_%s_ptr = (%s *)&(%s);"
                    pp_t s pp_t  (dump_a_leftval s) ;
                  O.fii "*_%s_ptr = (%s)%s;" s pp_t (dump_a_v v)
                end
              else if not (is_srcu_struct ty) then
                O.fii "%s = (%s)%s;"
                  (dump_a_leftval s)
                  (CType.dump ty)
                  (dump_a_v v))
        test.T.globals ;
      List.iter
        (fun (proc,(_,(outs,_))) ->
          List.iter
            (fun (reg,t) ->
              O.fii "_a->%s[_i] = %s;"
                (A.Out.dump_out_reg proc reg)
                (match CType.is_ptr t with
                | false -> Skel.sentinel
                | true -> "NULL"))
            outs)
        test.T.code ;
      O.oii "sense_init(_a->sync_barrier);" ;
      begin let open KBarrier in
      match Cfg.barrier with
      | User ->
          O.oii "_a->barrier[_i] = 0;"
      | No|TimeBase ->
          ()
      end ;
      O.oi "}" ;
      begin let open KBarrier in
      match Cfg.barrier with
      | User|No -> ()
      | TimeBase ->
          O.oi "sense_init(_a->barrier);"
      end ;
      if MemType.need_flush mts then O.oi "klitmus_flush_caches();" ;
      O.o "}" ;
      O.o "" ;
      ()

(***************)
(* Test proper *)
(***************)

let dump_barrier_def () =
  if timebase_possible then begin
     O.o "/* Read timebase */" ;
     O.o "#define TIMEBASE" ;
     O.o "typedef uint64_t tb_t ;" ;
     O.o "" ;
     Insert.insert O.o "timebase.c";
     O.o ""
  end ;
  ObjUtil.insert_lib_file O.o "kbarrier-tb.txt" ;
  O.o "" ;
  begin let open KBarrier in
  match Cfg.barrier with
  | User ->
      ObjUtil.insert_lib_file O.o "kbarrier-user.txt" ;
      O.o ""
  | TimeBase|No -> ()
  end


let dump_threads _tname env test =
  O.o "/***************/" ;
  O.o "/* Litmus code */" ;
  O.o "/***************/" ;
  O.o "" ;
  let global_env = U.select_global env in
  let global_env =
    List.map
      (fun (x,ty) -> x,CType.strip_const (CType.strip_volatile ty))
      global_env in
  let aligned_env =
    List.filter
      (fun (a,_) -> U.is_aligned a env)
      test.T.globals in
  List.iter
    (fun (proc,(out,(_outregs,envVolatile))) ->
      Lang.dump_fun O.out Template.no_extra_args
        global_env envVolatile proc out ;
      O.f "static int thread%i(void *_p) {" proc ;
      O.oi "ctx_t *_a = (ctx_t *)_p;" ;
      O.o "" ;
      O.oi "smp_mb();" ;
      O.oi "sense_wait(_a->sync_barrier);";
      O.oi "smp_mb();" ;
      O.oi "for (int _j = 0 ; _j < stride ; _j++) {" ;
      O.oii "for (int _i = _j ; _i < size ; _i += stride) {" ;
      begin let open KBarrier in
      match Cfg.barrier with
      | User ->
          O.fiii "barrier_wait(%i,_i,&_a->barrier[_i]);" proc
      | TimeBase ->
          O.oiii "sense_wait(_a->barrier);"
      | No -> ()
      end ;
      let tr_idx t idx = match Cfg.sharelocks with
      | Some _ when is_srcu_struct t || is_spinlock_t t ->
          sprintf "%s %% %s" idx spinsize
      | Some _|None -> idx in
      Lang.dump_call (LangUtils.code_fun proc)
        [] tr_idx O.out (Indent.as_string indent3)
        (global_env,aligned_env) envVolatile proc out ;
      O.oii "}" ;
      O.oi "}" ;
      O.oi "smp_mb();" ;
      O.oi "atomic_inc(&done);" ;
      O.oi "smp_mb();" ;
      O.oi "wake_up(wq);" ;
      O.oi "smp_mb();" ;
      O.oi "return 0;" ;
      O.o "}" ;
      O.o "" ;
      ())
    test.T.code ;
  ()

let dump_zyva tname env test =
  O.o "/********/" ;
  O.o "/* Zyva */" ;
  O.o "/********/" ;
  O.o "" ;
  O.o "static outs_t *zyva(void) {" ;
  O.oi "ctx_t **c = ctx;" ;
  O.oi "outs_t *outs = NULL;" ;
  O.oi "const int nth = ninst * nthreads;" ;
  O.oi "struct task_struct **th;" ;
  O.o "" ;
  O.oi "th = kzalloc(sizeof(struct task_struct *) * nth, GFP_KERNEL);" ;
  O.oi "if (!th) return NULL;" ;
  O.oi "for (int _k = 0 ; _k < nruns ; _k++) {" ;
  O.oii "int _nth = 0;" ;
  O.o "" ;
  O.oii "for (int _ni = 0 ; _ni < ninst ; _ni++) init_ctx(c[_ni],size);" ;
  O.oii "atomic_set(&done,0);" ;
  O.oii "smp_mb();" ;
  O.oii "for (int _ni = 0 ; _ni < ninst ; _ni++) {" ;
  for i = 0 to T.get_nprocs test-1 do
    O.fiii "th[_nth] = kthread_create(thread%i,c[_ni],\"thread%i\");"
      i i ;
    O.oiii "if (IS_ERR(th[_nth])) {kfree(th); return outs;}" ;
    O.oiii "_nth++;"
  done ;
  O.oii "}" ;
  O.oii "if (affincr != 0) {" ;
  O.oiii "int _idx=0, _idx0=0, _incr=affincr > 0 ? affincr : 1;" ;
  O.oiii "if (affincr < 0) shuffle_array(online,nonline);" ;
  O.oiii "for (int _t = 0 ; _t < nth ; _t++) {" ;
  O.oiv "kthread_bind(th[_t],online[_idx]);" ;
  O.oiv "_idx += _incr;";
  O.oiv "if (_idx >= nonline) _idx = ++_idx0;" ;
  O.oiv "if (_idx >= nonline) _idx = _idx0 = 0;" ;
  O.oiii "}" ;
  O.oii "}" ;
  O.oii "for (int _t = 0 ; _t < nth ; _t++) wake_up_process(th[_t]);" ;
  O.oii "wait_event_interruptible(*wq, atomic_read(&done) == nth);" ;
  O.oii "smp_mb();" ;
  O.oii "for (int _ni = 0 ; _ni < ninst ; _ni++) {" ;
  O.oiii "ctx_t *_a = c[_ni];" ;
  O.oiii "for (int _i = 0 ; _i < size ; _i++) {" ;
  let tag_copy s = sprintf "_%s_i" s in
  let dump_a_rloc loc =
    match U.find_rloc_type loc env with
    | CType.Base "atomic_t" ->
        sprintf "atomic_read(&_a->%s[_i])"  (A.dump_rloc_tag loc)
    | _ ->
        if is_rloc_align env loc then
          tag_copy (A.dump_rloc_tag loc)
        else
          sprintf "_a->%s[_i]" (A.dump_rloc_tag loc) in
  O.oiv "outcome_t _o;" ;
  O.oiv "int _cond;" ;
  A.RLocSet.iter
    (fun rloc ->
      if is_rloc_align env rloc then
        let t = U.find_rloc_type rloc env in
        let pp_t = CType.dump t in
        let tag = A.dump_rloc_tag rloc in
        let ptr = sprintf "_ptr_%s_i" tag in
        begin
          O.fiv "%s *%s = (%s *)&(%s);"
            pp_t ptr pp_t (sprintf "_a->%s[_i]" tag) ;
          O.fiv "%s %s = *%s;" pp_t (tag_copy tag) ptr
        end)
    (U.get_observed_locs test) ;
  begin match test.T.filter with
  | None -> ()
  | Some f ->
      O.fiv "if (!%s) continue;"
        (DC.funcall_prop "filter_cond" f dump_a_rloc dump_a_addr)
  end ;
  O.fiv "_cond = %s;"
    (dump_cond_fun_call test dump_a_rloc dump_a_addr)  ;
  A.RLocSet.iter
    (fun loc ->
      O.fiv "_o[%s] = %s;"
        (dump_rloc_idx loc)
        (let sloc = dump_a_rloc loc in
        if U.is_rloc_ptr loc env then sprintf "idx_addr(_a,_i,%s)" sloc
        else sloc))
    (U.get_displayed_locs test) ;
  O.oiv "outs = add_outcome_outs(outs,_o,_cond);" ;
  O.oiii "}" ;
  O.oii "}" ;
  O.oii "cond_resched();" ;
  O.oi "}" ;
  O.oi "kfree(th);" ;
  O.oi "return outs;" ;
  O.o "}" ;
  O.o "" ;
  O.o "static int do_it(struct seq_file *m) {" ;
  O.oi "ktime_t time_start = ktime_get();" ;
  O.oi "outs_t *outs = zyva();" ;
  O.oi "ktime_t time_end = ktime_get();" ;
  let fmt = sprintf "Test %s %s\\n" tname
      (ConstrGen.pp_kind (ConstrGen.kind_of test.T.condition)) in
  O.fi "seq_printf(m,\"%s\");" fmt ;
  let fmt = "Histogram (%\"PCTR\" states)\\n" in
  O.fi "seq_printf(m,\"%s\",count_nstates(outs));" fmt ;
  O.oi "dump_outs(m,outs);" ;
  O.oi "{" ;
  O.oii "count_t pos=count_show(outs),neg=count_noshow(outs);" ;
  O.oii "char *msg = \"Sometimes\";" ;
  O.oii "u64 delta =  ktime_to_ms(ktime_sub(time_end, time_start));";
  O.oii "u64 sec = divBy1000(delta);" ;
  O.oii "u64 cent = divBy10(delta-1000*sec + 5);" ;
  let ok_expr =
    let open ConstrGen in
    match test.T.condition with
    | ExistsState _ -> "pos > 0"
    | NotExistsState _ |ForallStates _-> "neg == 0" in
  O.fii "seq_printf(m,\"%%s\\n\\n\",%s ? \"Ok\" : \"No\");" ok_expr ;
  let pos,neg  = let open ConstrGen in match test.T.condition with
  | ExistsState _|ForallStates _ -> "pos","neg"
  | NotExistsState _ -> "neg","pos" in
  let fmt = "Witnesses\\nPositive: %\"PCTR\", Negative: %\"PCTR\"\\n" in
  O.fii "seq_printf(m,\"%s\",%s,%s);" fmt pos neg ;
  let fmt =
    "Condition " ^ U.pp_cond test ^ " is %svalidated\n" in
  O.fii "seq_printf(m,%S,%s?\"\":\"NOT \");" fmt ok_expr ;
  List.iter
    (fun (k,i) ->
      if
        MiscParser.key_match MiscParser.mt_key k
        ||         MiscParser.key_match MiscParser.memory_type_key k
      then
        O.fii "seq_printf(m,\"%%s=%%s\\n\",%S,%S);" k i)
    test.T.info ;
  begin match U.get_info MiscParser.hash_key test with
  | None -> ()
  | Some h ->
      let fmt = MiscParser.hash_key ^ "=" ^ h in
      O.fii "seq_printf(m,\"%%s\\n\",%S);" fmt ;
      ()
  end ;
  O.oii "if (pos == 0) msg = \"Never\";" ;
  O.oii "else if (neg == 0) msg = \"Always\";" ;
  let fmt = sprintf "Observation %s %%s %%\"PCTR\" %%\"PCTR\"\\n" tname in
  O.fii "seq_printf(m,\"%s\",msg,pos,neg);" fmt ;
  let fmt = sprintf "Time %s %%llu.%%02llu\\n\\n" tname in
  O.fii "seq_printf(m,\"%s\",sec,cent);" fmt ;
  O.oi "}" ;
  O.oi "free_outs(outs);" ;
  O.oi "return 0;" ;
  O.o "}" ;
  O.o "" ;
  ()

(**********)
(* ProcFs *)
(**********)

let dump_proc tname _test =
  let tname = String.escaped tname in
  O.o "static atomic_t running=ATOMIC_INIT(0);" ;
  O.o "" ;
  O.o "static int litmus_proc_show(struct seq_file *m,void *v) {" ;
  O.oi "if (atomic_add_return(1,&running) != 1) {";
  let fmt = "%s: already running, good bye!\\n" in
  O.fii "seq_printf(m,\"%s\",\"%s\");" fmt tname ;
  O.oii "atomic_dec(&running);" ;
  O.oii "return 0;" ;
  O.oi "}" ;
  O.oi "if (ninst == 0 || ninst * nthreads > nonline) {" ;
  let fmt = "%s: skipped\\n" in
  O.fii "seq_printf(m,\"%s\",\"%s\");" fmt tname ;
  O.oii "atomic_dec(&running);" ;
  O.oii "return 0;" ;
  O.oi "} else {" ;
  O.oii "int ret = do_it(m);" ;
  O.oii "atomic_dec(&running);" ;
  O.oii "return ret;" ;
  O.oi "}" ;
  O.o "}" ;
  O.o "" ;
  O.o "#ifndef HAVE_PROC_CREATE_SINGLE" ;
  O.o "static int\nlitmus_proc_open(struct inode *inode,struct file *fp) {" ;
  O.oi "return single_open(fp,litmus_proc_show,NULL);" ;
  O.o "}" ;
  O.o "" ;
  O.o "static const struct file_operations litmus_proc_fops = {" ;
  O.oi ".owner   = THIS_MODULE," ;
  O.oi ".open    = litmus_proc_open," ;
  O.oi ".read    = seq_read," ;
  O.oi ".llseek   = seq_lseek," ;
  O.oi ".release = single_release," ;
  O.o "};" ;
  O.o "#endif" ;
  O.o "" ;
  ()

(**************************)
(* Init, Exit and friends *)
(**************************)

let dump_init_exit has_memtype _test =
  O.o "static int __init" ;
  O.o "litmus_init(void) {" ;
  O.oi "int err=0;" ;
  O.o "#ifdef HAVE_PROC_CREATE_SINGLE" ;
  O.fi "struct proc_dir_entry *litmus_pde = proc_create_single(\"%s\",0,NULL,litmus_proc_show);" Cfg.litmus ;
  O.o "#else" ;
  O.fi "struct proc_dir_entry *litmus_pde = proc_create(\"%s\",0,NULL,&litmus_proc_fops);" Cfg.litmus ;
  O.o "#endif" ;
  O.oi "if (litmus_pde == NULL) { return -ENOMEM; }" ;
  O.oi "stride = stride == 0 ? 1 : stride;" ;
  O.oi "nonline = num_online_cpus ();" ;
  O.oi "online = kzalloc(sizeof(*online)*nonline,GFP_KERNEL);" ;
  O.oi "if (online == NULL) goto clean_pde;" ;
  O.oi "{" ;
  O.oii "int cpu,_k;" ;
  O.oii "_k=0; for_each_cpu(cpu,cpu_online_mask) online[_k++] = cpu;";
  O.oi "}" ;
  O.oi "if (avail == 0 || avail > nonline) avail = nonline;" ;
  O.oi "if (ninst == 0) ninst = avail / nthreads ;" ;
  O.o "" ;
  O.oi "ctx = kzalloc(sizeof(ctx[0])*ninst,GFP_KERNEL);" ;
  O.oi "if (ctx == NULL) { err = -ENOMEM ; goto clean_online; }" ;
  O.oi "for (int _k=0 ; _k < ninst ; _k++) {" ;
  O.oii "ctx[_k] = alloc_ctx(size);" ;
  O.oii "if (ctx[_k] == NULL) { err = -ENOMEM; goto clean_ctx; }" ;
  O.oi "}" ;
  O.o "" ;
  O.oi "wq =  kzalloc(sizeof(*wq), GFP_KERNEL);" ;
  O.oi "if (wq == NULL) { err = -ENOMEM; goto clean_ctx; }" ;
  O.oi "init_waitqueue_head(wq);" ;
  O.oi "return 0;" ;
  O.o "clean_ctx:" ;
  let free_ctx_sz = if has_memtype then ",size" else "" in
  O.fi "for (int k=0 ; k < ninst ; k++) free_ctx(ctx[k]%s);" free_ctx_sz;
  O.oi "kfree(ctx);" ;
  O.o  "clean_online:" ;
  O.oi "kfree(online);" ;
  O.o  "clean_pde:" ;
  O.fi "remove_proc_entry(\"%s\",NULL);" Cfg.litmus ;
  O.oi "return err;" ;
  O.o "}" ;
  O.o "" ;

  O.o "static void __exit" ;
  O.o "litmus_exit(void) {" ;
  O.fi "for (int k=0 ; k < ninst ; k++) free_ctx(ctx[k]%s);" free_ctx_sz;
  O.oi "kfree(ctx);" ;
  O.oi "kfree(online);" ;
  O.fi "remove_proc_entry(\"%s\",NULL);" Cfg.litmus ;
  O.o "}" ;
  O.o "" ;
  O.o "module_init(litmus_init);" ;
  O.o "module_exit(litmus_exit);" ;
  O.o "" ;
  O.o "MODULE_LICENSE(\"GPL\");" ;
  O.o "MODULE_AUTHOR(\"Luc\");" ;
  O.o "MODULE_DESCRIPTION(\"Litmus module\");" ;
  O.o "" ;
  ()


let dump name test =
  ObjUtil.insert_lib_file O.o "header.txt" ;
  let tname = name.Name.name in
  let env = U.build_env test in
  let mts = if memtype_possible then MemType.parse test.T.info else [] in
  let has_memtype = Misc.consp mts in
  dump_header () ;
  dump_params tname test ;
  dump_outs env test ;
  dump_barrier_def () ;
  dump_ctx mts env test ;
  dump_threads tname env test ;
  dump_filter env test ;
  dump_cond_fun env test ;
  dump_zyva tname env test ;
  dump_proc tname test ;
  dump_init_exit has_memtype test ;
  ()


end
