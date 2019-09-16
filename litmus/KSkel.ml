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
  val size : int
  val runs : int
  val avail : int option
  val stride : KStride.t
  val barrier : KBarrier.t
  val affinity : KAffinity.t
  val sharelocks : int option
end

module Make
    (Cfg:Config)
    (P:sig type code end)
    (A:Arch_litmus.Base)
    (T:Test_litmus.S with type P.code = P.code and module A = A)
    (O:Indent.S)
    (Lang:Language.S
    with type arch_reg = T.A.reg and type t = A.Out.t
    and module RegMap = T.A.RegMap) :
    sig
      val dump : Name.t -> T.t -> unit
    end =
  struct

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
    end

    module U = SkelUtil.Make(UCfg)(P)(A)(T)

    let iter_outs f proc = List.iter (f proc)

    let iter_all_outs f test =
      List.iter
        (fun (proc,(_,(outs,_))) -> iter_outs f proc outs)
        test.T.code

    let dump_loc_name loc =  match loc with
    | A.Location_reg (proc,reg) -> A.Out.dump_out_reg proc reg
    | A.Location_global s -> s
    | A.Location_deref (s,i) -> sprintf "%s_%i" s i

    module DC =
      CompCond.Make(O)
        (struct
          let with_ok = false
          module C = T.C
          module Loc = struct
            type t = A.location
            let compare = A.location_compare
            let dump loc = "_" ^ dump_loc_name loc
          end
        end)

    let cast_type loc =
      if A.arch = `X86_64 then
        match loc with
        | A.Location_reg (proc,r) -> "(" ^ CType.dump (A.typeof r) ^ ")"
        | _ -> ""
      else ""

    let do_dump_cond_fun env cond =
      let find_type loc =
        let t = match CType.strip_atomic (U.find_type loc env) with
        | CType.Base "atomic_t" ->  CType.Base "int"
        | t -> t in
        CType.dump (CType.strip_atomic t),CType.is_ptr t in
      DC.fundef find_type cond cast_type

    let dump_cond_fun env test = do_dump_cond_fun env test.T.condition

    let dump_cond_fun_call test dump_loc dump_val =
      DC.funcall test.T.condition dump_loc dump_val

    let dump_filter env test = match test.T.filter with
    | None -> ()
    | Some f ->
        let find_type loc =
          let t = U.find_type loc env in
          CType.dump (CType.strip_atomic t),CType.is_ptr t in
        DC.fundef_prop "filter_cond" find_type f cast_type


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
    let dump_loc_idx loc = sprintf "%s_f" (dump_loc_name loc)

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
      O.o "typedef u64 outcome_t[NOUTS];" ;
      O.o "" ;
      let _ =
        A.LocSet.fold
          (fun loc pos ->
            O.f "static const int %s = %i ;" (dump_loc_idx loc) pos ;
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
                let sloc = dump_loc_name loc in
                match U.find_type loc env with
                | CType.Pointer _ ->
                    sprintf "pretty_addr[(int)o[%s_f]]" sloc
                | CType.Base "atomic_t" ->
                     sprintf "(int)o[%s_f]" sloc
                | CType.Array _ -> assert false
                | t -> sprintf "(%s)o[%s_f]" (CType.dump t) sloc)
              (A.LocSet.elements outs))) in
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
      O.o "" ;
      let do_module_param t v = O.f "module_param(%s,%s,0644);" v t in
      let module_param = do_module_param "uint" in
      module_param "nruns" ;
      module_param "size" ;
      module_param "stride" ;
      module_param "avail" ;
      module_param "ninst" ;
      do_module_param "int" "affincr" ;
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

    let dump_a_v v =
      let open Constant in
      match v with
      | Concrete i -> A.V.Scalar.pp Cfg.hexa i
      | Symbolic ((s,None),0) -> dump_a_addr s
      | Label _ ->
          Warn.user_error "No label value for klitmus"
      | Symbolic _|Tag _ ->
          Warn.user_error "No tag nor indexed access for klitmus"

    let dump_ctx env test =
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
      O.o "typedef struct {" ;
      O.o "/* Shared locations */" ;
      List.iter
        (fun (s,t) ->
          let pp_t = SkelUtil.dump_global_type s (CType.strip_volatile t) in
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
      begin let open KBarrier in
      match Cfg.barrier with
      | User ->
          O.oi "int *barrier;" ;
      | TimeBase ->
          O.oi "sense_t *barrier;" ;
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
      if List.exists (fun (_,t) -> is_srcu_struct(t)) test.T.globals
      then begin
        O.o "static void cleanup_srcu_structs(struct srcu_struct *p,int sz) {" ;
        O.oi "for (int _i = 0 ; _i < sz ; _i++) cleanup_srcu_struct(&p[_i]);" ;
        O.o "}" ;
        O.o ""
      end ;
      O.o "static void free_ctx(ctx_t *p) { " ;
      O.oi "if (p == NULL) return;" ;
      let free tag = O.fi "if (p->%s) kfree(p->%s);" tag tag in
      List.iter
        (fun (s,t) ->
          if is_srcu_struct t then begin
            O.fi "if (p->%s) {" s ;
            let sz = do_spinsize "size" in
            O.fii "cleanup_srcu_structs(p->%s,%s);" s sz ;
            O.fii "kfree(p->%s);" s ;
            O.oi "}"
          end else begin
            free s
          end)
        test.T.globals ;
      iter_all_outs
        (fun proc (reg,_) ->
          let tag = A.Out.dump_out_reg proc reg in
          free tag) test ;
      begin
        let open KBarrier in
        match Cfg.barrier with
        | User -> free "barrier"
        | TimeBase -> O.oi "free_sense(p->barrier);"
      end ;
      O.oi "kfree(p);" ;
      O.o "}" ;
      O.o "" ;
      O.o "static ctx_t *alloc_ctx(size_t sz) { " ;
      O.oi "ctx_t *r = kzalloc(sizeof(*r),GFP_KERNEL);" ;
      O.oi "if (!r) { return NULL; }" ;
      let alloc sz tag =
        O.fi "r->%s = kmalloc(sizeof(r->%s[0])*%s,GFP_KERNEL);" tag tag sz;
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
                alloc "sz" s)
        test.T.globals ;
      iter_all_outs
        (fun proc (reg,_) ->
          let tag = A.Out.dump_out_reg proc reg in
          alloc "sz" tag) test ;
      begin let open KBarrier in
      match Cfg.barrier with
      | User -> alloc "sz" "barrier"
      | TimeBase ->
          O.oi "r->barrier = alloc_sense();";
          O.oi "if (!r->barrier) { return NULL; }"
      end ;
      O.oi "return r;" ;
      O.o "}" ;
      O.o "" ;
      O.o "static void init_ctx(ctx_t *_a,size_t sz) {" ;
      O.oi "for (int _i = 0 ; _i < sz ; _i++) {" ;
      List.iter
        (fun (s,_) ->
          let loc = A.Location_global s in
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
              if not (is_srcu_struct ty) then
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
      begin let open KBarrier in
      match Cfg.barrier with
      | User ->
          O.oii "_a->barrier[_i] = 0;"
      | TimeBase ->
          ()
      end ;
      O.oi "}" ;
      begin let open KBarrier in
      match Cfg.barrier with
      | User -> ()
      | TimeBase ->
          O.oi "barrier_init(_a->barrier);"
      end ;
      O.o "}" ;
      O.o "" ;
      ()

(***************)
(* Test proper *)
(***************)
let dump_barrier_def () =
  begin let open KBarrier in
  match Cfg.barrier with
  | User ->
      ObjUtil.insert_lib_file O.o "kbarrier-user.txt" ;
  | TimeBase ->
      ObjUtil.insert_lib_file O.o "kbarrier-tb.txt" ;
  end ;
  O.o ""


let dump_threads _tname env test =
  O.o "/***************/" ;
  O.o "/* Litmus code */" ;
  O.o "/***************/" ;
  O.o "" ;
  let global_env = U.select_global env in
  let global_env =
    List.map
      (fun (x,ty) -> x,CType.strip_volatile ty)
      global_env in
  List.iter
    (fun (proc,(out,(_outregs,envVolatile))) ->
      let myenv = U.select_proc proc env in
      Lang.dump_fun O.out myenv global_env envVolatile proc out ;
      O.f "static int thread%i(void *_p) {" proc ;
      O.oi "ctx_t *_a = (ctx_t *)_p;" ;
      O.o "" ;
      O.oi "smp_mb();" ;
      O.oi "for (int _j = 0 ; _j < stride ; _j++) {" ;
      O.oii "for (int _i = _j ; _i < size ; _i += stride) {" ;
      begin let open KBarrier in
      match Cfg.barrier with
      | User ->
          O.fiii "barrier_wait(%i,_i,&_a->barrier[_i]);" proc
      | TimeBase ->
          O.oiii "barrier_wait(_a->barrier);"
      end ;
      let tr_idx t idx = match Cfg.sharelocks with
      | Some _ when is_srcu_struct t || is_spinlock_t t ->
          sprintf "%s %% %s" idx spinsize
      | Some _|None -> idx in
      Lang.dump_call (LangUtils.code_fun proc)
        tr_idx O.out (Indent.as_string indent3)
        myenv global_env envVolatile proc out ;
      O.oii "}" ;
      O.oi "}" ;
      O.oi "atomic_inc(&done);" ;
      O.oi "smp_mb();" ;
      O.oi "wake_up(wq);" ;
      O.oi "smp_mb();" ;
      O.oi "do_exit(0);" ;
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
  O.oiv "_idx += _incr; ";
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
  let dump_a_loc loc =
    match U.find_type loc env with
    | CType.Base "atomic_t" ->
        sprintf "atomic_read(&_a->%s[_i])"  (dump_loc_name loc)
    | _ ->
        sprintf "_a->%s[_i]" (dump_loc_name loc) in
  O.oiv "outcome_t _o;" ;
  O.oiv "int _cond;" ;
  begin match test.T.filter with
  | None -> ()
  | Some f ->
      O.fiv "if (!%s) continue;"
        (DC.funcall_prop "filter_cond" f dump_a_loc dump_a_addr)
  end ;
  O.fiv "_cond = %s;"
    (dump_cond_fun_call test dump_a_loc dump_a_addr)  ;
  let locs = U.get_displayed_locs test in
  A.LocSet.iter
    (fun loc ->
      O.fiv "_o[%s] = %s;"
        (dump_loc_idx loc)
        (let sloc = dump_a_loc loc in
        if U.is_ptr loc env then sprintf "idx_addr(_a,_i,%s)" sloc
        else sloc))
    locs ;
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
  O.o "static int\nlitmus_proc_show(struct seq_file *m,void *v) {" ;
  O.oi "if (ninst == 0 || ninst * nthreads > nonline) {" ;
  let fmt = "%s: skipped\\n" in
  O.fii "seq_printf(m,\"%s\",\"%s\");" fmt tname ;
  O.oii "return 0;" ;
  O.oi "} else {" ;
  O.oi "return do_it(m);" ;
  O.oi "}" ;
  O.o "}" ;
  O.o "" ;
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
  O.o ""

(**************************)
(* Init, Exit and friends *)
(**************************)

let dump_init_exit _test =
  O.o "static int __init" ;
  O.o "litmus_init(void) {" ;
  O.oi "int err=0;" ;
  O.oi "struct proc_dir_entry *litmus_pde = proc_create(\"litmus\",0,NULL,&litmus_proc_fops);" ;
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
  O.oi "return 0; " ;
  O.o "clean_ctx:" ;
  O.oi "for (int k=0 ; k < ninst ; k++) free_ctx(ctx[k]);" ;
  O.oi "kfree(ctx);" ;
  O.o  "clean_online:" ;
  O.oi "kfree(online);" ;
  O.o  "clean_pde:" ;
  O.oi "remove_proc_entry(\"litmus\",NULL);" ;
  O.oi "return err;" ;
  O.o "}" ;
  O.o "" ;

  O.o "static void __exit" ;
  O.o "litmus_exit(void) {" ;
  O.oi "for (int k=0 ; k < ninst ; k++) free_ctx(ctx[k]);" ;
  O.oi "kfree(ctx);" ;
  O.oi "kfree(online);" ;
  O.oi "remove_proc_entry(\"litmus\",NULL);" ;
  O.o "}" ;
  O.o "" ;
  O.o "module_init(litmus_init);" ;
  O.o "module_exit(litmus_exit);" ;
  O.o "" ;
  O.o "MODULE_LICENSE(\"GPL\");" ;
  O.o "MODULE_AUTHOR(\"Luc\");" ;
  O.o "MODULE_DESCRIPTION(\"Litmus module\");" ;
  ()



let dump name test =
  ObjUtil.insert_lib_file O.o "header.txt" ;
  let tname = name.Name.name in
  let env = U.build_env test in
  dump_header () ;
  dump_params tname test ;
  dump_outs env test ;
  dump_barrier_def () ;
  dump_ctx env test ;
  dump_threads tname env test ;
  dump_filter env test ;
  dump_cond_fun env test ;
  dump_zyva tname env test ;
  dump_proc tname test ;
  dump_init_exit test ;
  ()


end
