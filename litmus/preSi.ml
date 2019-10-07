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
end

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

      open CType

(*******************************************)
(* Set compile time parameters from config *)
(*******************************************)
      module Insert =
        ObjUtil.Insert
          (struct
            let sysarch = Cfg.sysarch
            let word = Cfg.word
          end)

      module EPF =
        DoEmitPrintf.Make
          (struct
            let emitprintf = Cfg.stdio
            let ctr = Fmt.I32
          end)(O)

      let have_timebase = Insert.exists "timebase.c"
      let have_cache = Insert.exists "cache.c"

(*************)
(* Utilities *)
(*************)
      module UCfg = struct
        let memory = Memory.Direct
        let preload = Cfg.preload
        let mode = Cfg.mode
        let kind = Cfg.kind
        let hexa = Cfg.hexa
        let exit_cond = Cfg.exit_cond
      end
      module U = SkelUtil.Make(UCfg)(P)(A)(T)
      module UD = U.Dump(O)(EPF)

      let find_addr_type a env = U.find_type (A.Location_global a) env

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
        O.o "#include <stdlib.h>" ;
        O.o "#include <inttypes.h>" ;
        O.o "#include <unistd.h>" ;
        O.o "#include <errno.h>" ;
        O.o "#include <assert.h>" ;
        O.o "#include <time.h>" ;
        O.o "#include <limits.h>" ;
        O.o
          (if Cfg.stdio then "#include <stdio.h>"
          else "#include \"litmus_io.h\"") ;
        O.o "#include \"litmus_rand.h\"" ;
        O.o "#include \"utils.h\"" ;
        if Cfg.c11 then O.o "#include <stdatomic.h>";
        if true then begin (* Affinity always used *)
          O.o "#include \"affinity.h\""
        end ;
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
        O.f "#define NSTEPS %i" nsteps ;
        O.f "#define NSTEPS2 ((NSTEPS-1)/2)" ;
        O.o "#define STEP (DELTA_TB/(2*(NSTEPS-1)))" ;
        ()

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
          Insert.insert O.o "timebase.c"
        end

(* Memory barrier *)
      let dump_mbar_def () =
        O.o "/* Full memory barrier */" ;
        Insert.insert O.o "mbar.c" ;
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
        let fname =
          function
            | `PPC
            | `X86
            | `X86_64
            | `ARM
            | `MIPS
            | `AArch64 ->
                sprintf "barrier%s.c" lab_ext
            | _ -> assert false in
        Insert.insert O.o (fname Cfg.sysarch)

(**************)
(* Topologies *)
(**************)
      let get_all_vars test =
        let all = List.map fst test.T.globals in
        let vs =
          List.map
            (fun (_,(out,_)) -> A.Out.get_addrs out)
            test.T.code in
        all,vs

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
              let mode = Mode.PreSi
            end) (O) in
        O.o "/************/" ;
        O.o "/* Topology */" ;
        O.o "/************/" ;
        O.o "" ;
        Topo.dump_alloc (let _,vss = get_all_vars test in vss)

(************)
(* Outcomes *)
(************)

      let dump_loc_tag = function
        | A.Location_reg (proc,reg) -> A.Out.dump_out_reg proc reg
        | A.Location_global s -> s
        | A.Location_deref (s,i) -> sprintf "%s_%i" s i

      let does_pad t =
        let open CType in
        match t with
        | Pointer _
        | Array (("int"|"int32_t"|"uint32_t"|"int64_t"|"uint64_t"),_)
        | Base ("int"|"int32_t"|"uint32_t"|"int64_t"|"uint64_t") -> true
        | _ -> false

      let dump_loc_tag_coded loc =  sprintf "%s_idx" (dump_loc_tag loc)

      let choose_dump_loc_tag loc env =
        if U.is_ptr loc env then  dump_loc_tag_coded loc
        else  dump_loc_tag loc

(* Collected locations *)

      let fmt_outcome test env locs =
        U.fmt_outcome test
          (fun t -> match Compile.get_fmt Cfg.hexa t with
          | CType.Direct fmt|CType.Macro fmt ->
              if Cfg.hexa then "0x%" ^ fmt else "%" ^ fmt)
          locs env

      let dump_addr_idx s = sprintf "_idx_%s" s

      let dump_outcomes env test =
        let locs = U.get_displayed_locs test in
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
                 (List.map (fun (a,_) -> (sprintf "*%s") a) locs)) ;
            O.o "} vars_t;"
        end ;
        O.o "" ;
        UD.dump_vars_types test ;
        O.o "typedef struct {" ;
        let fields =
          A.LocSet.fold
            (fun loc k -> (U.find_type loc env,loc)::k)
            locs [] in
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
          (fun (t,loc) ->
            if CType.is_ptr t then
              O.fi "int %s;" (dump_loc_tag_coded loc)
            else match loc with
            | A.Location_global a ->
                O.fi "%s %s;"
                  (SkelUtil.dump_global_type a t) (dump_loc_tag loc)
            | _ ->
                O.fi "%s %s;"
                  (CType.dump t) (dump_loc_tag loc))
          fields ;
        if pad  then O.oi "uint32_t _pad;" ;
        O.o "} log_t;" ;
        O.o "" ;
(* There are some pointers in log *)
        let some_ptr =  U.ptr_in_outs env test in
        if some_ptr then begin
          (* To log actual pointers *)
          O.o "#define SOME_PTR 1" ;
          O.o "typedef struct {" ;
          A.LocSet.iter
            (fun loc ->
              let t = U.find_type loc env in
              if CType.is_ptr t then
                O.fi "%s %s;"  (CType.dump t) (dump_loc_tag loc))
            locs ;
          O.o "} log_ptr_t;" ;
          O.o "" ;
          (* Define indices *)
          List.iteri
            (fun k (a,_) ->
              O.f "static const int %s = %i;"
                (dump_addr_idx a) (k+1))
            test.T.globals ;
          O.o "" ;
          (*  Translation to indices *)
          let dump_test (s,_) =
            O.fi "else if (v_addr == p->%s) return %s;"
              s (dump_addr_idx s) in
          O.o "static int idx_addr(intmax_t *v_addr,vars_t *p) {" ;
          O.oi "if (v_addr == NULL) { return 0;}" ;
          List.iter dump_test test.T.globals ;
          O.oi "else { fatal(\"???\"); return -1;}" ;
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

        O.o "/* Dump of outcome */" ;
        O.o "static void pp_log(FILE *chan,log_t *p) {" ;
        let fmt = fmt_outcome test env locs
        and args =
          A.LocSet.map_list
            (fun loc -> match U.find_type loc env with
            | Pointer _ ->
                [sprintf "pretty_addr[p->%s]" (dump_loc_tag_coded loc)]
            | Array (_,sz) ->
                let rec pp_rec k =
                  if k >= sz then []
                  else
                    sprintf "p->%s[%i]" (dump_loc_tag loc) k::pp_rec (k+1) in
                pp_rec 0
            | _ ->
                [sprintf "p->%s" (dump_loc_tag loc)])
            locs in
        let args = List.concat args in
        EPF.fi ~out:"chan" fmt args ;
(*        O.fi "fprintf(chan,%s);" (String.concat "," (fmt::args)) ; *)
        O.o "}" ;
        O.o "" ;
        let locs = A.LocSet.elements locs in (* Now use lists *)
        O.o "/* Equality of outcomes */" ;
        O.o "static int eq_log(log_t *p,log_t *q) {" ;
        O.oi "return" ;
        let do_eq loc suf =
          let loc = choose_dump_loc_tag loc env in
          O.fii "p->%s == q->%s%s" loc loc suf in
        let do_eq_array loc suf = match U.find_type loc env with
        | Array (_,sz) ->
            let tag = choose_dump_loc_tag loc env in
            let rec pp_rec k =
              if k < sz then begin
                let suf = if k = sz-1 then suf else " &&" in
                O.fii "p->%s[%i] == q->%s[%i]%s" tag k tag k suf ;
                pp_rec (k+1)
              end in
            pp_rec 0
        | _ -> do_eq loc suf in
        let rec do_rec = function
          | [] -> O.oii "1;" ;
          | [x] -> do_eq_array x ";"
          | x::rem  -> do_eq_array x " &&" ; do_rec rem in
        do_rec  locs ;
        O.o "}" ;
        O.o "" ;
        some_ptr
(*
  O.o "/* Hash of outcome */" ;
  ObjUtil.insert_lib_file O.o "_mix.h" ;
  O.o "" ;
  O.o "static uint32_t hash_log(log_t *p) {" ;
  O.oi "uint32_t a,b,c; ";
  O.oi "a = b = c = 0xdeadbeef;" ;

  let dump_loc_tag loc = choose_dump_loc_tag loc env in
  let rec do_rec = function
  | [] -> ()
  | [x] ->
  O.fi "a += p->%s;" (dump_loc_tag x) ;
  O.oi "final(a,b,c);"
  | [x;y;] ->
  O.fi "a += p->%s;" (dump_loc_tag x) ;
  O.fi "b += p->%s;" (dump_loc_tag y) ;
  O.oi "final(a,b,c);"
  | [x;y;z;] ->
  O.fi "a += p->%s;" (dump_loc_tag x) ;
  O.fi "b += p->%s;" (dump_loc_tag y) ;
  O.fi "c += p->%s;" (dump_loc_tag z) ;
  O.oi "final(a,b,c);"
  | x::y::z::rem ->
  O.fi "a += p->%s;" (dump_loc_tag x) ;
  O.fi "b += p->%s;" (dump_loc_tag y) ;
  O.fi "c += p->%s;" (dump_loc_tag z) ;
  O.oi "mix(a,b,c);" ;
  do_rec rem in
  do_rec locs ;
  O.oi"return c;" ;
  O.o "}" ;
  O.o "" ;
  ()
 *)



      let dump_cond_fun env test =
        let module DC =
          CompCond.Make(O)
            (struct

              let with_ok = true
              module C = T.C
              module Loc = struct
                type t = A.location
                let compare = A.location_compare
                let dump loc = sprintf "p->%s" (choose_dump_loc_tag loc env)
              end
            end) in

        let cast_type = assert false in (* Not iplemented yet *)
        begin match test.T.filter with
        | None -> ()
        | Some f ->
            DC.fundef_onlog_prop "filter_cond" f cast_type ;
            O.o "" ;
            ()
        end ;
        let cond = test.T.condition in
        DC.fundef_onlog cond cast_type ;
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

      let get_param_vars test = match  test.T.globals with
      | [] -> []
      | _::xs -> xs

      let get_param_delays =
        if have_timebase then fun test -> Misc.interval 1 (T.get_nprocs test)
        else fun _ -> []

      let mk_get_param_pos _env test = match test.T.globals with
      | [] -> fun _ -> assert false
      | (_,t0)::xs ->
          let sum,m =
            List.fold_left
              (fun (i,m) (a,t) ->
                let sz = SkelUtil.nitems t in
                i+sz,StringMap.add a i m)
              (SkelUtil.nitems t0,StringMap.empty) xs in
          eprintf "sum=%i\n" sum ;
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
                List.map (fun a -> proc,a) (A.Out.get_addrs out))
              test.T.code in
          List.flatten r
        end else []

      let get_stats test =
        let open SkelUtil in
        begin let tags = get_param_vars test in
        if tags = [] then [] else
        [{tags=List.map (fun (s,_) -> pvtag s) tags;
          name = "vars"; max="NVARS"; tag = "Vars";
          process=(fun s -> s);};] end @
        begin let tags = get_param_delays test in
        if tags = [] then []
        else
          [{tags = List.map pdtag tags ;
            name = "delays"; max="NSTEPS"; tag="Delays";
            process = (sprintf "%s-NSTEPS2")};] end @
        begin let tags = get_param_caches test in
        if tags = [] then []
        else
          [{tags = List.map pctag tags;
            name = "dirs"; max="cmax"; tag="Cache";
            process=(fun s -> s);};] end

      let dump_parameters _env test =
        let v_tags = List.map (fun (s,_) -> pvtag s) (get_param_vars test)
        and d_tags = List.map pdtag (get_param_delays test)
        and c_tags = List.map pctag (get_param_caches test) in
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
        if have_timebase then
          O.o "static int addnsteps(int x) { return x+NSTEPS2; }" ;
        O.o "" ;
        O.o "static parse_param_t parse[] = {" ;
        O.oi "{\"part\",&param.part,id,SCANSZ}," ;
        let vs =
          String.concat " "
            (List.map
               (fun tag -> sprintf "{\"%s\",&param.%s,id,NVARS}," tag tag)
               v_tags) in
        O.oi vs ;
        let ds =
          String.concat " "
            (List.map
               (fun tag -> sprintf "{\"%s\",&param.%s,addnsteps,NSTEPS}," tag tag)
               d_tags) in
        O.oi ds ;
        let cs =
          String.concat " "
            (List.map
               (fun tag -> sprintf "{\"%s\",&param.%s,id,cmax}," tag tag)
               c_tags) in
        O.oi cs ;
        O.o "};" ;
        O.o "";
        O.o "#define PARSESZ (sizeof(parse)/sizeof(parse[0]))" ;
        O.o "";
(* Print *)
        O.o "static void pp_param(FILE *out,param_t *p) {" ;
        let fmt =
          "{" ^
          String.concat ", "
            (List.map (fun tag -> sprintf "%s=%%i" tag) all_tags) ^
          "}"
        and params = List.map (sprintf "p->%s") all_tags  in
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
        O.o "" ;
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

      let dump_hash_def tname _env test =
        let locs = U.get_displayed_locs test in
        let hashsz = match Cfg.check_nstates tname with
        | Some sz -> 3*sz
        | None -> hash_size (A.LocSet.cardinal locs) in
        O.f "#define HASHSZ %i" hashsz ;
        O.o "" ;
        ObjUtil.insert_lib_file O.o "_hash.c" ;
        O.o "" ;
        O.o "static void pp_entry(FILE *out,entry_t *p, int verbose, char **group) {" ;
        let fmt = "%-6PCTR%c>" in
        EPF.fi fmt ["p->c";"p->ok ? '*' : ':'";] ;
        O.oi "pp_log(out,&p->key);" ;
        O.oi "if (verbose) {" ;
        EPF.fii " # " [] ;
        O.oii "pp_param(out,&p->p);" ;
        EPF.fii " %s" ["group[p->p.part]"];
        O.oi "}" ;
        EPF.fi "%c" ["'\\n'"] ;
        O.o "}" ;
        O.o ""

(*****************)
(* Test instance *)
(*****************)

      let dump_instance_def _env _test =
        O.o "/***************/" ;
        O.o "/* Memory size */" ;
        O.o "/***************/" ;
        O.o "" ;
        O.o "/* Cache line */" ;
        O.f "#define LINE %i" Cfg.line ;
        O.o "" ;
        ObjUtil.insert_lib_file O.o "_instance.c" ;
        O.o "" ;
        ()

(*****************)
(* Run test code *)
(*****************)


(* Responsability for initialising or collecting, per thread *)
      let responsible =
        let rec do_rec seen = function
          | [] -> []
          | vs::vss ->
              let vs =
                List.filter (fun v -> not (StringSet.mem v seen)) vs in
              vs::do_rec (StringSet.union (StringSet.of_list vs) seen) vss in
        do_rec StringSet.empty

(* Untouched variables, per thread + responsability *)
      let part_vars test =
        let all,vs = get_all_vars test in
        let touched_set = StringSet.unions (List.map StringSet.of_list vs)
        and all_set = StringSet.of_list all in
        let rem = StringSet.elements (StringSet.diff all_set touched_set) in
        let rems = Misc.nsplit (T.get_nprocs test) rem in
        let vss = List.map2 (@) rems vs in
        List.combine rems (responsible vss)


      let dump_run_thread
          env test _some_ptr stats global_env
          (_vars,inits) (proc,(out,(_outregs,envVolatile)))  =
        let my_regs = U.select_proc proc env in
        let addrs = A.Out.get_addrs out in (* accessed in code *)
        O.fi "case %i: {" proc ;
        (* Delays *)
        if have_timebase then begin
          O.oii "int _delay = DELTA_TB;" ;
          if proc <> 0 then
            O.fii "_delay += (_p->d%i - (NSTEPS-1)/2)*STEP;" proc
        end ;
        (* Initialize them*)
        List.iter
          (fun a ->
            let at =  find_addr_type a env in
            let v = A.find_in_state (A.Location_global a) test.T.init in
            let ins =
              let pp_const v =
                let open Constant in
                match v with
                | Concrete i -> A.V.Scalar.pp Cfg.hexa i
                | Symbolic ((s,None),_) ->
                    sprintf "(%s)_vars->%s" (CType.dump at) s
                | Label _ ->
                    Warn.fatal "PreSi mode cannot handle code labels (yet)"
                | Symbolic _|Tag _ ->
                    Warn.user_error "Litmus cannot handle tags" in
              match at with
              | Array (t,sz) ->
                  sprintf "for (int _j = 0 ; _j < %i ; _j++) %s"
                    sz
                    (U.do_store (Base t) (sprintf "%s[_j]" a) (pp_const v))
              | _ ->
                  U.do_store at (sprintf "*%s" a) (pp_const v) in
            O.fii "%s;" ins)
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
        (* Synchronise *)
        if have_timebase then O.oii "_ctx->next_tb = read_timebase();" ;
        O.oii "barrier_wait(_b);" ;
        if have_timebase then begin
          O.oii "tb_t _tb0 = _ctx->next_tb;" ;
          O.oii "int _delta;" ;
          O.oii "do { _delta = read_timebase() - _tb0; } while (_delta < _delay);"
        end ;
        (* Dump code *)
        Lang.dump
          O.out (Indent.as_string Indent.indent2)
          my_regs global_env envVolatile proc out ;
        O.oii "barrier_wait(_b);" ;
(* Collect shared locations final values, if appropriate *)
        let globs = U.get_displayed_globals test in
        if not (StringSet.is_empty globs) then begin
          let to_collect =
            StringSet.inter
              globs
              (StringSet.of_list inits) in
          let to_collect =
            A.LocSet.filter
              (fun loc -> match loc with
              | A.Location_global s|A.Location_deref (s,_) ->
                  StringSet.mem s to_collect
              | A.Location_reg _ -> false)
              (U.get_displayed_locs test) in
          A.LocSet.iter
            (fun loc ->
              let tag = dump_loc_tag loc in
              match U.find_type loc env with
              | Array (_,sz) ->
                  O.fii
                    "for (int _j = 0 ; _j < %i ; _j++) %s[_j] = %s[_j];"
                    sz (OutUtils.fmt_presi_index tag) tag
              | _ ->
                  O.fii "%s = *%s;"
                    ((if U.is_ptr loc env then OutUtils.fmt_presi_ptr_index
                    else OutUtils.fmt_presi_index) tag)
                    tag)
            to_collect ;
          O.oii "barrier_wait(_b);"
        end ;
        if proc = 0 then begin
          (* addresse -> code *)
          A.LocSet.iter
            (fun loc ->
              if U.is_ptr loc env then
                O.fii "%s = idx_addr((intmax_t *)%s,_vars);"
                  (OutUtils.fmt_presi_index (dump_loc_tag_coded loc))
                  (OutUtils.fmt_presi_ptr_index (dump_loc_tag loc)))
            (U.get_displayed_locs test) ;
          (* condition *)
          let id = match test.T.filter with
          | None -> Indent.indent2
          | Some _f ->
              O.oii "if (filter_cond(_log)) {" ;
              Indent.indent3 in
          O.ox id "int _cond = final_ok(final_cond(_log));" ;
          (* recorded outcome *)
          O.ox id "hash_add(&_ctx->t,_log,_p,1,_cond);" ;
          (* Result and stats *)
          O.ox id "if (_cond) {" ;
          let nid = Indent.tab id in
          O.ox nid "_ok = 1;" ;
          O.ox nid "(void)__sync_add_and_fetch(&_g->stats.groups[_p->part],1);" ;
          let open SkelUtil in
          List.iter
            (fun {tags; name; _} ->
              let idx =
                String.concat ""
                  (List.map (sprintf "[_p->%s]") tags) in
              O.fx nid "(void)__sync_add_and_fetch(&_g->stats.%s%s,1);" name idx)
            stats ;
          O.ox id "}" ;
          begin match test.T.filter with
          | None -> () | Some _ -> O.oii "}"
          end
        end ;
        O.oii "break; }" ;
        ()

      let dump_run_def env test some_ptr stats  =
        O.o "/*************/" ;
        O.o "/* Test code */" ;
        O.o "/*************/" ;
        O.o "" ;
        O.o "inline static int do_run(thread_ctx_t *_c, param_t *_p,global_t *_g) {" ;
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
                O.fi "%s volatile *%s = (%s volatile *)_vars->%s;" t a t a)
              globs ;
            ()
        end ;
        O.oi "barrier_wait(_b);" ;
        O.oi "switch (_role) {" ;
        let global_env = U.select_global env in
        List.iter2
          (dump_run_thread env test some_ptr stats global_env)
          (part_vars test)
          test.T.code ;
        O.oi "}" ;
        O.oi "return _ok;" ;
        O.o "}" ;
        O.o ""

(********)
(* zyva *)
(********)

      let dump_choose_params_def env test stats =
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
        O.oi "param_t *q = g->param;" ;
        let has_globals =
          match test.T.globals with
          | [] -> false
          | _::_ ->
              O.oi "intmax_t *_mem = ctx->mem;" ;
              O.oi "vars_t *_vars = &ctx->v;" ;
              true in
        O.o "" ;
        O.oi "for (int _s=0 ; _s < g->size ; _s++) {" ;
        let n = T.get_nprocs test in
        let ps =
          let open SkelUtil in
          List.fold_right
            (fun st k ->
              match st.name with
              | "dirs"|"vars" -> k
              | _ ->
                  List.fold_right
                    (fun tag k -> (tag,st.max)::k)
                    st.tags k)
            stats [] in
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
(* Location placement comes first, as cache setting depends on it *)
            if has_globals then begin
              List.iter
                (fun (a,_) ->
                  try
                    let pos = get_param_pos a in
                    (* Must come first [raises Not_found] *)
                    let tag = pvtag a in
                    O.fiii
                      "ctx->p.%s = comp_param(&c->seed,&q->%s,NVARS,0);"
                      tag tag ;
                    O.fiii "_vars->%s = _mem + LINESZ*ctx->p.%s + %i;"
                      a tag pos
                  with Not_found ->
                    O.fiii "_vars->%s = _mem;" a)
                vs ;
              ()
            end ;
(* Standard parameters *)
            if i=n-1 then O.oiii "ctx->p.part = part;" ;
            List.iter
              (fun (tag,max) ->
                O.fiii
                  "ctx->p.%s = comp_param(&c->seed,&q->%s,%s,0);" tag tag max ;)
              ps ;
(* Cache parameters, locations must be allocated *)
            if has_globals then O.oiii "barrier_wait(&ctx->b);" ;
            List.iter
              (fun (_proc,v as p) ->
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
                O.oiii "}" ;)
              cs ;
            O.oiii "break;")
          (List.combine vss (List.combine pss css)) ;
        O.oii "}" ;
        O.oii "(void)do_run(c,&ctx->p,g);" ;
        O.oi "}" ;
        O.o "}" ;
        O.o ""

      let dump_choose_def env test stats =
        dump_choose_params_def env test stats ;
        O.o "static void choose(int id,global_t *g) {" ;
        O.oi "param_t *q = g->param;" ;
        O.oi "thread_ctx_t c; c.id = c.seed = id;" ;
        O.o "" ;
        O.oi "if (q->part >=0) {" ;
        O.oii "set_role(g,&c,q->part);";
        O.oii "for (int nrun = 0; nrun < g->nruns ; nrun++) {" ;
        O.oiii
          "if (g->verbose>1) fprintf(stderr, \"Run %i of %i\\r\", nrun, g->nruns);" ;
        O.oiii "choose_params(g,&c,q->part);" ;
        O.oii "}" ;
        O.oi "} else {" ;
        O.oii "st_t seed = 0;" ;
        O.oii "for (int nrun = 0; nrun < g->nruns ; nrun++) {" ;
        O.oiii
          "if (g->verbose>1) fprintf(stderr, \"Run %i of %i\\r\", nrun, g->nruns);" ;
        O.oiii "int part = rand_k(&seed,SCANSZ);" ;
        O.oiii "set_role(g,&c,part);";
        O.oiii "choose_params(g,&c,part);" ;
        O.oii "}" ;
        O.oi "}" ;
        O.o "}" ;
        O.o ""


      let _dump_scan_def _env test =
        O.o "static void scan(int id,global_t *g) {" ;
        O.oi "param_t p,*q = g->param;" ;
        O.oi "thread_ctx_t c; c.id = id;" ;
        O.oi "int nrun = 0;" ;
        O.o "" ;
        O.oi "g->ok = 0;" ;
        O.oi "do {" ;
        O.oii "for (int part = 0 ; part < SCANSZ ; part++) {" ;
        O.oiii "if (q->part >= 0) p.part = q->part; else p.part = part;" ;
        O.oiii "set_role(g,&c,p.part);" ;
        (* Enumerate parameters *)
        let rec loop_delays i = function
          | [] ->
              O.ox i "if (do_run(&c,&p,g)) (void)__sync_add_and_fetch(&g->ok,1);" ;
              ()
          | d::ds ->
              let tag = pdtag d in
              O.fx i "for (int %s = 0 ; %s < NSTEPS ; %s++) {" tag tag tag ;
              O.fx (Indent.tab i)
                "if (q->%s >= 0) p.%s = q->%s; else p.%s = %s;"
                tag tag tag tag tag ;
              loop_delays (Indent.tab i) ds ;
              O.fx i "}" in
        let rec loop_caches i = function
          | [] -> loop_delays i (get_param_delays test)
          | c::cs ->
              let tag = pctag c in
              O.fx i "for (int %s = cflush ; %s < cmax ; %s++) {" tag tag tag ;
              O.fx (Indent.tab i)
                "if (q->%s >= 0) p.%s = q->%s; else p.%s = %s;"
                tag tag tag tag tag ;
              loop_caches (Indent.tab i) cs ;
              O.fx i "}" in
        let rec loop_vars i = function
          | [] -> loop_caches i (get_param_caches test)
          | (x,_)::xs ->
              let tag = pvtag x in
              O.fx i "for (int %s = 0 ; %s < NVARS ; %s++) {" tag tag tag ;
              O.fx (Indent.tab i)
                "if (q->%s >= 0) p.%s = q->%s; else p.%s = %s;"
                tag tag tag tag tag ;
              loop_vars (Indent.tab i) xs ;
              O.fx i "}" in
        loop_vars Indent.indent3 (get_param_vars test) ;
        O.oii "}" ;
        begin match Cfg.timelimit with
        | None -> ()
        | Some _ ->
            O.oii "if (id == 0) g->now = timeofday();"
        end ;
        O.oii "barrier_wait(&g->gb);" ;
        O.oii "if (++nrun >= g->nruns) break;" ;
        begin match Cfg.timelimit with
        | None -> ()
        | Some _ ->
            O.oiii "if ((g->now - g->start)/ 1000000.0 > TIMELIMIT) break;"
        end ;
        O.oi "} while (g->ok < g->noccs);" ;
        O.o "}" ;
        O.o ""

      let dump_zyva_def tname env test stats =
        O.o "/*******************/" ;
        O.o "/* Forked function */" ;
        O.o "/*******************/" ;
        O.o "" ;
(*        dump_scan_def env test ; *)
        dump_choose_def env test stats ;
        O.o "typedef struct {" ;
        O.oi "int id;" ;
        O.oi "global_t *g;" ;
        O.o "} zyva_t;" ;
        O.o "" ;
        O.o "static void *zyva(void *_a) {" ;
        O.oi "zyva_t *a = (zyva_t*)_a;" ;
        O.oi "int id = a->id;" ;
        O.oi "global_t *g = a->g;" ;
        O.oi
          (if Cfg.force_affinity then
            sprintf
              "force_one_affinity(id,AVAIL,g->verbose,\"%s\");"
              tname
          else
            "write_one_affinity(id);") ;
        O.oi "init_global(g,id);" ;
(*        O.oi "if (g->do_scan) scan(id,g); else choose(id,g);" ; *)
        O.oi "choose(id,g);" ;
        O.oi "return NULL;" ;
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
    ObjUtil.insert_lib_file O.o "header.txt" ;
    dump_header test ;
    dump_delay_def () ;
    dump_read_timebase () ;
    dump_mbar_def () ;
    dump_cache_def () ;
    dump_barrier_def () ;
    dump_topology test ;
    let env = U.build_env test in
    let stats = get_stats test in
    let some_ptr = dump_outcomes env test in
    dump_cond_def env test ;
    dump_parameters env test ;
    dump_hash_def doc.Name.name env test ;
    dump_instance_def env test ;
    dump_run_def env test some_ptr stats ;
    dump_zyva_def doc.Name.name env test stats ;
    dump_prelude_def doc test ;
    dump_main_def doc env test stats ;
    ()

end
