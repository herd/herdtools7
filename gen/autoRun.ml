(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
open Code
open AutoPhase
open AutoUtils

module Answers =
  Map.Make
    (struct
      type t = int
      let compare = compare
    end)

type base_ck = int StringMap.t

let pp_base chan m =
  StringMap.iter
    (fun k v -> fprintf chan "<%s,%i>" k v) 
    m 

type answers = string Answers.t

let pp_answers chan m =
  Answers.iter
    (fun n s -> fprintf chan "<%i,\"%s\">" n s)
    m

type ckpt = 
    { logs_ck : Strings.t ;
      base_ck : base_ck ;
      lastrun_ck : int ;
      answers_ck : answers ; }

let pp_ckpt chan c =
  fprintf chan "Last Run = %i\n" c.lastrun_ck ;
  fprintf chan "Base = %a\n" pp_base c.base_ck ;
  fprintf chan  "Logs = %a\n"
    (fun chan s -> Strings.pp chan "," output_string s)
    c.logs_ck ;
  fprintf chan "Answers = %a\n" pp_answers c.answers_ck ;
  flush chan 

let empty_ckpt =
  { logs_ck = Strings.empty ; base_ck = StringMap.empty ;
    lastrun_ck = -1 ; answers_ck = Answers.empty ; }

module Make(C:AutoConf.S) = struct
  open C
  module E = A.E
  module R = A.R
  module L = A.L

(* *)
(*
  let () =
  let p fmt = fprintf stderr fmt in
  p "arch = %s\n" (Misc.pp_arch A.A.arch) ;
  p "nprocs = %i\n" C.nprocs ;
  p "diy_sz = %i\n" C.diy_sz ;
  ()
 *)

(* One module is still configured the old way, using a reference... *)

  let () = if verbose > 1 then MySys.debug := true 

      
(*********)
(* Utils *)
(*********)
  let tar_ext =
    if C.opt.AutoOpt.compress then "tgz"
    else "tar"

  let untar tar =
    if C.opt.AutoOpt.compress then
      sprintf "( gunzip < %s | tar xmf - )" tar
    else
      sprintf "tar xmf %s" tar

  let remove name =
    if verbose > 1 then eprintf "Remove %s\n%!" name ;
    try Sys.remove name with _ -> ()

  let tarc tar arg =
    if C.opt.AutoOpt.compress then
      sprintf "tar zcf %s %s" tar arg
    else
      sprintf "tar cf %s %s" tar arg

        
  let open_process_in com =
    if verbose > 1 then eprintf "open_process_in: %s\n" com ;
    Unix.open_process_in com

  let uniq = sprintf "%i" (Unix.getpid ())

  let mk_uniq s = sprintf "%s.%s" s uniq

  let uniq_dist =
    sprintf "%s.%s"
      (Unix.gethostname ())
      uniq

  let mk_uniq_dist s = sprintf "%s.%s" s uniq_dist

(***********)
(* Parsing *)
(***********)

  let parse_relax s = R.parse_relax (LexUtil.One s)

  let parse_relaxs s =
    let rs = LexUtil.split s in
    let rs = R.parse_relaxs rs in
    let rs = R.expand_relaxs (fun _ _ -> assert false) rs in
    R.Set.of_list rs


(*********)
(* State *)
(*********)

  module Base = AutoBase.Make(A)

  module Outs = AutoOuts.Make(C)

  module FenceSet =
    MySet.Make
      (struct
        type t = R.fence
        let compare = A.A.compare_fence
      end)

(* Complete state *)
  type state =
      {
       nrun : int ; (* number of steps performed *)
       (* All relaxations involved *)
       all :  R.Set.t ;
       (* Initial safe set *)
       safe0 : R.Set.t ; 
       (* Cumulativity relaxation present in all *)
       cumul : FenceSet.t Config.cumul ;
       (* Three significant sets *)
       cur : R.Set.t ;
       safe_r : R.Set.t ;
       relaxed_r : R.Set.t ;
       (* Internal record of test subdirectories *)
       bases : Base.t ;
       (* Checkpoint data *)    
       lastrun : int ;
       logs : Strings.t ; 
       base_ntests : base_ck ;
       answers : answers ;
       (* List of tests not run again (as edge cycles) *)
       dontrun : Strings.t ;
       (* List of ok tests (as relaxations sets *)
       ok : Outs.outs ; no : Outs.outs ;
       (* interactive mode is forced and the checkpointed
          state has been pruned *)
       interactive_forced : bool ;
     }

  let checkpoint st =
    let name = Filename.concat my_dir AutoOpt.ckpt_name in
    let tname = sprintf "%s.tmp" name in

    let ckpt =
      { 
        logs_ck = st.logs ;
        base_ck  = st.base_ntests ;
        lastrun_ck = st.lastrun ;
        answers_ck = st.answers ;
      } in
    Misc.output_protect_gen open_out_bin
      (fun chan ->
        Marshal.to_channel chan
          ((C.opt,ckpt) : AutoOpt.t * ckpt) [])
      tname ;
    Sys.rename tname name ;
    if verbose > 1 then begin
      eprintf "** Saved state **\n" ;
      pp_ckpt stderr ckpt ;
      ()
    end ;
    ()


      
  let pp_state chan st =
    fprintf chan "** Step %i **\n" st.nrun ;
    fprintf chan "Testing: %a\n" R.pp_set st.cur ;
    fprintf chan "Relaxed: %a\n" R.pp_set st.relaxed_r ;
    fprintf chan "Safe   : %a\n%!" R.pp_set st.safe_r ;
    if verbose > 0 then Outs.pp chan "Ok" st.ok ;
    ()

  let pp_change chan tag old_set new_set =
    let more = R.Set.diff new_set old_set
    and less = R.Set.diff old_set new_set in
    let is_more = R.Set.is_empty more
    and is_less = R.Set.is_empty less in
    if not is_more then
      fprintf chan "Added %s: %a\n%!" tag R.pp_set more ;
    if not is_less then
      fprintf chan "Removed %s: %a\n%!" tag R.pp_set less ;
    ()


  let make_key phase st =
    let k =
      { Base.K.cur = st.cur ;
        Base.K.rel = st.relaxed_r ;
        Base.K.saf = st.safe_r ; } in
    { Base.Key.phase = phase ; Base.Key.key = k ; }

  let change_base key base bases st =
    { st with bases = Base.change key base bases }

(************)
(* Call diy *)
(************)

  let er e = R.ERS [E.plain_edge e]

  let remove_safe_rfi safe =
    let open R in
    let open E in
    let powd d = er (Po (Diff,Dir W,d)) in
    let fencedwd f d =  er (Fenced (f,Diff,Dir W,d)) in

    R.Set.filter
      (fun r ->
        match r with
        | ERS [{edge=Rf Int;}; {edge=Po (Diff,_,d);}] ->
            not (R.Set.mem (powd d) safe)
        | ERS [{edge=Rf Int;}; {edge=Fenced (f,Diff,_,d);}] ->
            not (R.Set.mem (fencedwd f d) safe)
        | _ -> true)
      safe

  let as_fence r =
    let open R in
    let open E in
    match r with
    | ERS [{edge=Fenced _;} as e;]
    | ERS [{edge=Rf Code.Ext;};{edge=Fenced _;} as e;]
    | ERS [{edge=Fenced _;} as e; {edge=Rf Code.Ext;};]
    | ERS [{edge=Rf Code.Ext;}; {edge=Fenced _;} as e; {edge=Rf Code.Ext;};]
      -> Some e
    | _ -> None

  let rfe = parse_relax "Rfe" 
  let relax_rfe = rfe


  let rfe_is_safe safes = R.Set.mem rfe safes

  let pp_rs chan =
    R.Set.pp chan "," 
      (fun chan r -> fprintf chan "%s" (R.pp_relax r))

  let diy_mode =
    if  C.opt.AutoOpt.transitive then "transitive"
    else "critical"

  let dump_no dir dontrun =
    let fname = Filename.concat dir "no" in
    Misc.output_protect
      (fun chan ->
        Strings.iter (fprintf chan "%s\n") dontrun)
      fname

  let mk_conf cumul dir base sz maxrelax relax safe =
    let fname = Filename.concat dir (sprintf "%s.conf" base) in
    Misc.output_protect
      (fun chan ->
        fprintf chan "-arch %s\n" (Archs.pp A.A.arch) ;
        fprintf chan "-name %s\n" base ;
        fprintf chan "-mode %s\n" diy_mode ;
        fprintf chan "-cumul %s\n"
          (let x = cumul in
           let open Config in
           match x with
          | Empty -> "false"
          | All -> "true" 
          | Set rs ->
              FenceSet.pp_str "," A.A.pp_fence rs) ;
        fprintf chan "-size %i\n" sz ;
        fprintf chan "-nprocs %i\n" nprocs ;
        if maxrelax > 0 then fprintf chan "-maxrelax %i\n" maxrelax ;
        let pp_rs_opt opt rs =
          if not (R.Set.is_empty rs) then
            fprintf chan "%s %a\n" opt pp_rs rs in
        pp_rs_opt "-relax" relax ;
        pp_rs_opt "-safe" safe ;
        fprintf chan "-no ./no\n" ;
        List.iter (fprintf chan "%s\n") C.opt.AutoOpt.diy_opts ;
        ())
      fname ;
    fname

  let diy_dir = Filename.concat

  let is_directory name =
    try Sys.is_directory name
    with Sys_error _ -> false

  let mk_base_dir name =
    if is_directory name then ()
    else MySys.mkdir name

  let cumul_all = function
    | Config.All -> true
    | _ -> false

  let diy dir base st cumul =
    let safe = st.safe_r
    and relax = st.cur in

    let safe,relax,cumul =
      if rfe_is_safe safe then
        let safe = R.expand_cumul safe in
        let relax = R.Set.diff (R.expand_cumul relax) safe in 
        safe,relax,Config.All
      else
        safe,relax,cumul in

    let dir = diy_dir dir base in

    let safe = remove_safe_rfi safe in

    mk_base_dir dir ;
    dump_no dir st.dontrun ;

    if R.Set.is_empty relax then dir,0
    else
      let do_diy cumul maxrelax = 
        let cname =
          mk_conf cumul dir base diy_sz maxrelax relax safe in
        let com =
          sprintf "cd %s && diy -conf %s" dir (Filename.basename cname) in
        let chan = open_process_in com in
        let ntests = AutoLex.diy chan in
        ignore (Unix.close_process_in chan) ;
        ntests in
      
      let rec do_rec cumul n =
        if n > 2 then
          let open AutoOpt in
          match C.opt.interpretation with
          | Single -> 0
          | Multi ->
              if cumul_all cumul then 0
              else do_rec Config.All 1
        else
          let ntests = do_diy cumul n in
          if ntests = 0 then do_rec cumul (n+1)
          else ntests in

      let ntests = do_rec cumul 1 in
      dir,ntests

  let safe_conf dir base safe cumul =
    mk_conf cumul dir base diy_sz 0 R.Set.empty safe

  let safe_diy st dir base safe cumul brfi =
    let dir = Filename.concat dir base in
    let safe = if brfi then remove_safe_rfi safe else safe in
    mk_base_dir dir ;
    dump_no dir st.dontrun ;
    if R.Set.is_empty safe then dir,0
    else
      let cname = safe_conf dir base safe cumul in
      let com =
        sprintf "cd %s && diy -conf %s" dir (Filename.basename cname) in
      let chan = open_process_in com in
      let ntests = AutoLex.diy chan in
      ignore (Unix.close_process_in chan);
      if ntests = 0 then Warn.warn_always "no safe test was generated";
      dir,ntests

(***************)
(* Call litmus *)
(***************)

  let litmus_opts =
    let open AutoOpt in 
    match C.mach with
    | Simul _ | Local | Distant _ -> litmus_opts
    | Cross (_,addr) -> sprintf "-crossrun %s %s" addr litmus_opts

  let litmus dir base st =
    let tar_name =
      Filename.concat (Filename.get_temp_dir_name ())
        (sprintf "%s.%s" (mk_uniq base) tar_ext) in
    let no = Filename.concat dir "no"
    and all = Filename.concat dir "@all" in
    let com =
      sprintf "litmus %s -no %s -speedcheck dont %s -o %s"
        all no litmus_opts tar_name in
    if st.nrun > st.lastrun then begin
      MySys.exec_stdout com
    end ;
    tar_name

(*************) 
(* Run tests *)
(*************)
  let addpath = opt.AutoOpt.distaddpath 
  let cross_dir = C.opt.AutoOpt.work_dir

  module Compile =
    AutoCom.Make
      (struct
        open AutoOpt
        let addpath = addpath
        let target = match C.mach with
        | Simul _|Local -> AutoCom.Local
        | Distant addr|Cross (addr,_) -> AutoCom.Distant addr
      end)

  module Run =
    AutoCom.Make
      (struct
        open AutoOpt
        let addpath = addpath
        let target = match C.mach with
        | Simul _|Local|Cross (_,_) -> AutoCom.Local
        | Distant addr -> AutoCom.Distant addr
      end)


  let do_cleans dist_sh rm names =
    let com =
      sprintf "%s -rf %s" rm
        (String.concat " " names) in
    try dist_sh com with _ -> ()

  let compile_cleans = do_cleans Compile.dist_sh "/bin/rm"
  let run_cleans = do_cleans Run.dist_sh C.opt.AutoOpt.distrm

  let upload base dname tarname =
    let open AutoOpt in
    let dist_tar =
      Filename.concat
        dist_dir
        (sprintf "%s.%s" (mk_uniq_dist base) tar_ext) in
    let clean_all =
      match C.mach with
      | Local|Simul _|Distant _ ->
          (fun _ ->
            remove tarname ;
            compile_cleans [dist_tar; dname])
      | Cross (_,_) ->
          (fun _ ->
            remove tarname ;
            compile_cleans [dist_tar; dname] ;
            remove (sprintf "%s.tmp" (Filename.basename dist_tar)) ;
            run_cleans
              [Filename.basename dist_tar; Filename.basename dname]) in
    Sys.set_signal Sys.sigint
      (Sys.Signal_handle (fun _ -> clean_all () ; exit 1)) ;
(********************)
    Compile.dist_upload tarname dist_tar ;
    remove tarname ;
    let compile =
      sprintf
        "/bin/rm -rf %s && mkdir %s && cd %s && %s && /bin/rm %s && %s" 
        dname dname dname (untar dist_tar) dist_tar build in      
    begin match C.mach with
    | Local|Simul _|Distant _ ->
        begin
          try Compile.dist_sh compile
          with e -> compile_cleans [dist_tar; dname] ; raise e
        end ;
        dname
    | Cross (_,_) ->
        let dist_tar2 = Filename.basename dist_tar
        and dname2 = Filename.basename dname in
        let com =
          sprintf "%s && cd .. && %s"
            compile (tarc dist_tar dname2) in
        begin
          try Compile.dist_sh com
          with e -> compile_cleans [dist_tar; dname] ; raise e
        end ;        
        let inter =
          Filename.concat cross_dir dist_tar2 in
        begin try
          Compile.dist_download dist_tar inter ;
        with e ->
          compile_cleans [dist_tar] ;
          remove inter ;
          raise e
        end ;
        compile_cleans [dist_tar; dname] ;
        let com =
          sprintf "cd %s && %s && %s -f %s"
            cross_dir
            (untar dist_tar2) C.opt.AutoOpt.distrm dist_tar2 in
        let dname2 = Filename.concat cross_dir dname2 in
        begin try
          Run.dist_sh com ;
        with _ ->
          run_cleans [dname2] ;
        end ;
        dname2
    end          
      

  let run com dname dir base =

    let alloc_log base = AutoBase.alloc_log dir base in

    let rec do_run base xs st = match xs with
    | [] -> [],base,st
    | opts::rem ->
        let fname,base = alloc_log base in
        let st =
          if Strings.mem fname st.logs then st
          else begin
            let com = sprintf "cd %s && %s %s" dname com opts in
            begin try  Run.dist_sh_save com fname
            with e ->
              if dname <> dir then run_cleans [dname] ;
              raise e end ;
            let logs = Strings.add fname st.logs in
            let st = { st with logs = logs ; } in
            checkpoint st ;
            st
          end in
        let fnames,base,st = do_run base rem st in
        fname::fnames,base,st in

    do_run base run_opts


  let compile_and_run base tarname st =
    let dname = Filename.concat dist_dir (mk_uniq_dist base.AutoBase.id) in
    let dir = Filename.concat my_dir base.AutoBase.id in
    let dname =
      if st.nrun > st.lastrun then
        upload base.AutoBase.id dname tarname
      else dname in
    let r = run "sh run.sh" dname dir base st in
    if st.nrun > st.lastrun then begin
      let com = sprintf "%s -rf %s" C.opt.AutoOpt.distrm dname in
      Run.dist_sh com
    end ;
    Sys.set_signal Sys.sigint Sys.Signal_default ;
    r

  let simul_run base prog opts st =
    let prog = match prog with
    | "ppcmem" -> "ppcmem.sh"
    | _ -> prog in
    let dir = Filename.concat my_dir base.AutoBase.id in
    run
      (sprintf "%s -auto @all %s%s"
         prog opts
         (if verbose > 1 then "" else " 2>/dev/null"))
      dir dir base st

(****************)
(* Analyse logs *)
(****************)
  let tested_single o =
    R.Set.union
      (R.Set.of_list o.L.relaxs)
      (R.Set.of_list o.L.safes)

  let tested_multi st o =
    R.Set.unions
      (R.SetSet.elements
         (R.relaxs_of st.all (A.E.parse_edges o.L.cycle)))

  let tested_relaxs tested_one logs =
    R.Set.unions (List.rev_map tested_one logs)

  let read_logs show_ok cumul names st  =
    let logs = L.add_files names in
    let tested_one =
      if cumul_all cumul then tested_multi st
      else tested_single in
    let present = tested_relaxs tested_one logs in
    let yes,no =
      List.partition (fun o -> o.L.validates) logs in
    let dontrun =
      Strings.union
        (Strings.of_list
           (List.map (fun o -> o.L.cycle) yes))
        st.dontrun in
    
    let ok = Outs.diff (Outs.make st.all yes) st.ok in
    let new_ok = Outs.union ok st.ok in
    let outs_no = Outs.diff (Outs.make st.all no) new_ok in
    if (show_ok || verbose > 0) && yes <> []  then begin
      let chan = if show_ok then stdout else stderr in
      Outs.pp chan "Ok just read in log" ok
    end ;
    outs_no,{ st with dontrun = dontrun ; ok = new_ok ; },present

  let get_relaxed_assuming safe ok = Outs.get_relaxed_assuming safe ok


(* Safe heuristic *)
  let (++) x f = if R.Set.is_empty x then f () else x
    
  let get_safe_in_outs os st =    
    if verbose > 0 then Outs.pp stderr "No" os ;
    let os = Outs.simplify_for_safes st.relaxed_r st.cur os in
    if verbose > 0 then Outs.pp stderr "Simplified No" os ;
    let ok = Outs.simplify_for_safes st.relaxed_r st.cur st.ok in
    if verbose > 0 then Outs.pp stderr "Simplified Ok" ok ;
    let safe =
      Outs.safe_by_inter os
        ++ (fun () -> Outs.safe_by_cardinal ok os) in
    if verbose > 0 then begin
      eprintf "Safes from logs: %a\n%!" R.pp_set safe
    end ;
    safe

(* False safe heuristic *)

  let reduce_safe st =
    let rec do_rec safe ok =
      let ok = Outs.unexplained safe ok in
      if Outs.is_empty ok then safe
      else begin
        if verbose > 0 then Outs.pp stderr "Unexplained" ok ;
        let false_safe = Outs.false_safes st.safe0 safe ok  in
        if verbose > 0 then eprintf "False safes %a\n%!" R.pp_set false_safe ;
        do_rec (R.Set.diff safe false_safe) ok
      end in
    let safe = st.safe_r and ok = st.ok in
    let false_safe0 = R.Set.inter safe (get_relaxed_assuming st.safe0 st.ok) in
    let safe =
      if R.Set.is_empty false_safe0 then safe
      else begin
        if verbose > 0 then eprintf "False safes (exact) %a\n%!"
            R.pp_set false_safe0 ;
        (R.Set.diff safe false_safe0)
      end in
    do_rec safe ok


(********)
(* zyva *)
(********)
      
(* Generic diy & litmus & run *)

  let call_diy_litmus_run phase st cumul brfi =
    let key = make_key phase st in
    let base,bases = Base.look key st.bases in
    let base_id = base.AutoBase.id in
    let src_dir,base =
      if
        base.AutoBase.ntests < 0
      then
        if st.nrun > st.lastrun then begin
          let src_dir,ntests =
            match phase with
            | One -> diy my_dir base_id st cumul
            | Two -> safe_diy st my_dir base_id st.safe_r cumul brfi in
          let base = { base with AutoBase.ntests = ntests ; } in
          src_dir,base
        end else 
          diy_dir my_dir base_id,          
          try 
            let ntests = StringMap.find base_id st.base_ntests in
            { base with AutoBase.ntests = ntests ; }
          with Not_found -> assert false
      else diy_dir my_dir base_id,base in
    let ntests = base.AutoBase.ntests in
    printf "Phase %s in %s (%i tests)\n%!" (AutoPhase.pp phase) base_id ntests ;
    let st = change_base key base bases st in
    let st =
      { st with
        base_ntests = StringMap.add base_id  ntests st.base_ntests; } in
(* Result at last *)
    if ntests = 0 then
      None,st
    else begin
      let open AutoOpt in
      let logs,base,st = match C.mach with
      | Simul (prog,opts) ->
          simul_run base prog opts st
      | Local|Distant _|Cross (_,_) ->
          let tar_name = litmus src_dir base_id st in
          compile_and_run base tar_name st in
      let st = change_base key base bases st in
      Some logs,st
    end



(* Sub-Step one, find new relaxations to be relaxed or safe *)

(* Returns a pair bool X state, the boolean is false when obvious fixpoint is reached
   (ie sub-phase 1 does not produce any test *)      

  let phase_one st =
    let cumul = st.cumul in
    let logs,st = call_diy_litmus_run One st cumul true in
    match logs with
    | None -> st
    | Some logs ->
        let no,st,present = read_logs false cumul logs st in
        printf "Actually tested: %a\n%!" R.pp_set
          (R.Set.inter st.cur present) ;
(* This log may generate new relaxes. Do it now
   [to avoid false safes, eg [PodWR Fre] X 2 Ok and X 3 No *)
        let st =
          let new_relax = get_relaxed_assuming st.safe_r st.ok in
          pp_change stdout "relax" st.relaxed_r new_relax ;
          { st with relaxed_r = new_relax ; } in
(* Now find some safes... *)
        let seen_safe =  get_safe_in_outs no st in
        let new_safe = R.Set.union st.safe_r seen_safe in
        let new_relax = get_relaxed_assuming new_safe st.ok in
        pp_change stdout "safe" st.safe_r new_safe ;
        pp_change stdout "relax" st.relaxed_r new_relax ;
        let new_cur =
          R.Set.diff
            st.all (R.Set.union new_safe new_relax) in
        { st with 
          cur = new_cur ;
          safe_r = new_safe ;
          relaxed_r =  new_relax ; }


(* Sub-Step two, test safe relaxations indeed to be safe *)


  let phase_two st =
    let cumul =
      let open AutoOpt in
      match C.opt.interpretation with
      | Multi -> Config.All
      | Single -> st.cumul in
    let logs,st  = call_diy_litmus_run Two st cumul true in
    match logs with
    | None -> st
    | Some logs ->
        let _,st,present = read_logs false cumul logs st in
        let absent = R.Set.diff st.safe_r present in
        if not (R.Set.is_empty absent) then
          printf "Not tested: %a\n%!" R.pp_set absent ;
        let new_safe  = reduce_safe st in
        let new_relax = get_relaxed_assuming new_safe st.ok in
        pp_change stdout "safe" st.safe_r new_safe ;
        pp_change stdout "relax" st.relaxed_r new_relax ;
        let new_cur =
          R.Set.diff
            st.all (R.Set.union new_safe new_relax) in
        { st with 
          cur = new_cur ;
          safe_r = new_safe ;
          relaxed_r =  new_relax ; }


  let update_nrun st =
    let run_completed = st.nrun in
    let st =
      { st with
        nrun = run_completed+1 ;
        lastrun = max run_completed st.lastrun ; } in
    checkpoint st ;
    st

(* Conformance *)
  let rec conform_safe st stop =
    printf "** Step %i **\n%!" st.nrun ;
    if stop = 0 then st.ok
    else
      let cumul = Config.All in
      let logs,st = call_diy_litmus_run Two st cumul false in
      let st = 
        match logs with
        | None -> st
        | Some logs ->
            let _,st,present = read_logs true cumul logs st in
            let absent = R.Set.diff st.safe_r present in
            if not (R.Set.is_empty absent) then
              printf "Not tested: %a\n%!" R.pp_set absent ;
            st in
      conform_safe (update_nrun st) (stop-1)

  let conform st =
    let ok = conform_safe st opt.AutoOpt.stabilise in
    if Outs.is_empty ok  then begin
      printf "Safe set %a is conform\n" R.pp_set st.safe0
    end else begin
      printf "Safe set %a is not conform\n" R.pp_set st.safe0  ;
      Outs.pp stdout "Invalidating tests" ok
    end


(********)
(* Zyva *)
(********)

(* Stuff for asking user guidance *)

  let rec do_ask_user st =
    printf "Dear user, choose a subset of safe candidate relaxations from \n%a,\nor type 'all' for the whole set\n? %!"
      R.pp_set st.cur ;
    let line = read_line () in
    match line with
    | "all" -> line,st.cur
    | _ ->
        try line,R.Set.inter (parse_relaxs line) st.cur
        with Misc.Fatal msg -> eprintf "%s\n%!" msg ; do_ask_user st

  let ask_user_record st =    
    try
      let line = Answers.find st.nrun st.answers in
      printf "Previous run answer is: %s\n%!" line ;
      let safe = match line with
      | "all" -> st.cur
      | _ ->
          try R.Set.inter (parse_relaxs line) st.cur
          with Misc.Fatal _ -> assert false in
      safe,st 
    with Not_found ->
      if interactive || st.interactive_forced then
        let line,safe = do_ask_user st in
        safe,{ st with answers = Answers.add st.nrun line st.answers ; }
      else begin
        printf "Assuming %a to be safe\n%!" R.pp_set st.cur ;
        st.cur,st
      end
          
(*
  These settings of state will have two effects:
  1. force user input;
  2. start dumping checkpointed files now.
  Notice that some information extracted from the original checkpointed
  file is still present, such as the sub-directories of tests (aka base)
  and the next log to dump in those.
 *)
  let forget_up_to_now st =
    { st with
      answers = Answers.empty ;
      lastrun = st.nrun ;
    }

  let rec ask_user st =
    if C.opt.AutoOpt.force_interactive && not st.interactive_forced then
      ask_user
        (forget_up_to_now { st with interactive_forced = true ; } )
    else      
      let safe,st = ask_user_record st in
      let unexplained =
        Outs.unexplained (R.Set.union safe st.safe_r) st.ok in
      if Outs.is_empty unexplained then
        safe,st
      else begin
        printf
          "You cannot select %a!!!\n" 
          R.pp_set safe ;
        Outs.pp stdout "Unexplained Ok tests" unexplained ;
        if not interactive then begin
          printf "User selection of final safes required\n" ;
          printf "Restart as 'dont -restart -i'\n%!" ;
          exit 0
        end ;
        let st = { st with answers = Answers.remove st.nrun st.answers ; } in
        ask_user st
      end

(*
  Extract simple edges from composite  CR
*)

  module Expand = struct
    open R

    let rfe = E.parse_edge "Rfe"
    let is_rfe e = E.compare rfe e = 0

    let expand_one safes r = match r with
    | ERS []|PPO -> assert false
    | ERS [_] -> None
    | ERS es ->
        let non_safes =
          List.filter
            (fun e -> not (R.Set.mem (ERS [e]) safes))
            es in
        let non_safes = E.Set.elements (E.Set.of_list non_safes) in
        begin match non_safes with
        | [e] when not (is_rfe e) -> Some (ERS [e])
        | _ -> None
        end

  let expand_ones safes rs =
    let rs =
      R.Set.fold
        (fun r k -> match expand_one safes r with
        | None -> r::k
        | Some r0 -> r0::r::k)
        rs [] in
    R.Set.of_list rs

    let remove_all_safes safes =
      let rs =
        R.Set.fold
          (fun r k -> match r with
          | ERS (_::_::_ as es) ->
              if
                List.for_all
                  (fun e -> R.Set.mem (ERS [e]) safes)
                  es
              then k
              else r::k
          | _ -> r::k)
          safes [] in
      R.Set.of_list rs
            
(* Remove cumulativity from safes when rfe is safe *)
    let cumul safes =
      if R.Set.mem relax_rfe safes then
        let safes =
          R.Set.fold
            (fun r k -> match r with
            | ERS []|PPO -> assert false
            | ERS [_] -> r::k
            | ERS es ->
                let es = E.Set.of_list es in
                let yes,no = E.Set.partition is_rfe es in
                begin match
                  E.Set.as_singleton yes,E.Set.as_singleton no
                with
                | Some _,Some e -> ERS [e]::k
                | _,_ -> r::k
                end)
            safes [] in
        R.Set.of_list safes
      else safes

    let expand_nice (safes,relaxs) =
      let safes = expand_ones safes safes 
      and relaxs = expand_ones safes relaxs in
      let safes =  R.Set.diff safes relaxs in
(*      let safes = remove_all_safes safes in *)
      cumul safes,relaxs


    let rec fix (safes,relaxs as p)  =
      let s,r as p1 = expand_nice p in
      if R.Set.equal s safes && R.Set.equal r relaxs then p1
      else fix p1
  
    let nice st = fix (st.safe_r,st.relaxed_r) 
  end
    
            

  let over st =
    R.Set.iter
      (fun r ->
        let title =
          sprintf "Witness(es) for relaxed %s" (R.pp_relax r) in
        let w = Outs.get_relax_witnesses st.safe_r r st.ok in
        Outs.pp stdout title w)
      st.relaxed_r ;
(*
  let st = 
      if rfe_is_safe st.safe_r then expand_cumul_st st 
      else st in
    let relaxed = analyse_observed_relaxs st.relaxed_r st.safe_r in
    let safe = remove_safe_rfi st.safe_r in
*)
    let nice_safe,nice_relaxed = Expand.nice st in
    fprintf stdout "Observed relaxed: %a\n" R.pp_set nice_relaxed;
    fprintf stdout "Observed safe: %a\n" R.pp_set nice_safe ;    
    fprintf stdout "** Now checking safe set conformance **\n%!" ;
    let safe_check =
      Expand.remove_all_safes (Expand.cumul st.safe_r) in
    let st_conform =
      { st with safe_r = safe_check ;
        relaxed_r = R.Set.empty ;
        cur = R.Set.empty ; all = safe_check ; safe0 = safe_check ;
        ok = Outs.empty ;
        dontrun = Strings.empty ;
      } in    
    let ok = conform_safe st_conform opt.AutoOpt.stabilise in
    fprintf stdout "Observed relaxed: %a\n" R.pp_set nice_relaxed;
    fprintf stdout "Observed safe: %a\n" R.pp_set nice_safe ;
    if not (Outs.is_empty ok) then begin
      fprintf stdout "!!! The safe set failed conformance check !!!\n" ;
      Outs.pp stdout "Invalidating tests" ok
    end ;
    ()


(* Compute 'observed' current set *) 
let as_fences rs =
  R.Set.fold
    (fun r k -> match as_fence r with
    | Some e -> R.Set.add (R.ERS [e]) k
    | None -> k)
  rs R.Set.empty

  let reduce_current st =
    let safe_fences = as_fences st.safe_r in
    R.Set.fold
      (fun r k -> match as_fence r with
      | Some e ->
          if R.Set.mem (R.ERS [e]) safe_fences then k
          else R.Set.add r k
      | None -> R.Set.add r k)
      st.cur R.Set.empty

  let observed_current st =
    if rfe_is_safe st.safe_r then reduce_current st
    else st.cur

(* Zyva *)
  let st_same st1 st2 =
    R.Set.compare st1.safe_r st2.safe_r = 0 &&
    Outs.equal st1.ok st2.ok

  let rec explo st0 stop =
    pp_state stdout st0 ;
    let st1 = phase_one st0 in
    let st2 = phase_two st1 in
    let stop = if st_same st0 st2 then stop+1 else 0 in
    if stop < opt.AutoOpt.stabilise then
      explo (update_nrun st2) stop
    else if R.Set.is_empty (observed_current st2) then
      over (update_nrun st2)
    else
      let user_safe,st2 = ask_user st2 in
      let cur =  R.Set.diff st2.cur user_safe
      and safe =  R.Set.union st2.safe_r user_safe in
      let st2 = { st2 with cur = cur ; safe_r = safe ; } in
      if R.Set.is_empty (observed_current st2) then
        over (update_nrun st2)
      else
        explo (update_nrun st2) 0


(****************)
(* Entry points *)
(****************)

  let empty_state =
    { nrun = 0 ;
      cumul = Config.All ;
      lastrun = -1 ;
      all = R.Set.empty ;
      safe0 = R.Set.empty ;
      cur = R.Set.empty ; 
      safe_r = R.Set.empty ; relaxed_r = R.Set.empty ; 
      bases = Base.empty ;
      base_ntests = StringMap.empty ;
      logs = Strings.empty ;
      answers = Answers.empty ;
      dontrun = Strings.empty ;
      ok = Outs.empty;  no = Outs.empty;
      interactive_forced = false ;
    }


  let compute_cumul rs =
    let fs = FenceSet.of_list (R.all_fences rs) in
    let fcs = FenceSet.of_list (R.all_cumul_fences rs) in
    if FenceSet.is_empty fcs then
      Config.All
    else
      let r = FenceSet.diff fs fcs in
      if FenceSet.is_empty r then
        Config.Empty
      else
        Config.Set r
(*
  let concat r t = match r,t with
  | R.ERS rs,R.ERS ts -> R.ERS (rs@ts)
  | _,_ -> assert false
*)
  let restore_explo c =
    if verbose > 1 then begin
      eprintf "** State restored (explo) **\n" ;
      pp_ckpt stderr c
    end ;
    let cur = parse_relaxs C.testing
    and safe =  parse_relaxs C.safe in
    let all = R.Set.union cur safe in

    explo
      { empty_state with
        logs = c.logs_ck ;  lastrun = c.lastrun_ck ;
        base_ntests = c.base_ck ;
        answers = c.answers_ck ;
        cur = R.Set.diff cur safe ;
        safe_r = safe ; all = all ;
        safe0 = safe ;
        cumul = compute_cumul all ;
      } 0

  let go_explo () = restore_explo empty_ckpt

  let restore_conform c =
    let safe = parse_relaxs C.safe in
    conform 
      { empty_state with
        logs = c.logs_ck ;  lastrun = c.lastrun_ck ;
        base_ntests = c.base_ck ;
        answers = c.answers_ck ;
        safe_r = safe ;
        safe0 = safe ;
        all = safe ;
        (* Useless, but well *)
        cumul = Config.All ;
      }


  let go_conform () = restore_conform empty_ckpt ;
end
