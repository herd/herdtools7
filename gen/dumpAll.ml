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

open Printf

module type Config = sig
  include Top_gen.Config
  val family : string option
  val canonical_only : bool
  val fmt : int
  val no : string list
  val tarfile : string option
  val sufname : string option
  val addnum : bool
  val numeric : bool
  val lowercase : bool
  val overload : int option
  val cpp : bool
  val scope : Scope.t
  val info : MiscParser.info
  val stdout: bool
end

module Make(Config:Config)(T:Builder.S)

    = struct


      module Tar =
        Tar.Make
          (struct
            let verbose = Config.verbose
           let outname = Config.tarfile
          end)

      type edge = T.edge


(* Families *)

(* Environment for test names *)
      module Env = MyMap.Make(String)

(****************************)
(* High-level normalisation *)
(****************************)

      let normalise =
        if Config.canonical_only then
          let module Normer = Normaliser.Make(Config)(T.E) in
          fun cy ->
            let ncy = Normer.normalise cy in
            if Config.verbose > 0 then
              eprintf "Changed %s -> %s\n"
                (T.E.pp_edges cy)
                (T.E.pp_edges ncy) ;
            ncy
        else fun cy -> cy

      let mk_base = match Config.family with
      | Some b -> fun _cy -> b
      | None ->
          let module Normer = Normaliser.Make(Config)(T.E) in
          Normer.family

(* Complete name *)

      let mk_fmt base n = sprintf "%s%0*i" base Config.fmt n

      let add_suffix = match  Config.sufname with
      | None -> fun n -> n
      | Some s -> fun n -> n ^ s

      let global_mk_name =
        if Config.numeric then
          fun env base _es ->
            let n = try Env.find base env with Not_found -> 0 in
            add_suffix (mk_fmt base n),Env.add base (n+1) env
        else
          let module Namer = Namer.Make(T.A)(T.A)(T.E) in
          fun env base es -> add_suffix (Namer.mk_name base es),env

      exception DupName of string

(* No need to add disambiguating numbers to numeric names *)
      let addnum =  Config.addnum

      let dup_name =
        if addnum then
          fun env name ->
            let n = try Env.find name env with Not_found -> 0 in
            (match n with | 0 -> name | _ ->  mk_fmt name n),
            Env.add name (n+1) env
        else
          fun env name ->
            try
              let _ =  Env.find name env in
              raise (DupName name)
            with Not_found -> name,Env.add name 0 env


(**********************************)
(* Computation of cycle signature *)
(**********************************)

(*
  A bit contrieved...
  - change list to array,
  - find starting index of minimal sequence in array,
  - build the signature starting from this index.
 *)

(* Signatures as strings, for the sake of compacity *)
      module W = Warn.Make(Config)

      type sigs =
          { sig_next : int ; sig_map : int T.E.Map.t ; sig_set : StringSet.t}

      let get_sig sigs e =
        try T.E.Map.find e sigs.sig_map,sigs
        with Not_found ->
          let i = sigs.sig_next in
          W.warn "New sig: %s -> %i" (T.E.pp_edge e) i ;
          if i >  0xffff then
            Warn.warn_always
              "Signatures for are more than 2 bytes, expect duplicates" ;
          i,
          { sigs with
            sig_next=i+1; sig_map = T.E.Map.add e i sigs.sig_map; }

      let sig_of sigs out e =
        let i,sigs = get_sig sigs e in
        let c1 = i land 0xff in
        let c2 = (i lsr 8) land 0xff in
        out (Char.chr c1) ;
        out (Char.chr c2) ;
        sigs

      let sig_of_shift =
        let buff = Buffer.create 16 in
        let add c = Buffer.add_char buff c in

        fun sigs t k ->
          let sz = Array.length t in
          assert(sz > 0) ;
          let incr i = if i+1 >= sz then 0 else i+1 in
          let rec do_rec sigs i =
            let sigs = sig_of sigs add t.(i) in
            let j = incr i in
            if j=k then sigs else do_rec sigs j in
          let sigs = do_rec sigs k in
          let r = Buffer.contents buff in
          Buffer.clear buff ;
          r,sigs

      let have_seen sigs norm = match norm with
      | [] -> false,sigs
      | _::_ ->
          let t = Array.of_list norm in
          let xxx,sigs = sig_of_shift sigs t 0 in
          if StringSet.mem xxx sigs.sig_set then
            true,sigs
          else
            false,{ sigs with sig_set = StringSet.add xxx sigs.sig_set; }

(******************)
(* Internal state *)
(******************)

      type t =
          {
           ntests : int ;        (* number of tests outputed so far *)
           sigs : sigs ;     (* Signatures of compiled tests *)
           env : int Env.t ;     (* State for getting numeric names *)
           dup : int Env.t ;     (* State for getting fresh names *)
           relaxed : T.R.SetSet.t ;
         }

      type check = edge list list -> bool
      type info = (string * string) list
      (* `mk_info` is part of the prepared-test stage and should stay pure:
         it should only contain metadata derived earlier, not touch global
         state, perform I/O, or depend on output ordering. *)
      type mk_info = info * T.R.Set.t
      let no_info = [],T.R.Set.empty

      type mk_name =  edge list -> string option
      let no_name _ = None

      type mk_scope =  edge list -> BellInfo.scopes option
      let no_scope _ = None

      type generator =
          (edge list -> mk_info -> mk_name -> mk_scope -> t -> t) -> t -> t

      let empty_sig =
        { sig_next = 0 ; sig_map = T.E.Map.empty ; sig_set = StringSet.empty }

      let sigs_init cys =
        let cys = List.rev_map T.E.parse_edges cys in
        List.fold_left
          (fun k es ->
            let _,k = have_seen k es in
            k)
          empty_sig cys

      let empty_t =
        { ntests = 0 ;
          sigs = sigs_init Config.no ;
          env = Env.empty; dup = Env.empty;
          relaxed = T.R.SetSet.empty; }

(************************************** ****)
(* Check duplicates, compile and dump test *)
(*******************************************)


(* Adapt actual filename *)
      let tar_output_protect f name =
        Misc.output_protect f (Tar.outname name)

(* Compile & dump proper *)

      type prepared =
          {
           es : T.E.edge list ;
           relax_set : T.R.Set.t ;
           t : T.test ;
         }

(* Output test proper *)
      let do_dump_test all_chan t res =
        let n = T.get_name t in
        let src =
          sprintf "%s.%s" n (if Config.cpp then "c" else "litmus") in
        if Config.stdout then
          T.dump_test_channel stdout t
        else
          tar_output_protect
            (fun chan -> T.dump_test_channel chan t)
            src ;
(* And litmus file name in @all file *)
        if not Config.stdout then
          fprintf all_chan "%s\n" src ;
        if Config.verbose > 0 then eprintf "Test: %s\n" n ;
(*    printf "%s: %s\n" n (pp_edges cycle.orig) ; *)
        { res with ntests = res.ntests+1; }

      let build_test check es mk_info =
        let es,c = T.C.resolve_edges es in
        let c,init = T.C.finish c in
        let cy = T.E.pp_edges es in
        let info,relaxed = mk_info in
        let info = Config.info@("Cycle",cy)::info in
        let t = T.test_of_cycle "__tmp__" ~info ~check ~init es c in
        { es; relax_set=relaxed; t; }

(* Dump from prepared test, with specified scope tree *)
      let dump_test_st keep_name all_chan generated_test env n mk_st res =
        let t = generated_test.t in
        let st = mk_st (T.get_nprocs t) in
        let n =
          if keep_name then n
          else n ^ "+" ^ Namer.of_scope st in
        let n,dup = dup_name res.dup n in
        let t = T.set_name t n in
        let t = T.set_scope t st in
        let res =
          { res with
            env; dup; relaxed= T.R.SetSet.add generated_test.relax_set res.relaxed; } in
        do_dump_test all_chan t res

      let dump_test all_chan mk_name mk_scope generated_test res =
        let es = generated_test.es in
        let n,env = match mk_name es with
        | None ->
            let fam = mk_base es in
            let n,env = global_mk_name res.env fam es in
            n,env
        | Some n ->
            n,res.env in

        match Config.scope with
        | Scope.No ->
            let n,dup = dup_name res.dup n in
            let t = T.set_name generated_test.t n in
            let res =
              { res with
                env; dup; relaxed= T.R.SetSet.add generated_test.relax_set res.relaxed; } in
            do_dump_test all_chan t res
        | Scope.Default ->
            let keep_name,mk_st =
              (match mk_scope es with
              | None -> false,T.A.ScopeGen.default
              | Some st -> true,(fun _ -> st)) in
            dump_test_st keep_name all_chan generated_test env n mk_st res
        | Scope.One st ->
            dump_test_st false all_chan generated_test env n
              (fun _ -> st) res
        | Scope.Gen scs ->
            let res =
              { res with
                env; relaxed= T.R.SetSet.add generated_test.relax_set res.relaxed; } in
            T.A.ScopeGen.gen scs (T.get_nprocs generated_test.t)
              (fun st res ->
                let n = n ^ "+" ^ Namer.of_scope st in
                let n,dup = dup_name res.dup n in
                let t = T.set_name generated_test.t n in
                let t = T.set_scope t st in
                let res = { res with dup;} in
                do_dump_test all_chan t res)
              res
        | Scope.All ->
            let res =
              { res with
                env; relaxed= T.R.SetSet.add generated_test.relax_set res.relaxed; } in
            T.A.ScopeGen.all (T.get_nprocs generated_test.t)
              (fun st res ->
                let n = n ^ "+" ^ Namer.of_scope st in
                let n,dup = dup_name res.dup n in
                let t = T.set_name generated_test.t n in
                let t = T.set_scope t st in
                let res = { res with dup;} in
                do_dump_test all_chan t res)
              res

      (* Duplication checks and dump the tests
         `all_chan` is the shared @all output channel.
         `check` is the last-minute cycle predicate.
         `es` is the candidate edge list from the caller.
         `mk_info`, `mk_name`, and `mk_scope` are caller-provided hooks.
         `res` is the current dumper accumulator. *)
      let check_dump all_chan check es mk_info mk_name mk_scope res =
        try
          T.E.varatom es
          |> List.fold_left
            (fun res es ->
              (* `T.E.resolve_edges` is the edge-level step: normalize the
                 expanded edge list before logging and cycle construction. *)
              let es = T.E.resolve_edges es |> normalise in
              let res =
                if Config.canonical_only then begin
                  let seen,sigs = have_seen res.sigs es in
                  if seen then Warn.fatal "Duplicate" ;
                  { res with sigs }
                end else res in
              if Config.debug.Debug_gen.generator then begin
                eprintf "------------------------------------------------------\n" ;
                eprintf "Cycle: %s\n" (T.E.pp_edges es) ;
              end;
              let test = build_test check es mk_info in
              dump_test all_chan mk_name mk_scope test res)
            res
        with
        (* `DupName` happens when the final emitted test name collides,
           usually after scope/name decoration in the dump path. *)
        | DupName name ->
          if Config.verbose > 0 then
            eprintf "Duplicate name: %s\n" name ;
          res
        | Normaliser.CannotNormalise msg ->
          if Config.verbose > 0 then
            eprintf "Cannot normalise error: %s\n" msg ;
          res
        | Misc.Fatal msg|Misc.UserError msg ->
          if Config.verbose > 0 then
            eprintf "Fatal ignored: %s\n" msg ;
          res
        |Misc.Exit ->
            res
(* Exported *)
      let all ?(check=(fun _ -> true)) gen =
        let output f =
          if Config.stdout then
            f stderr
          else
            tar_output_protect f "@all"
        and print = if Config.stdout then eprintf else printf in
        output (fun all_chan ->
            fprintf all_chan
              "# %s\n" (String.concat " " (Array.to_list Sys.argv)) ;
            fprintf all_chan "# Version %s, Revision: %s\n"
              Version.version Version.rev ;
            let res =  gen (check_dump all_chan check) empty_t in
            flush stderr ;
            print
              "Generator produced %d tests\n%!"
              res.ntests ;
            if
              T.R.SetSet.exists (fun r -> not (T.R.Set.is_empty r)) res.relaxed
            then
              print "Relaxations tested: %a\n"
                T.R.pp_set_set res.relaxed) ;
        Tar.tar () ;
        Hint.close_out Config.hout

    end
