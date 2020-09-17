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

open Printf

module type Config = sig
  val memory : Memory.t
  val preload : Preload.t
  val mode : Mode.t
  val kind : bool
  val hexa : bool
  val exit_cond : bool
  val have_fault_handler : bool
end

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
| Volatile t|Atomic t -> nitems t
| Base _|Pointer _ -> 1

let dump_fatom_tag d ((p,lbl),v) =
  sprintf "fault_P%d%s_%s" p (match lbl with None -> "" | Some lbl -> "_" ^ lbl) (d v)

let dump_pteval_flags s p =
  if PTEVal.is_default p then s
  else
    let open PTEVal in
    let add b s k = if b<>0 then s::k else k in
    let msk =
      add p.valid "msk_valid"
        (add p.af "msk_af"
           (add p.dbm "msk_dbm"
              (add p.db "msk_db" []))) in
    let msk = String.concat "|" msk in
    sprintf "litmus_set_pte_flags(%s,%s)" s msk

(* Skeleton utilities, useful for Skel and PreSi *)

module Make
    (Cfg:Config)
    (P:sig type code end)
    (A:Arch_litmus.Base)
    (T:Test_litmus.S with type P.code = P.code and module A = A) : sig

(* Info *)
      val get_info : string -> T.t -> string option
      val get_prefetch_info : T.t -> string

(* Typing utilities *)
      type env
      val pp_env : env -> string
      val build_env : T.t -> env
      val find_type : A.location -> env -> CType.t
      val find_type_align : string -> env -> CType.t option
      val is_aligned : string -> env -> bool
      val dump_mem_type : string -> CType.t -> env -> string

      val select_proc : int -> env -> CType.t A.RegMap.t
      val select_global : env -> (string * CType.t) list
      val select_aligned : env -> (string * CType.t) list

(* Some dump`ing stuff *)
      val cast_reg_type : A.location -> string
      val register_type : A.location ->  CType.t -> CType.t
      val fmt_outcome : T.t -> (CType.base -> string) -> A.LocSet.t -> env -> string
      val fmt_faults : A.V.v Fault.atom list -> string

(* Locations *)
      val get_displayed_locs : T.t -> A.LocSet.t
      val get_displayed_globals : T.t -> StringSet.t
      val get_displayed_ptes : T.t -> StringSet.t
      val get_observed_locs : T.t -> A.LocSet.t
      val get_observed_globals : T.t -> StringSet.t
      val get_stabilized : T.t -> StringSet.t
      val is_ptr : A.location -> env -> bool
      val ptr_in_outs : env -> T.t -> bool
      val get_faults : T.t -> A.V.v Fault.atom list
      val find_label_offset : Proc.t -> string -> T.t -> int

(* Instructions *)
      val do_store : CType.t -> string -> string -> string
      val do_load : CType.t -> string -> string

(* Condition *)
      val pp_cond : T.t -> string

(* Dump stuff *)
      module Dump : functor (O:Indent.S) -> functor(EPF:EmitPrintf.S) -> sig
        (* Some small dump functions common std/presi *)
        val dump_vars_types : T.t -> unit

            (* Same output as shell script in (normal) shell driver mode *)
        val prelude : Name.t -> T.t -> unit

            (* Dump results *)
        val postlude :
            Name.t -> T.t -> Affi.t option -> bool ->
              stat list -> unit
      end
    end = struct
      module G = Global_litmus
(* Info *)
      let get_info key test =
        try Some (List.assoc key test.T.info)
        with Not_found -> None

      let get_prefetch_info test = match get_info "Prefetch" test with
      | Some i -> i
      | None -> ""

(* Typing stuff *)
      type env =
          CType.t A.LocMap.t * CType.t StringMap.t

      let pp_env (env,_m) =
        A.LocMap.pp_str
          (fun loc t ->
            sprintf "%s -> %s" (A.pp_location loc) (CType.dump t))
          env

      let build_env test = test.T.type_env
(*
  let m =

 *)

          (*
            let e = A.LocMap.empty in
            let e =
            List.fold_left
            (fun e (s,t) ->
(*              eprintf "BUILD %s <%s>\n" s (CType.dump t) ; *)
            A.LocMap.add (A.Location_global s) t e)
            e test.T.globals in
            let e =
            List.fold_left
            (fun e (proc,(_,(outs, _))) ->
            List.fold_left
            (fun e  (reg,t) ->
            A.LocMap.add (A.Location_reg (proc,reg)) t e)
            e outs)
            e test.T.code in
            let pp = A.LocMap.fold
            (fun loc t k ->
            sprintf "%s -> %s" (A.pp_location loc) (CType.dump t)::k)
            e [] in
            eprintf "Env: {%s}\n" (String.concat "; " pp) ;
            e
           *)

      let do_find_type loc env =
        try A.LocMap.find loc env
        with Not_found -> Compile.base

      let find_type loc (env,_) = match loc with
      | A.Location_deref (s,_) ->
          begin match do_find_type (A.Location_global s) env with
          | CType.Array (t,_) -> CType.Base t
          | _ -> Warn.user_error "Non array %s refered as array" (G.pp s)
          end
      | _ -> do_find_type loc env

      let is_aligned loc (_,env) =
        try ignore (StringMap.find loc env) ; true with Not_found -> false

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
            | A.Location_reg (q,reg) when p = q -> Some reg
            | A.Location_global _ | A.Location_reg _  -> None
            | A.Location_deref _ -> assert false)
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
            | A.Location_global (G.Addr a) -> Some a
            | A.Location_deref _ -> assert false)
          env

      let select_aligned env =
        select_types
          (function
            | A.Location_reg _|A.Location_global (G.Pte _|G.Phy _) -> None
            | A.Location_deref _ -> assert false
            | A.Location_global (G.Addr loc) ->
                if is_aligned loc env then Some loc else None)
          env

(* Format stuff *)
      let cast_reg_type loc =
        if A.arch = `X86_64 then
          match loc with
          | A.Location_reg (_,r) -> "(" ^ CType.dump (A.typeof r) ^ ")"
          | _ -> ""
        else ""

      let tr_out test = OutMapping.info_to_tr test.T.info

      let pp_loc tr_out loc =  match loc with
      | A.Location_reg (proc,reg) -> tr_out (sprintf "%d:%s" proc (A.pp_reg reg))
      | A.Location_global s -> G.pp s
      | A.Location_deref (s,i) -> sprintf "%s[%d]" (G.pp s) i
      let register_type _loc t = t (* Systematically follow given type *)

      let fmt_outcome test pp_fmt_base locs env =
        let tr_out = tr_out test in
        let rec pp_fmt t = match t with
        | CType.Pointer _ -> "%s"
        | CType.Base "pteval_t" -> "(oa:%s, af:%d, db:%d, dbm:%d, valid:%d)"
        | CType.Base t -> pp_fmt_base t
        | CType.Atomic t|CType.Volatile t -> pp_fmt t
        | CType.Array (t,sz) ->
            let fmt_elt = pp_fmt_base t in
            let fmts = Misc.replicate sz fmt_elt in
            let fmt = String.concat "," fmts in
            sprintf "{%s}" fmt in

        A.LocSet.pp_str " "
          (fun loc ->
            sprintf "%s=%s;"
              (pp_loc tr_out loc)
              (pp_fmt (register_type loc (find_type loc env))))
          locs

      let fmt_faults fs = String.concat "" (List.map (fun _ -> "%s") fs)

(* Locations *)
      let get_displayed_locs t =
        A.LocSet.union
          (T.C.locations t.T.condition)
          (A.LocSet.of_list t.T.flocs)

      let filter_globals locs =
        A.LocSet.fold
          (fun a k -> match a with
          | A.Location_global (G.Addr a)
          | A.Location_deref (G.Addr a,_) -> StringSet.add a k
          | A.Location_reg _
          | A.Location_global (G.Pte _|G.Phy _)
          | A.Location_deref ((G.Pte _|G.Phy _),_)
            -> k)
          locs StringSet.empty

      let filter_ptes locs =
        A.LocSet.fold
          (fun a k -> match a with
          | A.Location_global (G.Pte a) ->  StringSet.add a k
          | A.Location_deref (G.Pte _,_) -> assert false
          | A.Location_reg _
          | A.Location_global (G.Phy _|G.Addr _)
          | A.Location_deref ((G.Phy _|G.Addr _),_)
            -> k)
          locs StringSet.empty

      let get_displayed_globals t = filter_globals (get_displayed_locs t)

      let get_displayed_ptes t = filter_ptes (get_displayed_locs t)

      let get_observed_locs t =
        let locs =  get_displayed_locs t in
        match t.T.filter with
        | None ->  get_displayed_locs t
        | Some filter ->
            A.LocSet.union locs (T.C.locations_prop filter)

      let get_observed_globals t =  filter_globals (get_observed_locs t)

      let get_stabilized t =
        let env = build_env t in
        StringSet.filter
          (fun loc -> not (is_aligned loc env))
          (get_observed_globals t)

      let is_ptr loc env =
        let t = find_type loc env in
       CType.is_ptr t

      let ptr_in_outs env test =
        let locs = get_displayed_locs test in
        A.LocSet.exists (fun loc ->is_ptr loc env ) locs

      let get_faults test = T.C.get_faults test.T.condition

      let find_label_offset p lbl test =
        try
          T.find_offset test.T.src.MiscParser.prog p lbl
        with Not_found ->
          let v = Constant.Label (p,lbl) in
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
        match a with
        | LV (loc,v) ->
            sprintf "%s=%s" (tr_out (A.pp_location loc)) (A.V.pp Cfg.hexa v)
        | LL (loc1,loc2) ->
            sprintf "%s=%s" (tr_out (A.pp_location loc1))
              (tr_out (A.pp_rval loc2))
        | FF f -> Fault.pp_fatom A.V.pp_v f

      let pp_cond test =
        let tr_out = tr_out test in
        ConstrGen.constraints_to_string (pp_atom tr_out) test.T.condition

      module Dump (O:Indent.S) (EPF:EmitPrintf.S) = struct

        let dump_vars_types test =
          let _,env = build_env test in
          let globs = test.T.globals in
          List.iter
            (fun (s,t) -> match t with
            | CType.Array (t,sz) ->
                O.f "typedef %s %s[%d];" t (type_name s) sz
            | _ ->
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
          O.oi "fprintf(out,\"Generated assembler\\n\");" ;
          O.oi "ass(out);" ;
          O.o "}" ;
          O.o "" ;
          ()

(* Postlude *)

        let pp_nstates nstates =
          EPF.fi "Histogram (%d states)\n" [nstates]

        let cstring s = sprintf "%S" s
        let show_stats = match Cfg.mode with
        | Mode.Kvm -> false
        | Mode.Std|Mode.PreSi -> true

        let postlude doc test affi show_topos stats =
          let t = if Cfg.exit_cond then "int" else "void" in
          O.o "#define ENOUGH 10" ;
          O.o "" ;
          begin match Cfg.mode with
          | Mode.Std ->
              O.f "static %s postlude(FILE *out,cmd_t *cmd,hist_t *hist,count_t p_true,count_t p_false,tsc_t total) {" t
          | Mode.PreSi ->
              O.f "static %s postl sude(FILE *out,global_t *g,count_t p_true,count_t p_false,tsc_t total) {" t ;
              O.oi "hash_t *hash = &g->hash ;"
          | Mode.Kvm ->
              O.f "static %s postlude(global_t *g,count_t p_true,count_t p_false,tsc_t total) {" t ;
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
          | Mode.PreSi ->
              pp_nstates "hash->nhash" ;
              O.oi "pp_hash(out,hash,g->verbose > 1,g->group);"
          | Mode.Kvm ->
              pp_nstates "hash->nhash" ;
              O.oi "pp_hash(hash,g->verbose > 1,g->group);" ;
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
              if Cfg.have_fault_handler then O.oi "pp_faults();" ;
              let s = sprintf "Time %s "  doc.Name.name in
              O.fi "puts(%S);" s ;
              O.oi "emit_double(tsc_millions(total));" ;
              O.oi "puts(\"\\n\");"
          end ;
          if Cfg.exit_cond then O.oi "return cond;" ;
          O.o "}" ;
          O.o "" ;
          ()

      end
    end
