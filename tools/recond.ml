(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(********************)
(* Change condition *)
(********************)

open Printf

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "recond"

let nprog = Filename.basename prog


module type Config = sig
  val verbose : int
  val check_cond : string -> string option
  val check_name : string -> bool
  val outcomes : bool
  val asobserved : bool
  val toexists : bool
  val hexa : bool
end

module Make(Config:Config)(Out:OutTests.S) =
  struct
    module D = Splitter.Default
    module LU = LexUtils.Make(D)
    module S = Splitter.Make(D)

    let mk_dump_loc map = match map with
    | None -> MiscParser.dump_location
    | Some m ->
        fun loc ->
          let pp = MiscParser.dump_location loc in
          StringMap.safe_find pp pp m

    let dump_outcomes chan dump_loc c =
      CondUtils.fold_outcomes c
        (fun bds () ->
          Out.fprintf chan "  " ;
          List.iter
            (fun (loc,v) ->
              Out.fprintf chan "%s=%s;"
                (dump_loc loc)
                (SymbConstant.pp Config.hexa v))
            bds ;
          Out.fprintf chan "\n") ()

    let dump_observed chan dump_loc c =
      Out.fprintf chan "Observed\n" ;
      let _ =
        CondUtils.fold_outcomes c
          (fun bds prefix ->
            Out.fprintf chan "%s" prefix ;
            let pp =
              List.map
                (fun (loc,v) ->
                  sprintf "%s=%s;"
                    (dump_loc loc)
                    (SymbConstant.pp Config.hexa v)) bds in
            let pp = String.concat " " pp in
            Out.fprintf chan "%s\n" pp ;
            "and ")
          "    " in
      ()

    let dump_locs out dump_loc locs = match locs with
    | [] -> ()
    | _::_ ->
        Out.fprintf out "%s\n"
          (DumpUtils.dump_locations dump_loc locs)

    let reparse map =
      if
        Config.outcomes || Config.asobserved ||
        Config.toexists || (match map with Some _ -> true | None -> false)
      then
        fun mk x -> LogConstr.parse_locs_cond (mk x)
      else
        fun _ _ -> None

    let toexists cond =
      let open ConstrGen in
      match cond with
      | ExistsState _ -> cond
      | NotExistsState p -> ExistsState p
      | ForallStates (And []) -> ExistsState (And [])
      | ForallStates p -> ExistsState (Not p)

    let from_chan idx_out fname in_chan =
      try
        let { Splitter.locs = locs; start = start; name=name; info; _} =
          S.split fname in_chan in
        if Config.check_name name.Name.name then begin
          if Config.verbose > 0 then
            eprintf "Let's go on %s\n%!" fname ;
          let map =
            try
              let map = List.assoc OutMapping.key info in
              let map =
                try LexOutMapping.parse map with _ -> assert false in
              let map = OutMapping.inverse map in
              Some map
            with Not_found -> None in
          let base = Filename.basename fname in
          let out = Out.open_file base in
          Misc.output_protect_close Out.close
            (fun out ->
              let _,_,(constr_start,constr_end),(last_start,loc_eof) = locs in
              let echo sec =
                let lexbuf = LU.from_section sec in_chan in
                Echo.echo_fun lexbuf (Out.put_char out)  in
              echo (start,constr_start) ;

              let echocond =
                not
                  (Config.asobserved || Config.toexists || Misc.is_some map) in
              let cond_checked =  Config.check_cond name.Name.name in
              let echo_cond c = match c with
              | Some f ->  Out.fprintf out "%s\n" f
              | None -> echo (constr_start,constr_end) in

              let cond =
                begin match cond_checked with
                | Some f ->
                    if echocond then echo_cond cond_checked ;
                    reparse map Lexing.from_string f
                | None ->
                    if
                      not
                        (Config.asobserved || Config.toexists) then
                      echo_cond None ;
                    let sec = constr_start,constr_end in
                    reparse None (LU.from_section sec) in_chan
                end in
              if Config.asobserved then begin match cond with
              | Some (locs,cond) ->
                  dump_locs out (mk_dump_loc map) locs ;
                  dump_observed out (mk_dump_loc map) cond ;
              | None -> assert false
              end else
                let module D =
                  LogConstr.Dump
                    (struct
                      let hexa = Config.hexa
                      let tr = match map with
                      | None -> Misc.identity
                      | Some m ->
                          fun loc -> StringMap.safe_find loc loc m
                    end) in
                if Config.toexists then begin match cond with
                | Some (locs,cond) ->
                    begin match cond,map with
                    | ConstrGen.ExistsState _,None ->
                        echo_cond cond_checked
                    | _ ->
                        dump_locs out (mk_dump_loc map) locs ;
                        D.dump (Out.chan out) (toexists cond) ;
                        Out.fprintf out "\n"
                  end
                | None -> assert false
              end else begin match cond with
              | Some (locs,cond) ->
                  dump_locs out (mk_dump_loc map) locs ;
                  D.dump (Out.chan out) cond ;
                  Out.fprintf out "\n"
              | None -> ()
              end ;
              echo (last_start,loc_eof) ;
              if Config.outcomes then begin match cond with
              | None -> ()
              | Some (_,c) ->
                  Out.fprintf out "(* Outcomes: \n" ;
                  dump_outcomes out (mk_dump_loc map) c ;
                  Out.fprintf out "*)\n"
              end ;
              ())
            out ;
          Out.fprintf idx_out "%s\n" base
        end
      with LexMisc.Error (msg,pos) ->
        Printf.eprintf
          "%a: Lex error %s (in %s)\n" Pos.pp_pos pos msg fname ;
        raise Misc.Exit

    let from_file idx_chan name =
      if Config.verbose > 0 then eprintf "Test file %s\n" name ;
      try
        Misc.input_protect
          (fun in_chan -> from_chan idx_chan name in_chan)
          name
      with Misc.Exit -> ()
      | Misc.Fatal msg ->
          eprintf "Fatal error is not fatal, %s\n" msg

    let from_args args =
      if Config.verbose > 0 then
        eprintf "Scanning test files from %s\n"
          (String.concat "," args) ;
      let idx_out = Out.open_all () in
      Misc.output_protect_close Out.close
        (fun idx_out ->
          Misc.iter_argv (from_file idx_out) args)
        idx_out ;
      Out.tar ()

  end

(**********)
(* Driver *)
(**********)

let tar = ref None
and hexa = ref false
and conds = ref []
and verbose = ref 0
and outcomes = ref false
and asobserved = ref false
and toexists = ref false
let names = ref []
let excl = ref []

let set_conds c = conds := !conds @ [c]
let set_tar x = tar := Some x
let args = ref []

let opts =
  [ "-v",
    Arg.Unit (fun () -> incr verbose),
    " be verbose";
    "-hexa",
    Arg.Bool (fun b -> hexa := b),
    "<bool> set hexadecimal output";
    CheckName.parse_names names ;
    CheckName.parse_excl excl ;
    "-conds",
    Arg.String set_conds,
    "<name> specify conditions of tests (can be repeated)";
    "-o", Arg.String set_tar,
    "<name> output to directory or tar file <name>" ;
    "-asobserved", Arg.Bool (fun b -> asobserved := b),
    sprintf
      "<bool> disguise final condition as an observation, default %b"
      !asobserved;
    "-outcomes", Arg.Bool (fun b -> outcomes := b),
    sprintf
      "<bool> include a list of matching outcomes as a comment, default %b"
      !outcomes;
    "-toexists", Arg.Bool (fun b -> toexists := b),
    sprintf
      "<bool> change quantifier to exists, default %b"
      !toexists;
  ]

let () =
  Arg.parse opts
    (fun a -> args := a :: !args)
    (sprintf "Usage %s [options] [test]*" prog)

(* Read names *)
module Check =
  CheckName.Make
    (struct
      let verbose = !verbose
      let rename = []
      let select = []
      let names = !names
      let excl = !excl
    end)

(* Read conditions *)
module LR = LexRename.Make(struct let verbose = !verbose end)
let conds = LR.read_from_files !conds (fun s -> Some s)

let () =
  if !verbose > 0 then
    eprintf "%s: conditions loaded\n%!" nprog

let from_args =
  let module X =
    Make
      (struct
        let verbose = !verbose
        let outcomes = !outcomes
        let asobserved = !asobserved
        let toexists = !toexists
        let check_name = Check.ok
        let check_cond name = TblRename.find_value_opt conds name
        let hexa = !hexa
      end) in
  match !tar with
  | None ->
      let module Y = X(OutStd) in
      Y.from_args
  | Some n as t ->
      let module T =
        OutTar.Make
          (struct
            let verbose = !verbose
            let outname = t
          end) in
      let module Y = X(T) in
      if !verbose > 0 then eprintf "%s: output to %s\n%!" nprog n ;
      Y.from_args

let () =
  if !verbose > 0 then
    eprintf "%s: calling from_args\n%!" nprog

let () = from_args !args
