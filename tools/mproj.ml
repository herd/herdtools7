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

(*********************************************)
(* Project condition w.r.t. set of locations *)
(*********************************************)

open Printf

module type Config = sig
  val verbose : int
  val locs : MiscParser.LocSet.t TblRename.t
end

module Make(Config:Config) =
struct
  module D = Splitter.Default
  module LU = LexUtils.Make(D)
  module S = Splitter.Make(D)
  open ConstrGen

  let check_locs name =
    try Some (TblRename.find_value Config.locs name)
    with Not_found -> None

  let is_true = function
    | And [] -> true
    | _ -> false

  let ors ps =
    if List.exists is_true ps then And []
    else Or ps

  let ands ps =
    let ps = List.filter (fun p -> not (is_true p)) ps in
    And ps

  let rec proj_p locs p = match p with
    | Not _
    | Implies _ -> Warn.fatal "Bad condition in mproj"
    | Atom (LL (loc,_)|LV (Loc loc,_)) as p ->
        if MiscParser.LocSet.mem loc locs then p else And []
    | Atom (LV (Deref _,_)) ->
        prerr_endline "TODO" ; assert false
    | Atom (FF (_,x)) ->
        let loc = MiscParser.Location_global (Constant.check_sym x) in
        if MiscParser.LocSet.mem loc locs then p else And []
    | Or ps ->
        Or (List.map (proj_p locs)  ps)
    | And ps ->
        ands (List.map (proj_p locs)  ps)


  let proj locs = function
    | ForallStates p -> ForallStates (proj_p locs p)
    | ExistsState p -> ExistsState (proj_p locs p)
    | NotExistsState p -> NotExistsState (proj_p locs p)

  module Dump =
    LogConstr.Dump
      (struct
        let hexa = false
        let tr = Misc.identity
      end)

  let from_chan chan fname in_chan =
    try
      let { Splitter.locs = locs;  name=name; _} =
        S.split fname in_chan in
      let tname =  name.Name.name in
      match check_locs tname with
      | None -> ()
      | Some ls ->
          let _,_,(constr_start,constr_end),(_last_start,_loc_eof) = locs in
          let lexbuf = LU.from_section (constr_start,constr_end) in_chan in
          match LogConstr.parse_locs_cond lexbuf with
          | None -> ()
          | Some (ls0,c0) ->
              assert (ls0=[]) ;
              let c = proj ls c0 in
              fprintf chan "%s \"" tname ;
              Dump.dump chan c ;
              fprintf chan "\"\n" ;
              ()
    with LexMisc.Error (msg,pos) ->
      Printf.eprintf
	"%a: Lex error %s (in %s)\n" Pos.pp_pos pos msg fname ;
      raise Misc.Exit

  let from_file chan name =
    try
      Misc.input_protect
        (fun in_chan -> from_chan chan name in_chan)
        name
    with Misc.Exit -> ()
    | Misc.Fatal msg|Misc.UserError msg ->
        eprintf "Fatal error is not fatal, %s\n" msg

  let from_args args = Misc.iter_argv (from_file stdout) args
end

(**********)
(* Driver *)
(**********)

let verbose = ref 0

let args = ref []
let locs = ref []

let opts =
  [ "-v", Arg.Unit (fun () -> incr verbose)," be verbose";
    "-locs", Arg.String (fun s -> locs := !locs @ [s]), " <name> specify location files";]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mproj"

let () =
  Arg.parse opts
    (fun a -> args := a :: !args)
    (sprintf "Usage %s [options] [test]*" prog)

module Verbose = struct let verbose = !verbose end
module LR = LexRename.Make(Verbose)

module X =
 Make
   (struct
     let verbose = !verbose
     let locs = LR.read_from_files !locs LogConstr.parse_locs
   end)

let () = X.from_args !args
