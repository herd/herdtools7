(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2012 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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
    | Atom (LL (loc,_)|LV (loc,_)) as p ->
        if MiscParser.LocSet.mem loc locs then p else And []
    | Or ps ->
        Or (List.map (proj_p locs)  ps)
    | And ps ->
        ands (List.map (proj_p locs)  ps)


  let proj locs = function
    | ForallStates p -> ForallStates (proj_p locs p)
    | ExistsState p -> ExistsState (proj_p locs p)
    | NotExistsState p -> NotExistsState (proj_p locs p)

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
              LogConstr.dump chan c ;
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
    | Misc.Fatal msg ->
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

