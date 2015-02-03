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

(**********************************)
(* Extract condition from test(s) *)
(**********************************)
open Printf

module type Config = sig
  val verbose : int
  val check_name : string -> bool
end

module Make(Config:Config) =
  struct
    module D = Splitter.Default
    module LU = LexUtils.Make(D)
    module S = Splitter.Make(D)

    let echo sec in_chan out_chan =
      let lexbuf = LU.from_section sec in_chan in
      Echo.echo lexbuf out_chan

    let from_chan chan fname in_chan =    
      try
        let { Splitter.locs = locs;  name=name; _} =
          S.split fname in_chan in
        if Config.check_name name.Name.name then begin
          let _,_,(constr_start,constr_end),(_last_start,_loc_eof) = locs in
          let echo sec =
            let lexbuf = LU.from_section sec in_chan in
            Echo.echo lexbuf chan in
          fprintf chan "%s \"" name.Name.name ;
          echo (constr_start,constr_end) ;
          fprintf chan "\"\n" ;
          ()
        end
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
let names = ref []

let args = ref []

let opts = 
  [ "-v", Arg.Unit (fun () -> incr verbose)," be verbose";
    "-names",
    Arg.String (fun s -> names := [s] @ !names),
    "<name> select tests whose names are listed in file <name> (cumulate when repeated)";
  ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mcond"

let () =
  Arg.parse opts
    (fun a -> args := a :: !args)
    (sprintf "Usage %s [options] [test]*" prog)

(* Read names *)
let names = match !names with
| [] -> None
| names -> Some (ReadNames.from_files names StringSet.add StringSet.empty)

module X =
 Make
   (struct
     let verbose = !verbose
     let check_name = match names with
     | None -> fun _ -> true
     | Some names -> (fun name -> StringSet.mem name names)
   end)

let () = X.from_args !args

