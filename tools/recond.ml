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

(********************)
(*  Change condition *)
(********************)

open Printf

module type Out = sig
  type t
  val open_all : unit -> t
  val open_file : string -> t
  val close : t -> unit
  val put_char : t -> char -> unit
  val fprintf : t -> ('a, out_channel, unit) format -> 'a
  val tar : unit -> unit
end

module OutStd = struct
  type t = No | Out

  let open_all () = No
  let open_file _ = Out
  let close _ = ()

  let put_char out c = match out with
  | No -> ()
  | Out -> output_char stdout c

  let fprintf t fmt =  match t with
  | No -> ifprintf stdout fmt
  | Out -> fprintf stdout fmt

  let tar () = ()
end

module OutTar(O:Tar.Option) = struct
  module T = Tar.Make(O)

  type t = out_channel
  let do_open name = open_out (T.outname name)
  let open_all () = do_open "@all"
  let open_file name = do_open name
  let close chan = close_out chan
  let put_char = output_char
  let fprintf chan fmt = Printf.fprintf chan fmt
  let tar = T.tar
end

module type Config = sig
  val verbose : int
  val check_cond : string -> string option
  val check_name : string -> bool
  val outcomes : bool
  val asobserved : bool
end

module Make(Config:Config)(Out:Out) =
  struct
    module D = Splitter.Default
    module LU = LexUtils.Make(D)
    module S = Splitter.Make(D)

    let dump_outcomes chan c =
      CondUtils.fold_outcomes c
        (fun bds () ->
          Out.fprintf chan "  " ;
          List.iter
            (fun (loc,v) ->
              Out.fprintf chan "%s=%s;"
                (MiscParser.dump_location loc)
                (SymbConstant.pp_v v))
            bds ;
          Out.fprintf chan "\n") ()

    let dump_observed chan c =
      Out.fprintf chan "Observed\n" ;
      let _ =
        CondUtils.fold_outcomes c
          (fun bds prefix ->
            Out.fprintf chan "%s" prefix ;
            let pp =
              List.map
                (fun (loc,v) ->
                  sprintf "%s=%s;"
                    (MiscParser.dump_location loc)
                    (SymbConstant.pp_v v)) bds in
            Out.fprintf chan "%s\n" (String.concat " " pp) ;
            "and ")
          "    " in
      ()

    let reparse =
      if Config.outcomes || Config.asobserved then
        fun mk x -> LogConstr.parse_locs_cond (mk x)
      else
        fun _ _ -> None


    let from_chan idx_out fname in_chan =    
      try
        let { Splitter.locs = locs; start = start; name=name; _} =
          S.split fname in_chan in
        if Config.check_name name.Name.name then begin
          let base = Filename.basename fname in
          let out = Out.open_file base in
          Misc.output_protect_close Out.close        
            (fun out ->
              let _,_,(constr_start,constr_end),(last_start,loc_eof) = locs in
              let echo sec =
                let lexbuf = LU.from_section sec in_chan in
                Echo.echo_fun lexbuf (Out.put_char out)  in
              echo (start,constr_start) ;
              let cond =
                begin match Config.check_cond name.Name.name with
                | Some f ->
                    if not Config.asobserved then Out.fprintf out "%s\n" f ;
                    reparse Lexing.from_string f
                | None ->
                    let sec = constr_start,constr_end in
                    if not Config.asobserved then echo sec ;
                    reparse (LU.from_section sec) in_chan
                end in
              if Config.asobserved then begin match cond with
              | Some (_,cond) ->
                  dump_observed out cond
              | None -> assert false
              end ;
              echo (last_start,loc_eof) ;
              if Config.outcomes then begin match cond with
              | None -> ()
              | Some (_,c) ->
                  Out.fprintf out "(* Outcomes: \n" ;
                  dump_outcomes out c ;
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
      try
        Misc.input_protect
          (fun in_chan -> from_chan idx_chan name in_chan)
          name
      with Misc.Exit -> ()
      | Misc.Fatal msg ->
          eprintf "Fatal error is not fatal, %s\n" msg

    let from_args args =
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
and conds = ref []
and verbose = ref 0
and outcomes = ref false
and asobserved = ref false
let names = ref []

let set_conds c = conds := !conds @ [c]
let set_tar x = tar := Some x
let args = ref []

let opts = 
  [ "-v",
    Arg.Unit (fun () -> incr verbose),
    " be verbose";
    "-names",
    Arg.String (fun s -> names := [s] @ !names),
   "<name> select tests whose names are listed in file <name> (cumulate when repeated)";
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
  ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "recond"

let () =
  Arg.parse opts
    (fun a -> args := a :: !args)
    (sprintf "Usage %s [options] [test]*" prog)

(* Read names *)
let names = match !names with
| [] -> None
| names -> Some (ReadNames.from_files names StringSet.add StringSet.empty)

(* Read conditions *)
module LR = LexRename.Make(struct let verbose = !verbose end)
let conds = LR.read_from_files !conds (fun s -> Some s)


let from_args = 
  let module X =
    Make
      (struct
        let verbose = !verbose
        let outcomes = !outcomes
        let asobserved = !asobserved
        let check_name = match names with
        | None -> fun _ -> true
        | Some names -> (fun name -> StringSet.mem name names)
        let check_cond name = TblRename.find_value_opt conds name
      end) in
  match !tar with
  | None ->
      let module Y = X(OutStd) in
      Y.from_args
  | Some _ as t ->
      let module T =
        OutTar
          (struct
	    let verbose = !verbose
	    let outname = t
	  end) in
       let module Y = X(T) in
       Y.from_args

let () = from_args !args

