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
  type t = out_channel
  val open_file : string -> t
  val close : t -> unit
end

module OutTar(O:Tar.Option) = struct
  module T = Tar.Make(O)

  type t = out_channel
  let open_file name = open_out (T.outname name)
  let close chan = close_out chan
end

module type Config = sig
  val verbose : int
end

module Make(Config:Config)(Out:Out) =
  struct
    module D = Splitter.Default
    module LU = LexUtils.Make(D)
    module S = Splitter.Make(D)

    let dump_outcomes name chan c =
      let _k =
        CondUtils.fold_outcomes c
          (fun bds k ->
            let fname = sprintf "cond%02i.txt" k in
            Printf.fprintf chan "%s\n" fname ;
            Misc.output_protect_gen
              Out.open_file
              (fun chan ->
                Printf.fprintf chan "%s \"(%s)\"\n" name
                  (String.concat " /\\ "
                     (List.map
                        (fun (loc,v) ->
                          sprintf "%s=%s"              
                            (MiscParser.dump_location loc)
                            (SymbConstant.pp_v v))
                        bds)))
              fname ;
            k+1)
          0 in
      ()

    let from_chan fname in_chan =    
      try
        let { Splitter.locs = locs; name;_} =
          S.split fname in_chan in
        let _,_,(constr_start,constr_end),(_last_start,_loc_eof) = locs in
        let sec = constr_start,constr_end in
        let cond =
          match LogConstr.parse_locs_cond (LU.from_section sec in_chan) with
          | Some (_,cond) -> cond
          | None -> assert false in
        dump_outcomes name.Name.name stdout cond ;
      with LexMisc.Error (msg,pos) ->
        Printf.eprintf
	  "%a: Lex error %s (in %s)\n" Pos.pp_pos pos msg fname ;
        raise Misc.Exit
          
    let from_file name =
      try
        Misc.input_protect
          (fun in_chan -> from_chan name in_chan)
          name
      with Misc.Exit -> ()
      | Misc.Fatal msg ->
          eprintf "Fatal error, %s\n" msg ;
          raise Misc.Exit

  end

(**********)
(* Driver *)
(**********)

let tar = ref Filename.current_dir_name
and verbose = ref 0

let set_tar x = tar := x
let arg = ref None

let opts = 
  [ "-v",
    Arg.Unit (fun () -> incr verbose),
    " be verbose";
   "-o", Arg.String set_tar,
    sprintf
      "<name> output to directory or tar file <name>, default %s" !tar;
  ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "splitcond"

let () =
  Arg.parse opts
    (fun a -> match !arg with
    | None -> arg := Some a
    | Some _ ->
        raise (Arg.Bad "takes exactly one argument"))
    (sprintf "Usage %s [options] [test]*" prog)

let from_file = 
  let module X =
    Make
      (struct
        let verbose = !verbose
      end) in
  let module T =
    OutTar
      (struct
	let verbose = !verbose
	let outname = Some !tar
      end) in
  let module Y = X(T) in
  Y.from_file

let () = match !arg with
  | Some arg ->
      begin try from_file arg
      with Misc.Exit -> () end
  | None ->
      eprintf "%s takes exactly one argument!!!\n" prog ;
      exit 2

