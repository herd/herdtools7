(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


open Printf
open LogState

let verbose = ref 0
let logs = ref []
let select = ref []
let names = ref []
let rename = ref []
let excl = ref []
let npar = ref 1
let hexa = ref false

let options =
  let open CheckName in
  [  
  ("-q", Arg.Unit (fun _ -> verbose := -1),
   "<non-default> be silent");  
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
  ("-j", Arg.Int (fun i -> npar := i),
   (sprintf "<int> parallel sum using <n> processeses, default %i" !npar)) ;
   parse_hexa hexa;
   parse_rename rename;
   parse_select select; parse_names names; parse_excl excl;
 ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "sum"

let () =
  Arg.parse options
    (fun s -> logs := !logs @ [s])
    (sprintf "Usage %s [options]* [log]*
[log]* are litmus log file names. If no command line argument is given,
log names are taken from standard input.
Options are:" prog)

let npar = !npar
let select = !select
let rename = !rename
let names = !names
let excl = !excl
let verbose = !verbose
let hexa = !hexa

module Verbose = struct let verbose = verbose end

(* Options for recursive calls *)

let expn_opt opt xs k =
  List.fold_right
    (fun x k -> opt::x::k)
    xs k

let par_opts =
  expn_opt "-select" select
    (expn_opt "-rename" rename
       (expn_opt "-names" names
          (expn_opt "-excl" excl [])))

(* Now handle the same options, which are to be
   honored only when there are no recursive calls *)


module Check =
  CheckName.Make
    (struct
      let verbose = verbose
      let check v =  if npar <= 1 then v else []
      let rename = check rename
      let select = check select
      let names = check names
      let excl = check excl
    end)

let fnames = match !logs with
| [] ->
    let rec read_rec k =
      let o =
        try Some (read_line ())
        with End_of_file -> None in
      match o with
      | Some x -> read_rec (x::k)
      | None -> k in
    read_rec []
| xs ->  xs 


module LS = LogState.Make(Verbose)

module LL =
  LexLog.Make
    (struct
      let verbose = verbose
      include Check
      let hexa = hexa
    end)


let zyva fnames  =
  let tests = LL.read_names fnames in
  
(* Dumping of log files *)


  let dump_hash chan = function
  | None -> ()
  | Some h -> fprintf chan "Hash=%s\n" h in

  let dump_condition chan v c = match c,v with
  | Some c,(Ok|No) ->
      fprintf chan
        "Condition %a is%s validated\n"
        LogConstr.dump c
        (if v = Ok then "" else " not")
  | _,_ -> () in

  let dump_prop chan c = match c with
  | Some c ->
      fprintf chan
        "Condition (%a)\n"
        LogConstr.dump_prop (ConstrGen.prop_of c)
  | None -> () in

  let dump_test chan t =
    fprintf chan "Test %s%s\n" t.tname
      (if is_reliable t.kind then " "^LS.pp_kind t.kind else "") ;
    LS.dump_states chan t.states ;
    if is_reliable t.kind then begin
      fprintf chan "%s\n" (LS.pp_validation t.validation) ;
      fprintf chan "Witnesses\n" ;
      let p,n = t.witnesses in
      fprintf chan "Positive: %s Negative: %s\n"
        (Int64.to_string p) (Int64.to_string n) ;
      dump_condition chan t.validation t.condition 
    end else begin
      fprintf chan "??\n" ;
      dump_prop chan t.condition       
    end ;
    dump_hash chan t.hash ;    
    LS.dump_topologies chan t.topologies ;
    begin match t.time with
    | None -> ()
    | Some time -> 
        fprintf chan "Time %s %0.2f\n" t.tname time
    end ;
    output_char chan '\n';
    () in

  let dump_log chan =  Array.iter (dump_test chan) in

  let total_outcomes tsts =
    let k = ref Int64.zero in
    Array.iter
      (fun tst -> k := Int64.add !k (LS.get_nouts tst.states))
      tsts ;
    !k in

  let sum = LS.union_logs tests in
  dump_log stdout sum ;
  flush stdout ;
  if verbose >= 0 then
    eprintf "total nouts: %.2fM\n" (LS.millions (total_outcomes sum)) ;
  ()

module type Opt = sig
  val verbose : int
  val j : int
  val opts : string list
end

module Task(O:Opt) = struct
  let tmp = Filename.get_temp_dir_name ()

  let dir =
    let t = Filename.concat tmp (sprintf "sum.%i" (Unix.getpid ())) in
    Unix.mkdir t  0o700 ;
    t

  let clean () =
    let rec rm d =
      let f =
        try Some (Unix.readdir  d)
        with End_of_file -> None in
      match f with
      | None -> ()
      | Some ("."|"..") -> rm d
      | Some f ->
          Unix.unlink (Filename.concat dir f) ;
          rm d in
    let d = Unix.opendir dir in
    rm d ;
    Unix.closedir d ;
    Unix.rmdir dir
          
(* Protection *)
  let () =
    Sys.set_signal
      Sys.sigint (Sys.Signal_handle (fun _ -> clean() ; exit 1))

  let opts = "-q" :: O.opts

  let popen idx args =
    let oname = Filename.concat dir
        (sprintf "sum-%02i.txt" idx) in 
    let com = 
      let args = String.concat " " (opts@args)  in
      sprintf "%s %s >%s" prog args oname in
    if O.verbose > 0 then eprintf "Starting '%s'\n%!" com ;
    let chan = Unix.open_process_in com in
    oname,chan

    let cut args =
      let t = Array.make O.j [] in
      let rec do_rec k args =
        if k >= O.j then do_rec 0 args
        else match args with
        | [] -> ()
        | a::args ->
            t.(k) <- a :: t.(k)  ;
            do_rec (k+1) args in
      do_rec 0 args ;
      let xs = Array.to_list t in
      let xs = List.filter (function [] -> false | _::_ -> true) xs in
      Array.of_list xs

  let run args =
    let t = cut args in
    let outs = Array.mapi popen t in
    Array.iteri
      (fun _i (_oname,chan) ->
        try while true do ignore (input_char chan) done
        with End_of_file -> ignore (Unix.close_process_in chan))
      outs ;
    
    let onames =  List.map fst (Array.to_list outs) in
    zyva onames ;
    clean ()
end

let () =
  try
    if npar > 1 then
      let module M =
        Task
          (struct
            let j = npar
            let verbose = verbose
            let opts = par_opts
          end) in
      M.run fnames
    else
      zyva fnames
  with Misc.Fatal msg ->
    eprintf "Fatal error: %s\n%!" msg

