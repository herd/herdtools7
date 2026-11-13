(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
module Misc = Base.Misc

open Printf

module TokenPosition = struct

  type t = First | Last

  let tags = [ "first"; "last"; ]

  let parse s =
    match String.lowercase_ascii s with
    | "first" -> Some First
    | "last" -> Some Last
    | _ -> None

  let pp = function
    | First -> "first"
    | Last -> "last"

  let quote_command tokpos com args =
    let com = Filename.quote com
    and args = List.map Filename.quote args in
    match tokpos with
    | First ->
        fun name ->
          String.concat " " (com :: Filename.quote name :: args)
    | Last ->
        let com =  String.concat " " (com::args) in
        fun name -> com ^ " " ^ Filename.quote name

end
(* Task engine *)

module type TArg = sig
  val com : string
  val comargs : string list
  val verbose : int
  val tokpos : TokenPosition.t
end

module Task(A:TArg) = struct
  let stdout_chan = stdout
  open Unix
  module W = Warn.Make(A)

  let quote_command =
    TokenPosition.quote_command A.tokpos A.com A.comargs

  let update_exit_status,get_exit_status =
    let exit_status = ref 0 in
    (fun n -> if n <> 0 && !exit_status = 0 then exit_status := n),
    (fun () -> !exit_status)

  type task =
      { idx : int ; chan : in_channel ; buff : Buffer.t; }

  let popen idx name =
    let com = quote_command name in
    if A.verbose > 2 then eprintf "Starting: '%s' on %02i\n" com idx ;
    let chan = Unix.open_process_in com in
    set_nonblock (descr_of_in_channel chan) ;
    let buff = Buffer.create 128 in
    Some { idx; buff; chan; }

  let table = Hashtbl.create 17

  let get_waiting () = Hashtbl.fold (fun fd _ r -> fd::r) table []

  let rec start_task idx (nrun,iter as k) = match iter with
  | None -> k
  | Some iter -> match Misc.next_iter iter with
    | Some (name,iter) ->
        let task = popen idx name in
        begin match task with
        | Some task ->
            let fd = descr_of_in_channel task.chan in
            Hashtbl.add table fd task ;
            if A.verbose > 1 then eprintf "Start %02i\n%!" idx ;
            nrun+1,Some iter
        | None ->  start_task idx (nrun,Some iter)
      end
    | None -> nrun,None

  let sz = 1024

  let warn_status st =
    Warn.warn_always "task ended with %s"
      (match st with
      | WEXITED i -> sprintf "exit %i" i
      | WSIGNALED i -> sprintf "signaled %i" i
      | WSTOPPED i -> sprintf "stopped %i" i)

  let to_buff fd t =
    let buff = Bytes.create sz in
    let rec to_rec () =
      try
        if A.verbose > 2 then eprintf "Read %02i\n%!" t.idx ;
        let nread = read fd buff 0 sz in
        if A.verbose > 1 then eprintf "Got %i from %02i\n%!" nread t.idx ;
        match nread with
        | 0 -> true
        | n ->
            Buffer.add_string t.buff (Bytes.sub_string buff 0 n) ;
            to_rec ()
      with
      | Unix_error ((EWOULDBLOCK|EAGAIN),_,_) -> false
      | e -> raise e in
    to_rec ()

  let process_task (nrun,files as k) fd =
    let task =
      try Hashtbl.find table fd
      with Not_found -> assert false in
    let is_over = to_buff fd task in
    if is_over then begin
      if A.verbose > 1 then eprintf "Over %02i\n%!" task.idx ;
      Hashtbl.remove table fd ;
      begin match close_process_in task.chan with
      | WEXITED n ->
          update_exit_status n ;
          Buffer.output_buffer stdout_chan task.buff ;
          flush stdout_chan
      | st ->
          warn_status st
      end ;
      start_task task.idx (nrun-1,files)
    end else begin
      if A.verbose > 2 then eprintf "Again %02i\n%!" task.idx ;
      k
    end

  let ppok ok =
     List.iter
      (fun fd ->
        let {idx=idx;_} =
          try Hashtbl.find table fd
          with Not_found -> assert false in
        eprintf " %02i" idx)
      ok ;
    eprintf "\n%!"

  let rec loop (nrun,_ as k) =
    if nrun > 0 then begin
      let fds = get_waiting () in
      assert (List.length fds = nrun) ;
      let ok,_,_ = select fds [] [] (-1.0) in
      if A.verbose > 0 then begin match ok with
      | []|[_] ->
          if A.verbose > 1 then begin
            eprintf "Select" ;
            ppok ok
          end
      | _ ->
          eprintf "Multiple select:" ;
          ppok ok
      end ;
      let k = List.fold_left process_task k ok in
      loop k
    end

  let run j names =
    let names = match names with
    | [] -> Misc.fold_stdin Misc.cons []
    | _::_ -> names in
    let names = Misc.mk_iter names in
    let rec start_rec k = function
      | 0 -> k
      | j -> start_rec (start_task j k) (j-1) in
    loop (start_rec (0,Some names) j)

end


let tokens = ref []
let com = ref "echo"
let verbose = ref 0
let j = ref 1
let tokpos = ref TokenPosition.Last
let do_exit = ref false
let comargs = ref []

let usage = String.concat "\n" [
  Printf.sprintf "Usage: %s [options] [<token> ...] [-- <command> [<argument> ...]]"
    (Filename.basename Sys.argv.(0)) ;
  "" ;
  "Apply a command to every non-option token on the command-line. If none are" ;
  "provided, tokens are read from stdin. Tokens that start with `@` are" ;
  "interpreted as filepaths, and the lines of the file are read as tokens." ;
  "If no command is given, echo is used" ;
  "" ;
  "Options:" ;
]

let parse_args () =
  Arg.parse
    (LibOpts.parse_verbose verbose
    @ ["-j", Arg.Int (fun i -> j := i),"<n> manage <n> simultaneous jobs" ;
     begin let module P = ParseTag.Make(TokenPosition) in
       P.parse "-token-position" tokpos
       "token argument position in forked command invokation"
     end ;
     "-exit",
     Arg.Bool (fun b -> do_exit := b),
     Printf.sprintf "replicate (first) non-zero exit status, default %b"
       !do_exit;
     "--", Arg.Rest_all (function
       | command :: arguments ->
           com := command ;
           comargs := arguments
       | [] -> raise (Arg.Bad "-- must be followed by a command")),
     "Separator between mapply and the command path and its arguments. "
     ^ "Any parameters on its right side will be passed as is to the command" ;
    ])
    (fun token -> tokens := token :: !tokens)
    usage

let () =
  parse_args () ;
  if !j <= 1 then begin
    let exit_status = ref 0 in
    let quote_command =
      TokenPosition.quote_command !tokpos !com !comargs in
    let do_test name =
      let com = quote_command name in
      let status = Sys.command com in
      if status <> 0 && !exit_status = 0 then exit_status := status in
    Misc.iter_argv_or_stdin do_test !tokens ;
    if !do_exit then exit !exit_status
  end else begin
    let module T =
      Task
        (struct
          let com = !com
          let comargs = !comargs
          let verbose = !verbose
          let tokpos = !tokpos
        end) in
    T.run !j !tokens ;
    if !do_exit then T.get_exit_status () |> exit
  end
