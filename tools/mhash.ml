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

open Printf

exception Over

module Action = struct
  type t = Check | Rewrite

  let tags = ["check"; "rewrite";]

  let parse s = match Misc.lowercase s with
  | "check" -> Some Check
  | "rewrite" -> Some Rewrite
  | _ -> None

  let pp = function
    | Check -> "check"
    | Rewrite -> "rewrite"
end

module Top
    (Opt:
       sig
         val verbose : int
         val action : Action.t
         val check_name : string -> bool
       end) =
  struct
    open Action

    module T = struct
      type t = 
        { tname : string ;
          hash : string option;
          map : string -> string; }
    end

    module Make(A:ArchBase.S) = struct

      let zyva name parsed =
	let tname = name.Name.name in
	let hash = MiscParser.get_hash parsed in
        let map = OutMapping.info_to_tr parsed.MiscParser.info in
	if Opt.verbose > 1
	then
          eprintf "%s %s\n"
	    tname
	    (match hash with 
	    | None -> "none" 
	    | Some h -> h);
        { T.tname = tname ;
          hash = hash;
          map=map; }
    end

    module Z = ToolParse.Top(T)(Make)

    type name = {fname:string; tname:string;}

    let do_test name (kh,km as k) =
      try
        let {T.tname = tname;
             hash = h; map=map;} = Z.from_file name in
        let h = Misc.as_some h in
        let old = StringMap.safe_find h tname kh in
        if old <> h then begin
          eprintf "Different hashes for test %s" tname ;
          raise Over
        end ;
        StringMap.add tname h kh,StringMap.add tname map km
      with
      | Misc.Exit -> k
      | Misc.Fatal msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 name msg ;
          k
      | e ->
          eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
          raise e

    let zyva tests logs =
      let env,map = Misc.fold_argv do_test tests (StringMap.empty,StringMap.empty) in
      let module Lex =
        LexHashLog.Make
          (struct
            let verbose = Opt.verbose

            let ppinfo = match Opt.action with
            | Check ->
                fun pos name ->
                  printf
	            "%a: hash mismatch for test %s\n" Pos.pp_pos pos name
            | Rewrite ->
                if Opt.verbose > 0 then
                  fun pos name ->
                    eprintf
	              "%a: rewrite hash for test %s\n" Pos.pp_pos pos name
                else
                  fun _ _ -> ()

            let env = env
            let map = map
            let check_name  = Opt.check_name
          end) in
      match logs with
      | [] -> 
          let action = match Opt.action with
          | Check -> Lex.check_chan
          | Rewrite -> Lex.rewrite_chan in
          action stdin
      | _::_ ->
          let action = match Opt.action with
          | Check -> Lex.check
          | Rewrite -> Lex.rewrite in
          let action =
            if Opt.verbose > 0 then
              fun fname ->
                eprintf "reading %s\n%!" fname ;
                action fname
            else action in
          List.iter action logs
  end



let verbose = ref 0
let action = ref Action.Check
let names = ref []
let excl = ref []
let tests = ref []

let arg = ref []

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mhash"

let () =
  let open CheckName in
  Arg.parse
    [
     "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
     parse_select tests;
     parse_names names;
     parse_excl excl;
     begin let module P = ParseTag.Make(Action) in     
     P.parse "-action" action "action performed" end ;
    ]
    (fun s -> arg := !arg @ [s])
    (sprintf "Usage: %s [options]* [log]*" prog)

let tests = !tests
let logs = !arg

module Check =
  CheckName.Make
    (struct
      let verbose = !verbose
      let rename = []
      let select = []
      let names = !names
      let excl = !excl
    end)

module X =
  Top
    (struct
      let verbose = !verbose
      let action = !action
      let check_name n = Check.ok n
    end)

let () = X.zyva tests logs
