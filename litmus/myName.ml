(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(*************************************)
(* Extended Filename like facilities *)
(*************************************)


let chop_litmus source =
  if Filename.check_suffix source ".litmus" then
    Filename.chop_extension source
  else
    source

let outname source ext =
  let base = Filename.basename (chop_litmus source) in
  base ^ ext


(* Open litmus own files *)

let try_open dir name =
  let rname = Filename.concat dir name in
  try rname,open_in rname
  with _ -> raise Exit

let envlib = try Some (Sys.getenv "LITMUSDIR") with Not_found -> None

let open_lib name =
  try try_open "." name
  with Exit -> try match envlib with
  | Some lib -> try_open lib name
  | None -> raise Exit
  with Exit -> try try_open Config.libdir name
  with Exit -> Warn.fatal "Cannot find file %s" name
  

let name_lib name =
  let r,chan = open_lib name in
  begin try close_in chan with _ -> () end ;
  r

    
let read_cfg name =
  try
  let name =
    if Filename.check_suffix name ".cfg" then name
    else name ^ ".cfg" in
  if Filename.is_implicit name then
    let _rname,chan = open_lib name in
    LexConf.lex chan ;
    close_in chan
  else
    Misc.input_protect LexConf.lex name
  with
  | Arg.Bad msg
  | LexConf.Error msg ->
     Warn.fatal "Bad configuration file: %s" msg


(* Legal C symbol *)
let start_with_letter s =
  String.length s > 0 &&
  begin match s.[0] with
  | 'a'..'z'|'A'..'Z' -> true
  | _ -> false
  end

let tr_symbol name =
  let b = Buffer.create 16 in
  let len = String.length name in
  let rec do_rec i =
    if i >= len then Buffer.contents b
    else begin
      let c = name.[i] in
      begin match c with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' -> Buffer.add_char b c
      | _ ->
          Buffer.add_string b
            (Printf.sprintf "_%02X_" (Char.code c))
      end ;
      do_rec (i+1)
    end in
  do_rec 0

let as_symbol t =
  let name = t.Name.name in
  let sym = tr_symbol name in
  if start_with_letter name then sym
  else "X" ^ sym
