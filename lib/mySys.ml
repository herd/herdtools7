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

let save_temps = ref false
let debug = ref false

(********************************)
(* Extended Sys like facilities *)
(********************************)


(********************)
(* Output for lines *)
(********************)

let output_line chan line =
  output_string chan line ;
  output_char chan '\n'

let output_lines chan = List.iter (output_line chan)
let print_lines = output_lines stdout


(*******************)
(* Input for lines *)
(*******************)

(* read a file line by line *)
let rec read_by_line chan kont res =
  let line =
    try Some (input_line chan)
    with End_of_file -> None in
  match line with
  | None -> res
  | Some line -> read_by_line chan kont (kont line res)

(* Idem accumulating results in a list *)
let read_list chan f =
  let r =
    read_by_line chan
      (fun line res -> match f line with
      | Some r -> r::res
      | None -> res)
      [] in
  List.rev r

(**********)
(* Remove *)
(**********)

let do_remove name = try Sys.remove name with _ -> ()

let remove name =
  if not !save_temps then do_remove name

(********)
(* Move *)
(********)

(* handle windows/Unix dialectic => no error when s2 exists *)
let move s1 s2 =
  if Sys.os_type <> "Unix" && Sys.file_exists s2 then do_remove s2 ;
  Sys.rename s1 s2
  
(********)
(* grep *)
(********)

(* is s1 [starting at k] a substring of s2 [starting at j+k] *)
let is_prefix k s1 j s2 =
  let rec do_rec k =
    if k < String.length s1 then
      let c = s1.[k] in
      if k+j < String.length s2 then
        let d = s2.[k+j] in
        if c = d then
          do_rec (k+1)
        else
          false
      else
        false
    else
      true in
  do_rec k

(* Is pat a substring of s ? *)
let is_substring pat s =
  if String.length pat = 0 then true
  else
    let len = String.length s in
    let rec do_rec k =
      if k < len then
        try
          let c = pat.[0] in
          let j = String.index_from s k c in
          if is_prefix 1 pat j s then true
          else do_rec (k+1)
        with Not_found -> false
      else
        false in
    do_rec 0

    
let do_grep out_chan pat in_chan =
  read_by_line in_chan
    (fun line () ->
      if is_substring pat line then
        output_line out_chan line)
    ()

let grep chan pat name = Misc.input_protect (do_grep chan pat) name


(*******************)
(* Cat and friends *)
(*******************)

let cat_chan chan kont = read_by_line chan (fun line () -> kont line) ()

let cat name kont =
  Misc.input_protect (fun chan -> cat_chan chan kont)  name

let cat_and_remove name kont =
  let r = cat name kont in
  remove name ;
  r

let cp ?prf ichan name =
  Misc.output_protect
    (fun chan ->
      begin match prf with
      | None -> ()
      | Some line ->
          output_line chan line
      end ;
      cat_chan ichan
        (fun line -> output_line chan line))
    name

(*************************)
(* Execute unix commands *)
(*************************)

open Printf

let command cmd =
  if !debug then begin
    eprintf "Exec: %s -> " cmd ;
    flush stderr
  end ;
  let r = Sys.command cmd in
  if !debug then begin
    eprintf "%i\n" r ;
    flush stderr
  end ;
  r
  
    
let exec_stdout cmd =
  match command cmd with
  | 0 -> ()
  | _ -> Warn.fatal "Exec of '%s' failed" cmd

(* Could do this with Unix.open_process, but well *)
let exec cmd kont =
  let f = Filename.temp_file "plumbing_" ".tmp" in
  let _cmd = cmd ^">"^f in
  match command _cmd with
  | 0 -> cat_and_remove f kont
  | _ -> Warn.fatal "Exec of '%s' failed" cmd      


(************************************)
(* Use above for making a directory *)
(************************************)

let mkdir name = exec_stdout (sprintf "/bin/rm -rf %s && mkdir %s" name name)

let mktmpdir () =
  let name = Filename.temp_file  "dir" ".tmp" in
  mkdir name ;
  name

let rmdir name = exec_stdout (sprintf "/bin/rm -rf %s" name)
