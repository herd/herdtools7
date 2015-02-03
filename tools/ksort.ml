(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, UCL, UK                                             *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

let verbose = false

let pp_name (n,es) =
  sprintf "N=%s E=[%s]" n
    (String.concat ","
       (List.map EdgeName.dbg es))
    
let read_name line =
  try
    let n = LexName.read line in
    if verbose then begin
      eprintf "%s -> %s\n" line (pp_name n)
    end ;
    n
  with LexName.Error msg ->
    Warn.fatal "%s on '%s'" msg line
    
let rec do_rec k chan =
  let line =  try Some (input_line chan) with End_of_file -> None in
  match line with
  | None -> k
  | Some line ->
      match String.length line with
      | 0 -> do_rec k chan
      | _ ->
          if line.[0] = '#' then do_rec k chan
          else
            let k =
              try ((read_name line,line)::k)
              with Misc.Fatal msg ->
                Warn.warn_always "%s" msg ;
                k in
            do_rec k chan


let names =
  ["W+RR";
   "2+2W";"MP";"SB";"LB";"R";"S";
   "WRC";"RWC";"WWC";"WRW+2W";"WRR+2W";"WRW+WR";
   "3.2W";"3.SB";"3.LB";"ISA2";"W+RWC";
   "Z6.0";"Z6.1";"Z6.2";"Z6.3";"Z6.4";"Z6.5";
   "IRIW";"IRRWIW";"IRWIW";
   "W+RR+WR+WR";
   "W+RR+WR+WW";
   "W+RR+WW+RR";
   "W+RR+WW+RW";
   "W+RR+WW+WR";
   "W+RR+WW+WW";
   "W+RW+RR+WR";
   "W+RW+RR+WW";
   "W+RW+RW+RR";
   "W+RW+RW+RW";
   "W+RW+RW+WR";
   "W+RW+RW+WW";
   "W+RW+WR+WR";
   "W+RW+WR+WW";
   "W+RW+WW+RR";
   "W+RW+WW+RW";
   "W+RW+WW+WR";
   "W+RW+WW+WW";
   "4.2W";"4.SB";"4.LB";
   "WW+RR+WR+WR";
   "WW+RR+WW+RR";
   "WW+RR+WW+RW";
   "WW+RR+WW+WR";
   "WW+RW+RR+WR";
   "WW+RW+RW+RR";
   "WW+RW+RW+RW";
   "WW+RW+RW+WR";
   "WW+RW+WR+WR";
   "WW+RW+WW+RW";
   "WW+RW+WW+WR";
   "WW+WR+WR+WR";
   "WW+WR+WW+WR";
   "WW+WW+RR+WR";
   "WW+WW+RW+RR";
   "WW+WW+RW+RW";
   "WW+WW+RW+WR";
   "WW+WW+WR+WR";
   "WW+WW+WW+RR";
   "WW+WW+WW+RW";
   "WW+WW+WW+WR";
 ]


let t = Hashtbl.create 37

let rec fill_rec n = function
  | [] -> ()
  | x::xs ->
      Hashtbl.add t x n ;
      fill_rec (n+1) xs

let () = fill_rec 0 names

let get_idx n =
  let r =
    try Hashtbl.find t n with Not_found -> -1 in
  if verbose then eprintf "IDX %s -> %i\n" n r ;
  r

let compare_names (n1,e1) (n2,e2) =
  let x1 = get_idx n1 and x2 = get_idx n2 in
  if x1 < x2 then -1
  else if x1 > x2 then 1
  else
    let c = String.compare n1 n2 in
    match c with
    | 0 ->
        Pervasives.compare e1 e2
    | _ -> c

let compare (k1,_) (k2,_) = compare_names k1 k2

let main () =
  let xs = do_rec [] stdin in
  let xs = List.sort compare xs in
  List.iter (fun (_,s) -> printf "%s\n" s) xs


let () = main ()
