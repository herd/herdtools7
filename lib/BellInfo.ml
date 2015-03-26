(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(* Tyler Sorensen, University College London                         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

open Printf

let _dbg = false

(* To disable checks that I do not understand yet -> false *)
let _docheck = true


(*************)
(* For tests *)
(*************)

type mem_space_map = (string * string) list

let pp_mem_map m = 
  String.concat ","
    (List.map (fun (n,r) -> sprintf "%s: %s" n r) m)

type scopes = 
 | Leaf of string* int list
 | Children of string * scopes list

let pp_int_list il =
  String.concat " " (List.map (sprintf "%i") il)
  

let rec pp_scopes s =
  match s with 
  | Leaf(s,i) -> sprintf "(%s %s)" s (pp_int_list i)
  | Children(s,sc) -> 
    let pp = List.map (fun x -> pp_scopes x) sc in
    sprintf "(%s %s)" s (String.concat " " pp)

type test = {
  regions : mem_space_map option;
  scopes : scopes option;
}

let pp chan t =
  begin match t.scopes with
  | None -> ()
  | Some sc ->
      fprintf chan "scopes: %s\n" (pp_scopes sc)
  end ;
  begin match t.regions with
  | None -> ()
  | Some m -> 
      fprintf chan "regions: %s\n" (pp_mem_map m) ;
  end
