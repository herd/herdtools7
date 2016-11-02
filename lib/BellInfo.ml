(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

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
  

let rec pp_scopes_rec s = match s with 
  | Leaf(s,i) -> sprintf "(%s %s)" s (pp_int_list i)
  | Children(s,ts) -> 
    sprintf "(%s %s)" s (pp_scopes_recs ts)

and pp_scopes_recs ts =
  String.concat " " (List.map pp_scopes_rec ts)

let pp_scopes = function
  | Children ("",ts) -> pp_scopes_recs ts
  | t -> pp_scopes_rec t

let contract_ps = function
  | Leaf (sc0,[p0])::rem ->
      begin try
        let ps =
          List.fold_right
            (fun t ps -> match t with
            | Leaf (sc1,[p1]) when String.compare sc0 sc1 = 0 ->
                p1::ps
            | _ -> raise Exit)
            rem [] in
        Some (p0::ps)
      with Exit -> None
      end
  | _ -> None

let rec do_contract st = match st with
  | Leaf _ -> st
  | Children (sc,ts) -> do_children sc sc ts

and do_children sc0 sc1 ts = match ts with
| [t] -> do_contract t
| _ ->
    let ts =  List.map do_contract ts in
    begin match contract_ps ts with
    | Some ps -> Leaf (sc0,ps)
    | None -> Children (sc1,ts)
    end

let contract st = match st with
| Leaf _ -> st
| Children ("",[t]) -> do_contract t
| Children ("",ts) -> 
    let ts =  List.map do_contract ts in
    Children ("",ts)
| Children (sc,ts) -> do_children sc "" ts

type test = {
  regions : mem_space_map option;
  scopes : scopes option;
}

let pp t =
  begin match t.scopes with
  | None -> ""
  | Some sc ->
      sprintf "scopes: %s\n" (pp_scopes sc)
  end ^ 
  begin match t.regions with
  | None -> ""
  | Some m -> 
      sprintf "regions: %s\n" (pp_mem_map m) ;
  end
