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
 | Tree of string * int list * scopes list

type levels = scopes

let pp_int_list il = String.concat " " (List.map (sprintf "%i") il)
  

let rec pp_scopes_rec s = match s with 
| Tree (s,ps,ts) -> 
    sprintf "(%s %s%s)" s
      (match ps with
        | [] -> ""
        | _ -> pp_int_list ps)
        (pp_scopes_recs ts)

and pp_scopes_recs ts = String.concat " " (List.map pp_scopes_rec ts)

let pp_scopes = function
  | Tree ("",[],ts) -> pp_scopes_recs ts
  | t -> pp_scopes_rec t

let contract_ps = function
  | Tree (sc0,[p0],[])::rem ->
      begin try
        let ps =
          List.fold_right
            (fun t ps -> match t with
            | Tree (sc1,[p1],[]) when String.compare sc0 sc1 = 0 ->
                p1::ps
            | _ -> raise Exit)
            rem [] in
        Some (p0::ps)
      with Exit -> None
      end
  | _ -> None

let rec do_contract st = match st with
  | Tree (_,_,[]) -> st
  | Tree (sc,_,ts) -> do_children sc sc ts

and do_children sc0 sc1 ts = match ts with
| [t] -> do_contract t
| _ ->
    let ts =  List.map do_contract ts in
    begin match contract_ps ts with
    | Some ps -> Tree (sc0,ps,[])
    | None -> Tree (sc1,[],ts)
    end

let contract st = match st with
| Tree (_,_,[]) -> st
| Tree ("",[],[t]) -> do_contract t
| Tree ("",[],ts) -> 
    let ts =  List.map do_contract ts in
    Tree ("",[],ts)
| Tree (sc,[],ts) -> do_children sc "" ts
| Tree (_,_::_,_) -> assert false

type test = {
  regions : mem_space_map option;
  scopes : scopes option;
  levels : scopes option ;
  }

let pp t = begin match t.scopes with
| None -> ""
| Some sc ->
    sprintf "scopes: %s\n" (pp_scopes sc)
end ^ 
  begin match t.regions with
  | None -> ""
  | Some m -> 
      sprintf "regions: %s\n" (pp_mem_map m) ;
  end
