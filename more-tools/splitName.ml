(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module
  Make (O:sig val debug : bool end) =
struct

  open Printf

  module CharMap = Map.Make(Char)

  type t = Node of (bool * t) CharMap.t

  let empty = Node CharMap.empty

  let insert key =
    let len = String.length key in
    let rec do_rec k m =
      match m with
      | Node t ->
          if k < len then
            let c = key.[k] in
            let b,u =
              try CharMap.find c t
              with Not_found -> false,empty in
            let bu =
              if k+1 = len then
                true,u
              else
                let u = do_rec (k+1) u in
                b,u in
            Node (CharMap.add c bu t)
          else m in
    do_rec 0

  let build dict =
    List.fold_left (fun k (s,_) ->  insert s k) empty dict

  let trie =
    List.filter
      (function
        | "reg",_ -> false
        | _ -> true)
      Dict.sets |> build

  let rec prefixes (Node t) =
    CharMap.fold
      (fun c (b,u) k ->
         if b then
           let sfs = suffixes u in
           match sfs with
           | [] -> k
           | _ -> ([c],sfs)::k
         else
           let pfs = prefixes u in
           match pfs with
           | [] -> k
           | _ ->
               List.fold_right
                 (fun (cs,sfs) k -> (c::cs,sfs)::k)
                 pfs k)
      t []

  and suffixes (Node t) =
    CharMap.fold
      (fun c (b,u) k ->
         let sfs =
           List.fold_right (fun sf k -> (c::sf)::k) (suffixes u) k in
         if b then [c]::sfs else sfs)
      t []

  let pp_chars cs =
    let buff = Buffer.create 8 in
    List.iter (Buffer.add_char buff) cs ;
    Buffer.contents buff

  let pfs = prefixes trie

  let add_c c = function
    | [] -> []
    | cs::ds -> (c::cs)::ds

  let rec split_suffix pf (Node t) =
    match pf with
    | [] -> []
    | c::cs ->
        begin
          try
            let b,u = CharMap.find c t in
            if b then
              match cs with
              | [] -> [[c]]
              | _ ->
                  begin
                    match split_suffix cs trie with
                    | [] ->  split_suffix cs u |> add_c c
                    | sfs -> [c]::sfs
                  end
            else split_suffix cs u |> add_c c
          with Not_found -> []
        end

  let split_suffix sf = split_suffix sf trie

  let pfs =
    List.fold_right
      (fun (pf,sfs) ->
         List.fold_right
           (fun sf k ->
              let key = pp_chars (pf@sf) in
              match split_suffix sf with
              | [] -> k
              | sfs -> (key,List.map pp_chars (pf::sfs))::k)
           sfs)
      pfs []

  let () =
    if O.debug then begin
      match pfs with
      | [] -> ()
      | _::_ ->
          prerr_endline "Some names are splitted:" ;
          List.iter
            (fun (pf,sfs) ->
               Printf.eprintf "%s -> {%s}\n"
                 pf
                 (String.concat "," sfs))
            pfs
    end

  let map =
    List.fold_left
      (fun m (key,ss) -> StringMap.add key ss m)
      StringMap.empty
      pfs

  let check s =
    try StringMap.find s map
    with Not_found -> [s]
end
