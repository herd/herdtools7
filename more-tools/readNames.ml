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
  Make
    (O:sig
      val includes : string list
      val libdir : string
      val debug : bool
    end) = struct

  open Printf

  module ML =
    MyLib.Make
      (struct
        include O
        let env = None
      end)

  module Parser = 
    ParseModel.Make
      (struct
        let debug = false
        let libfind = ML.find
      end)

  open AST 

  let rec is_fun = function
    | Bind (_,_,e)|BindRec (_,_,e) -> is_fun e 
    | Fun _ -> true
    | _ -> false

  let extract_bd names (_,pat,e) =
    match pat with
    | Pvar (Some name) when not (is_fun e) -> StringSet.add name names
    | _ -> names

  let extract_bds = List.fold_left extract_bd

  let rec extract_ins (seen,names as k) = function
  | Let (_,bds)|Rec (_,bds,_) ->
      let names = extract_bds names bds in
      seen,names
  | Include (_,name) -> extract_file seen names name
  | _ -> k

  and extract_ast seen names ast =
    List.fold_left extract_ins (seen,names) ast

  and extract_file seen names fname =
    let fname = ML.find fname in
    if StringSet.mem fname seen then (seen,names)
    else
      let mdl = Parser.parse fname in
      let _,_,ast = mdl in
      if O.debug then eprintf "Reading \"%s\"\n%!" fname ; 
      extract_ast (StringSet.add fname seen) names ast
    

  let read name =
    let _,names =
      extract_file StringSet.empty StringSet.empty name in
    names
end
    
