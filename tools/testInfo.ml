(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2016-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Extract information from test, at the moment name + fname + hash *)

module T = struct
  type t = 
      { tname : string ;
        fname : string ;
        hash : string ; } 

      let cmp_pair cmp1 cmp2 t1 t2 = match cmp1 t1 t2 with
      | 0 -> cmp2 t1 t2
      | r -> r

      let compare t1 t2 =
        cmp_pair
          (fun t1 t2 -> String.compare t1.tname t2.tname)
          (cmp_pair
             (fun t1 t2 ->  String.compare t1.hash t2.hash)
             (fun t1 t2 ->  String.compare t1.fname t2.fname))
          t1 t2
end

module Make(A:ArchBase.S) = struct

  let zyva name parsed =
    let tname = name.Name.name in
    let fname =  name.Name.file in
    let hash = MiscParser.get_hash parsed in
    let hash =
      match hash with 
      | None -> assert false
      | Some h -> h in
    { T.tname = tname ; fname=fname; hash = hash; }
end

module Z = ToolParse.Top(T)(Make)
