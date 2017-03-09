(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


module Make(V:Constant.S) = struct
  open Printf

  module A = LISAArch_litmus.Make(V)

  type arch_reg = A.reg
  type t = A.Out.t

  module Tmpl = A.Out

  let dump_fun chan env globEnv volatileEnv proc t =
    let addrs_proc = A.Out.get_addrs t in
    let addrs =
      List.map
        (fun x ->
          let ty =
            try List.assoc x globEnv
            with Not_found -> assert false in
          let ty = SkelUtil.dump_global_type x ty in
          sprintf "%s *%s" ty x)
        addrs_proc in
    let outs =
      List.map
        (fun x ->
          let ty =
            try List.assoc x env
            with Not_found -> assert false in
          let x = Tmpl.dump_out_reg proc x in
          sprintf "%s *%s" (CType.dump ty) x) t.Tmpl.final in
    let params =  String.concat "," (addrs@outs) in
    LangUtils.dump_code_def chan false proc params ;
    fprintf chan "}\n\n" ;
    ()
  let compile_addr_call x = sprintf "&_a->%s[_i]" x
  let compile_out_reg_call proc reg =
    sprintf "&_a->%s" (Tmpl.compile_out_reg proc reg)

  let dump_call chan indent env globEnv volatileEnv proc t =
    let addrs_proc = Tmpl.get_addrs t in
    let addrs = List.map compile_addr_call addrs_proc
    and outs = List.map (compile_out_reg_call proc) t.Tmpl.final in
    let args = String.concat "," (addrs@outs) in
    LangUtils.dump_code_call chan indent proc args
      
      
  let dump chan indent env globEnv volatileEnv proc t = ()

end
