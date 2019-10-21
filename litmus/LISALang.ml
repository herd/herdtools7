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

  let checkVal f v = f v

  module RegSet = A.Out.RegSet
  module RegMap = A.Out.RegMap

  let debug = false

  let do_dump compile_val _compile_addr compile_out_reg
      chan indent env proc t =
    let rec dump_ins k ts = match ts with
    | [] -> ()
    | t::ts ->
        fprintf chan "%s%s\n" indent (Tmpl.to_string t) ;
(*
  fprintf chan "\"%-20s%c_litmus_P%i_%i\\n\\t\"\n"
  (to_string t) A.comment proc k ;
 *)
        dump_ins (k+1) ts in
(* Prefix *)
    let reg_env = Tmpl.get_reg_env A.I.error t in
    let all_regs = Tmpl.all_regs t in
    let init =
      List.fold_left (fun m (r,v) -> RegMap.add r v m)
        RegMap.empty
        t.Tmpl.init in
    RegSet.iter
      (fun r ->
        let ty =
          try RegMap.find r env
          with  Not_found ->
            try RegMap.find r reg_env with
            | Not_found -> Compile.base  in
        if debug then
          eprintf "%i:%s -> %s\n" proc (Tmpl.tag_reg r) (CType.dump ty) ;
        fprintf chan "%s%s %s%s;\n" indent
          (CType.dump ty)
          (Tmpl.tag_reg r)
          (try
            let v = RegMap.find r init in
            sprintf " = %s" (compile_val v)
          with Not_found -> ""))
      all_regs ;
(* Code *)
    begin match t.Tmpl.code with
    | [] -> ()
    | code -> dump_ins 0 code
    end ;
(* Postfix *)
    fprintf chan "%sbarrier();\n" indent ;
    List.iter
      (fun reg ->
         fprintf chan "%s%s = %s;\n" indent
          (compile_out_reg proc reg) (Tmpl.tag_reg reg))
      t.Tmpl.final ;
    ()

(*****************)
(* As a function *)
(*****************)

  let compile_val_fun v =
    let open Constant in
    match v with
    | Symbolic ((s,None),0) -> sprintf "%s" s
    | Concrete _ -> Tmpl.dump_v v
    | Label _ -> Warn.user_error "No label value in LISA"
    | Symbolic _|Tag _ ->
        Warn.user_error "No tag nor indexed accesses in LISA"

  and compile_addr_fun x = sprintf "*%s" x

  and compile_out_reg_fun p r = sprintf "*%s" (Tmpl.dump_out_reg p r)

  let dump_fun chan env globEnv _volatileEnv proc t =
    let addrs_proc = A.Out.get_addrs t in
    let addrs =
      List.map
        (fun x ->
          let ty =
            try List.assoc x globEnv
            with Not_found -> Compile.base in
          let ty = SkelUtil.dump_global_type x ty in
          sprintf "%s *%s" ty x)
        addrs_proc in
    let outs =
      List.map
        (fun x ->
          let ty =
            try RegMap.find x env
            with Not_found -> assert false in
          let x = Tmpl.dump_out_reg proc x in
          sprintf "%s *%s" (CType.dump ty) x) t.Tmpl.final in

    let params =
      let p = addrs@outs in
      match p with
      | [] -> "void"
      | _::_ -> String.concat "," p in
    LangUtils.dump_code_def chan false proc params ;
    do_dump
      (checkVal compile_val_fun)
      compile_addr_fun
      (fun p r  -> sprintf "*%s" (Tmpl.dump_out_reg p r))
      chan "  "  env proc t ;
    fprintf chan "}\n\n" ;
    ()


  let compile_addr_call x = sprintf "&_a->%s[_i]" x
  let compile_out_reg_call proc reg =
    sprintf "&_a->%s" (Tmpl.compile_out_reg proc reg)

  let dump_call f_id _tr_idx chan indent _env _globEnv _volatileEnv proc t =
    let addrs_proc = Tmpl.get_addrs t in
    let addrs = List.map compile_addr_call addrs_proc
    and outs = List.map (compile_out_reg_call proc) t.Tmpl.final in
    let args = String.concat "," (addrs@outs) in
    LangUtils.dump_code_call chan indent f_id args


  let dump _chan _indent _env _globEnv _volatileEnv _proc _t = ()

end
