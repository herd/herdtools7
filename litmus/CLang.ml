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

module type Config = sig
  val memory : Memory.t
  val mode : Mode.t
  val comment : string
  val asmcommentaslabel : bool
end

module DefaultConfig = struct
  let memory = Memory.Direct
  let mode = Mode.Std
  let comment = "//"
  let asmcommentaslabel = false
end

module type Extra = sig
  val verbose : int
  val noinline : bool
  val simple : bool
  val out_ctx : string -> string
end

module Make(C:Config)(E:Extra) = struct
  open Printf
  module W = Warn.Make(E)

  type arch_reg = string
  module RegMap = StringMap

  type t = CTarget.t

  let dump_start chan indent proc =
    if not E.simple then begin
      if C.asmcommentaslabel then
        fprintf chan
          "%sasm __volatile__ (\"\\n%s:\" ::: \"memory\");\n"
          indent (LangUtils.start_label proc)
      else
        fprintf chan
          "%sasm __volatile__ (\"\\n%s\" ::: \"memory\");\n"
          indent (LangUtils.start_comment C.comment proc)
    end

  let dump_end chan indent proc =
    if not E.simple then begin
      if C.asmcommentaslabel then
        fprintf chan
          "%sasm __volatile__ (\"\\n%s:\" ::: \"memory\");\n"
          indent (LangUtils.end_label proc)
      else
        fprintf chan
          "%sasm __volatile__ (\"\\n%s\" ::: \"memory\");\n"
          indent (LangUtils.end_comment C.comment proc)
    end

  let dump_global_def _env (x,ty) =
    let x = CTarget.fmt_reg x in
    let pp_ty =  CType.dump ty in
    pp_ty ^ "*",x

  let out_type env x =
    try RegMap.find x env
    with Not_found -> assert false

  let dump_output_def env proc x =
    let outname = CTarget.dump_out_reg proc x
    and ty = out_type env x in
    sprintf "%s*" (CType.dump ty),outname

  let dump_fun chan env globEnv _envVolatile proc t =
(*
  let pp_env =
  String.concat "; "
  (List.map
  (fun (x,ty) -> sprintf "%s -> %s" x (CType.debug ty))
  globEnv) in
  eprintf "FUN: [%s]\n%!" pp_env ;
 *)
    let out fmt = fprintf chan fmt in
    let input_defs =
      List.map (dump_global_def globEnv) t.CTarget.inputs
    and output_defs =
      List.map (dump_output_def env proc) t.CTarget.finals in
    let defs = input_defs@output_defs in
    let params =
      String.concat ","
        (List.map
           (fun (ty,v) -> sprintf "%s %s" ty v)
           defs) in
    (* Function prototype  *)
    LangUtils.dump_code_def chan E.noinline proc params ;
    (* body *)
    dump_start chan "  " proc ;
    CTarget.out_code chan t.CTarget.code ;
    dump_end chan "  " proc ;
    (* output parameters *)
    List.iter
      (fun reg ->
        out "  *%s = (%s)%s;\n"
          (CTarget.dump_out_reg proc reg)
          (CType.dump (out_type env reg))
          (CTarget.fmt_reg reg))
      t.CTarget.finals ;
    out "}\n\n"


  let dump_call f_id tr_idx chan indent _env globEnv _envVolatile proc t =
    let is_array_of a =
      try  match List.assoc a globEnv with
      | CType.Array (t,_) -> Some t
      | _ -> None
      with Not_found -> None in
    let global_args =
      List.map
        (fun (x,t) ->
          let idx = tr_idx t "_i" in
          let cast = match is_array_of x with
          | Some t -> sprintf "(%s *)" t
          | None -> "" in
          let amper = match List.assoc x globEnv with
          | CType.Base "mtx_t" -> ""
          | _ -> "&" in
          match C.memory with
         | Memory.Direct ->
             sprintf "%s%s_a->%s[%s]" cast amper (CTarget.fmt_reg x) idx
         | Memory.Indirect ->
             sprintf "%s_a->%s[%s]" cast (CTarget.fmt_reg x) idx)
        t.CTarget.inputs
          and out_args =
            List.map
              (fun x -> sprintf "&%s" (E.out_ctx (CTarget.compile_out_reg proc x)))
              t.CTarget.finals in
          let args = String.concat "," (global_args@out_args) in
          LangUtils.dump_code_call chan indent f_id args


  let dump chan indent env globEnv _envVolatile proc t =
    let out x = fprintf chan x in
    out "%sdo {\n" indent;
    begin
      let indent = "  " ^ indent in
      let dump_input x =
        let ty,x = dump_global_def globEnv x in
        let amper = match List.assoc x globEnv with
        | CType.Base "mtx_t" -> ""
        | _ -> "&" in
        match C.memory with
        | Memory.Direct -> begin match C.mode with
          | Mode.Std ->
              out "%s%s %s = (%s)%s_a->%s[_i];\n" indent ty x ty amper x
          |  Mode.PreSi|Mode.Kvm -> ()
        end
        | Memory.Indirect ->
            out "%s%s %s = (%s)_a->%s[_i];\n" indent ty x ty x
      in
      let dump_output x =
        let ty = out_type env x in
        match C.mode with
        | Mode.Std ->
            let outname = CTarget.compile_out_reg proc x in
            out "%s%s = (%s)%s;\n"
              indent outname (CType.dump ty) (CTarget.fmt_reg x)
        | Mode.PreSi|Mode.Kvm ->
            let outname =
              if CType.is_ptr ty then
                CTarget.compile_presi_out_ptr_reg proc x
              else
                CTarget.compile_presi_out_reg proc x in
            out "%s%s = (%s)%s;\n"
              indent outname (CType.dump ty) (CTarget.fmt_reg x)

      in
      let print_start = dump_start chan in
      let print_end = dump_end chan in
      List.iter dump_input t.CTarget.inputs;
      print_start indent proc;
      CTarget.out_code chan t.CTarget.code ;
      print_end indent proc;
      List.iter dump_output t.CTarget.finals
    end;
    out "%s} while(0);\n" indent

end
