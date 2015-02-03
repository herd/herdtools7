(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*     Jacques-Pascal Deplaix, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module type Config = sig
  val comment : char
  val memory : Memory.t
  val mode : Mode.t
end

module Make(C:Config) = struct
  open Printf

  type arch_reg = CTarget.arch_reg
  type t = CTarget.t

  let dump_start chan indent proc =
    fprintf chan
      "%sasm __volatile__ (\"\\n%s\" ::: \"memory\");\n"
      indent (LangUtils.start_comment C.comment proc)

  let dump_end chan indent proc =
    fprintf chan
      "%sasm __volatile__ (\"\\n%s\" ::: \"memory\");\n"
      indent (LangUtils.end_comment C.comment proc)

  let dump_global_def env (x,ty) =
(*
    let oty =
      try List.assoc x env with Not_found -> assert false in
*)
    let x = CTarget.fmt_reg x in
    let pp_ty =  CType.dump ty in
    pp_ty ^ "*",x

  let out_type env x =
    try List.assoc x env
    with Not_found -> assert false 

  let dump_output_def env proc x =
    let outname = CTarget.dump_out_reg proc x
    and ty = out_type env x in
    sprintf "%s*" (CType.dump ty),outname

  let dump_fun chan env globEnv envVolatile proc t =
    let out x = fprintf chan x in
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
    LangUtils.dump_code_def chan proc params ;
    (* body *)
    dump_start chan "  " proc ;
    out "%s\n" t.CTarget.code ;
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


  let dump_call chan indent env globEnv envVolatile proc t =
    let is_array_of a = match List.assoc a globEnv with
    | CType.Array (t,_) -> Some t
    | _ -> None in
    let global_args =
      List.map
        (fun (x,_) ->
          let cast = match is_array_of x with
          | Some t -> sprintf "(%s *)" t
          | None -> "" in
          match C.memory with
        | Memory.Direct ->
            sprintf "%s&_a->%s[_i]" cast x
        | Memory.Indirect ->
            sprintf "%s_a->%s[_i]" cast x)
        t.CTarget.inputs
    and out_args =
      List.map
        (fun x -> sprintf "&%s" (CTarget.compile_out_reg proc x))
        t.CTarget.finals in
    let args = String.concat "," (global_args@out_args) in
    LangUtils.dump_code_call chan indent proc args


  let dump chan indent env globEnv envVolatile proc t =
    let out x = fprintf chan x in
    out "%sdo {\n" indent;
    begin
      let indent = "  " ^ indent in
      let dump_input x =
        let ty,x = dump_global_def globEnv x in
        match C.memory with
        | Memory.Direct -> begin match C.mode with
          | Mode.Std ->
              out "%s%s %s = (%s)&_a->%s[_i];\n" indent ty x ty x
          |  Mode.PreSi -> ()
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
        | Mode.PreSi ->
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
      out "%s\n" t.CTarget.code ;
      print_end indent proc;
      List.iter dump_output t.CTarget.finals
    end;
    out "%s} while(0);\n" indent

end
