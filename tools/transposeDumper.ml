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

(*****************)
(* Parsable dump *)
(*****************)
module type I = sig
  module A : ArchBase.S
  type prog =  (MiscParser.proc * A.pseudo list) list

  type v
  val dump_v : v -> string

  type state
  val dump_global_state : prog -> state -> string
  val dump_proc_state : int -> A.pseudo list -> state -> string option

  type prop
  val dump_prop : prop -> string
  val dump_constr : prop ConstrGen.constr -> string

  type location
  val dump_location : location -> string

  type fault_type
  val dump_fault_type : fault_type -> string
end

module Make(I:I) : sig
  type prog =  (MiscParser.proc * I.A.pseudo list) list
  val dump : out_channel ->
    Name.t ->
      (I.state, prog, I.prop, I.location, I.v, I.fault_type)  MiscParser.result
      -> unit
  val dump_info : out_channel ->
    Name.t ->
      (I.state, prog, I.prop, I.location, I.v, I.fault_type) MiscParser.result
      -> unit
end = struct
  open Printf
  open I
  type prog =  (MiscParser.proc * I.A.pseudo list) list
  let rec fmt_io io = match io with
  | A.Nop -> ""
  | A.Instruction ins -> A.dump_instruction ins
  | A.Label (lbl,io) -> lbl ^ ": " ^ fmt_io io
  | A.Symbolic s -> sprintf "codevar:%s" s
  | A.Macro (f,regs) ->
      sprintf
        "%s(%s)"
        f
        (String.concat "," (List.map A.pp_reg regs))
  | A.Align n -> sprintf ".p2align %d" n

  let rec clean_code = function
    | [] -> []
    | A.Nop::rem -> clean_code rem
    | i::rem -> i::clean_code rem

  open MiscParser

  let dump_sep chan tag = fprintf chan "\n----- %s\n" tag

  let dump_rloc = ConstrGen.dump_rloc I.dump_location

  let do_dump withinfo chan doc t =
    fprintf chan "%s %s\n" (Archs.pp A.arch) doc.Name.name ;
    begin match doc.Name.doc with
    | "" -> ()
    | doc -> fprintf chan "\"%s\"\n" doc
    end ;
    if withinfo then begin
        dump_sep chan "Info" ;
        List.iter
          (fun (k,i) -> fprintf chan "%s=%s\n" k i)
          t.info
      end ;
    (* Start *)
    dump_sep chan "Start" ;
    fprintf chan "%s\n" (dump_global_state  t.prog t.init) ;
    (* Procs *)
    let prog = t.prog in
    List.iter
      (fun ((p,_,_) as proc,code) ->
        dump_sep chan (MiscParser.pp_proc proc) ;
        begin match dump_proc_state p code t.init with
        | Some st ->
            fprintf chan "%s\n" st ;
            fprintf chan "***\n"
        | None -> ()
        end ;
        let code = clean_code code in
        List.iter (fun i -> fprintf chan "%s\n" (fmt_io i)) code ;
        ())
      prog ;
    let extra = t.extra_data in
    begin
      List.iter
        (function
         | CExtra _ -> ()
         | BellExtra bi ->
           dump_sep chan "Scope" ;
           fprintf chan "%s" (BellInfo.pp bi))
        extra;
      ()
    end ;
    (* Conditions *)
    dump_sep chan "Check" ;
    begin match t.locations with
    | [] -> ()
    | locs ->
        fprintf chan "locations [" ;
        let open LocationsItem in
        List.iter
          (function
           | Loc (loc,t) ->
               begin match t with
               | TestType.TyDef  ->
                   fprintf chan "%s; " (dump_rloc loc)
               | TestType.TyDefPointer ->
                   fprintf chan "%s*; "(dump_rloc loc)
               | TestType.Ty t ->
                   fprintf chan "%s %s; " (dump_rloc loc) t
               | TestType.Pointer t ->
                   fprintf chan "%s %s*; " (dump_rloc loc) t
               |  TestType.TyArray _|TestType.Atomic _ -> assert false
               end
           | Fault f -> fprintf chan "%s; " (Fault.pp_fatom I.dump_v I.dump_fault_type f))
          locs ;
        fprintf chan "]\n"
    end ;
    fprintf chan "%s\n" (I.dump_constr t.condition) ;
    ()

  let dump = do_dump false
  let dump_info = do_dump true

end
