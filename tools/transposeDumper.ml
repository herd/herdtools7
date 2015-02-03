(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, UCL, UK.                                            *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(*****************)
(* Parsable dump *)
(*****************)
module type I = sig
  module A : ArchBase.S
  type prog =  (int * A.pseudo list) list

  type state
  val dump_global_state : prog -> state -> string
  val dump_proc_state : int -> state -> string

  type constr
  val dump_constr : constr -> string

  type location
  val dump_location : location -> string

end

module Make(I:I) : sig
  type prog =  (int * I.A.pseudo list) list
  val dump : out_channel ->
    Name.t ->
      (I.state, prog, I.constr, I.location)  MiscParser.result
      -> unit
  val dump_info : out_channel ->
    Name.t ->
    (I.state, prog, I.constr, I.location)
        MiscParser.result
      -> unit
end = struct
  open Printf
  open I
  type prog =  (int * I.A.pseudo list) list
  let rec fmt_io io = match io with
  | A.Nop -> ""
  | A.Instruction ins -> A.dump_instruction ins
  | A.Label (lbl,io) -> lbl ^ ": " ^ fmt_io io
  | A.Macro (f,regs) ->
      sprintf
        "%s(%s)"
        f
        (String.concat "," (List.map A.pp_reg regs))

  let rec clean_code = function
    | [] -> []
    | A.Nop::rem -> clean_code rem
    | i::rem -> i::clean_code rem

  open MiscParser

  let dump_sep chan tag = fprintf chan "-------------------- %s\n" tag

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
      (fun (p,code) ->
        dump_sep chan (sprintf "Process %i" p) ;
        fprintf chan "%s\n" (dump_proc_state p t.init) ;
        fprintf chan "***\n" ;
        let code = clean_code code in
        List.iter (fun i -> fprintf chan "%s\n" (fmt_io i)) code ;
        ())
      prog ;
(* Conditions *)
    dump_sep chan "Check" ;
    begin match t.locations with
    | [] -> ()
    | locs ->
        fprintf chan "locations [" ;
        List.iter
          (fun (loc,t) -> match t with
          | MiscParser.TyDef  ->
              fprintf chan "%s; " (I.dump_location loc)
          | MiscParser.TyDefPointer ->
              fprintf chan "%s*; "(I.dump_location loc)
          | MiscParser.Ty t ->
              fprintf chan "%s %s; " (I.dump_location loc) t
          | MiscParser.Pointer t ->
              fprintf chan "%s %s*; " (I.dump_location loc) t)
          locs ;
        fprintf chan "]\n"
    end ;
    fprintf chan "%s\n" (I.dump_constr t.condition) ;
    ()

  let dump = do_dump false
  let dump_info = do_dump true

end
