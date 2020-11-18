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
(** Parsable dump *)

module type I = sig
  module A : ArchBase.S

  type v
  val dump_v : v -> string

  type state
  val dump_state : state -> string

  type prop
  val dump_prop : prop -> string
  val dump_constr : prop ConstrGen.constr -> string

  type location
  val dump_location : location -> string
end

module type Out = sig
  type t
  val fprintf : t -> ('a, out_channel, unit) format -> 'a
end

module OutChannel = struct
  type t = out_channel
  let fprintf chan fmt = Printf.fprintf chan fmt
end

module Make(Out:Out)(I:I) : sig
  val dump : Out.t ->
    Name.t ->
    (I.state, (MiscParser.proc * I.A.pseudo list) list, I.prop, I.location,I.v)
        MiscParser.result
      -> unit
  val dump_info : Out.t ->
    Name.t ->
    (I.state, (MiscParser.proc * I.A.pseudo list) list, I.prop, I.location,I.v)
        MiscParser.result
      -> unit
  val lines :
      Name.t ->
        (I.state, (MiscParser.proc * I.A.pseudo list) list, I.prop, I.location,I.v)
          MiscParser.result
      -> string list
end = struct
  open Printf
  open I

  let rec fmt_io io = match io with
  | A.Nop -> ""
  | A.Instruction ins -> A.dump_instruction ins
  | A.Label (lbl,io) -> lbl ^ ": " ^ fmt_io io
  | A.Symbolic s -> "codevar:"^s
  | A.Macro (f,regs) ->
      sprintf
        "%s(%s)"
        f
        (String.concat "," (List.map A.pp_reg regs))

  let fmt_col (p,is) = MiscParser.pp_proc p::List.map fmt_io is

  let prog chan prog =
    let pp = List.map fmt_col prog in
    Out.fprintf chan "%s" (Misc.string_of_prog pp)
(*
    dump_procs chan prog ;
    iter_prog (dump_ios chan)
      (List.map snd prog)
*)
  open MiscParser


  let do_dump withinfo chan doc t =
    Out.fprintf chan "%s %s\n" (Archs.pp A.arch) doc.Name.name ;
    begin match doc.Name.doc with
    | "" -> ()
    | doc -> Out.fprintf chan "\"%s\"\n" doc
    end ;
    if withinfo then begin
      List.iter
        (fun (k,i) -> Out.fprintf chan "%s=%s\n" k i)
        t.info
    end ;
    Out.fprintf chan "\n{%s}\n\n" (dump_state  t.init) ;
    prog chan t.prog ;
    Out.fprintf chan "\n" ;
    let locs = DumpUtils.dump_locations I.dump_location I.dump_v t.locations in
    if locs <> "" then Out.fprintf chan "%s\n" locs ;
    begin match t.filter with
    | None -> ()
    | Some p -> Out.fprintf chan "filter %s\n" (I.dump_prop p)
    end ;
    begin match t.extra_data with
    | NoExtra|CExtra _ -> ()
    | BellExtra bi ->
        Out.fprintf chan "\n%s\n" (BellInfo.pp bi)
    end ;
    Out.fprintf chan "%s\n" (I.dump_constr t.condition) ;
    ()

  let dump = do_dump false
  let dump_info = do_dump true

  let (@@) f k = f k

  let lines doc t =
    begin fun k -> sprintf "%s %s" (Archs.pp A.arch) doc.Name.name :: k
    end @@
    begin fun k -> match doc.Name.doc with
    | "" -> k
    | doc -> sprintf "\"%s\"" doc :: k
    end @@
    begin fun k ->  sprintf "{%s}" (dump_state  t.init) :: k
    end @@
    begin
      fun k ->
      let pp = List.map fmt_col t.prog in
      let pp = Misc.lines_of_prog pp in
      let pp = List.map (sprintf "%s;") pp in
      pp @ ""::k
    end @@
    begin fun k ->
      match t.locations with
      | [] -> k
      | locs ->
          DumpUtils.dump_locations I.dump_location I.dump_v locs::k
    end @@
    [I.dump_constr t.condition]
end
