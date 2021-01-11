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

(*****************)
(* Parsable dump *)
(*****************)
module type I = sig
  module A : Arch_litmus.Base

  module P : PseudoAbstract.S

  type state
  val dump_state :state -> string

  type prop
  val dump_prop : prop -> string
  val dump_constr : prop ConstrGen.constr -> string

  type location
  val dump_location : location -> string
end

module Make(I:I) : sig
  val dump : out_channel ->
    Name.t ->
    (I.state, I.P.code list, I.prop, I.location)
        MiscParser.result
      -> unit
  val dump_info : out_channel ->
    Name.t ->
    (I.state, I.P.code list, I.prop, I.location)
        MiscParser.result
      -> unit
  val lines :
      Name.t ->
        (I.state, I.P.code list, I.prop, I.location)
          MiscParser.result
      -> string list
end = struct
  open Printf
  open I

(*
    let prog chan prog =
    let pp = List.map I.P.dump_prog prog in
    Misc.pp_prog chan pp
*)
(*
    dump_procs chan prog ;
    iter_prog (dump_ios chan)
      (List.map snd prog)
*)
  open MiscParser

  let dump_rloc = ConstrGen.dump_rloc I.dump_location

  let dump_loc_type (rloc,t) = match t with
  | TyDef -> dump_rloc rloc ^";"
  | TyDefPointer -> dump_rloc rloc ^"*;"
  | Ty t -> sprintf "%s %s;" (dump_rloc rloc) t
  | Pointer t -> sprintf "%s %s*;" (dump_rloc rloc) t
  | TyArray _|Atomic _ -> assert false (* No arrays nor atomics in locations *)
  let dump_locations env =
    let pp = List.map dump_loc_type env in
    String.concat " " pp

  let do_dump withinfo chan doc t =
    fprintf chan "%s %s\n" (Archs.pp A.arch) doc.Name.name ;
    if withinfo then begin
      List.iter
        (fun (k,i) -> fprintf chan "%s=%s\n" k i)
        t.info
    end else begin
      List.iter
        (fun (k,i) ->
          if k = MiscParser.stable_key then fprintf chan "%s=%s\n" k i)
        t.info
    end ;
    begin match doc.Name.doc with
    | "" -> ()
    | doc -> fprintf chan "\"%s\"\n" doc
    end ;
    fprintf chan "\n{%s}\n\n" (dump_state  t.init) ;
    I.P.print_prog chan t.prog ;
    fprintf chan "\n" ;
    begin match t.locations with
    | [] -> ()
    | locs ->
        fprintf chan "locations [%s]\n" (dump_locations locs)
    end ;
    fprintf chan "%s\n" (I.dump_constr t.condition) ;
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
      let pp = I.P.dump_prog_lines t.prog in
      pp @ ""::k
    end @@
    begin fun k ->
      match t.locations with
      | [] -> k
      | locs ->
        sprintf "locations [%s]" (dump_locations locs)::k
    end @@
    [I.dump_constr t.condition]
end
