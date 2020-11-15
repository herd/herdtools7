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

(******************************)
(* A 'generic' parsing module *)
(******************************)
open Lexing

let call_parser name lexbuf lex parse =
  try parse lex lexbuf
  with
  | LexMisc.Error (msg,pos) ->
      Printf.eprintf
        "%a: Lex error %s (in %s)\n" Pos.pp_pos pos msg name ;
      raise Misc.Exit
  | Parsing.Parse_error ->
      let lxm = lexeme lexbuf
      and start_loc = lexeme_start_p lexbuf
      and end_loc = lexeme_end_p lexbuf in
      Printf.eprintf
        "%a: unexpected '%s' (in %s)\n"
        Pos.pp_pos2 (start_loc,end_loc)
        lxm name ;
      raise Misc.Exit
  | e ->
      Printf.eprintf
        "%a: Uncaught exception %s (in %s)\n"
        Pos.pp_pos lexbuf.lex_curr_p
        (Printexc.to_string e) name ;
      assert false


(* Configuration, to change kinds and condition *)
module type Config = sig
  val debuglexer : bool
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
end

module DefaultConfig = struct
  let debuglexer = false
  let check_kind _ = None
  let check_cond _ = None
end
(* input signature, a lexer and a parser for a given architecture *)
module type LexParse = sig
  type token

  val lexer : Lexing.lexbuf -> token
  val parser :
    (Lexing.lexbuf -> token) -> Lexing.lexbuf -> string CAst.t list
end

(* Output signature *)
module type S = sig
  val parse : in_channel -> Splitter.result ->
    (MiscParser.state, string CAst.t list,
     MiscParser.prop, MiscParser.location,MiscParser.maybev) MiscParser.result
end


module Make
    (O:Config)
    (P:PseudoAbstract.S with type code = string CAst.t)
    (A:Arch_litmus.Base)
    (L: LexParse) : S =
  struct

(*
  Transpose the instructions:
  a list of rows -> a list of columns (each being the program
  for a given processor
    let transpose procs prog =
      try
        let prog = Misc.transpose prog in
        List.combine procs prog
      with
      |  Misc.TransposeFailure | Invalid_argument "List.combine" ->
          Warn.fatal "mismatch in instruction lines"
*)

(************************)
(* Various basic checks *)
(************************)

let check_procs prog =
  let procs =
    List.fold_left
      (fun acc -> function CAst.Test cfun -> acc @ [cfun.CAst.proc] | CAst.Global _ -> acc)
      []
      prog
  in
  Misc.iteri
    (fun k p ->
      if not (Misc.int_eq k p) then
        Warn.fatal "Processes must be P0, P1, ...")
    procs ;
  procs

let check_loc procs loc = match loc with
| MiscParser.Location_reg (p,_) ->
    if not (List.mem p procs) then
      Warn.fatal "Bad process P%i" p
| _ -> ()

let check_atom procs a =
  let open ConstrGen in
  match a with
  | LV (loc,_) -> check_loc procs loc
  | LL (l1,l2) -> check_loc procs l1 ; check_loc procs l2
  | FF _ -> ()

let check_regs procs init locs final =
  List.iter (fun (loc,_) -> check_loc procs  loc) init ;
  LocationsItem.iter_locs (check_loc procs) locs ;
  ConstrGen.fold_constr (fun a () -> check_atom procs a) final ()


(***********)
(* Parsing *)
(***********)

(* Extract locations from condition *)
let get_locs_atom a =
  let open ConstrGen in
  let open MiscParser in
  match a with
  | LV (loc,_) -> LocSet.add loc
  | LL (loc1,loc2) ->
      (fun k -> LocSet.add loc1 (LocSet.add loc2 k))
  | FF (_,x) -> LocSet.add (MiscParser.Location_global x)

let get_locs c = ConstrGen.fold_constr get_locs_atom c MiscParser.LocSet.empty

(* Lexers *)
    module LexConfig = struct let debug = O.debuglexer end
    module LU = LexUtils.Make (LexConfig)
    module SL = StateLexer.Make (LexConfig)

    let parse_cond lexbuf =
      let cond =  call_parser "cond" lexbuf
          SL.token StateParser.main_constr in
      cond

    let call_parser_loc name chan loc =
      let lexbuf = LU.from_section loc chan in
      call_parser name lexbuf

    module D = CTestHash.Make(P)

    let parse chan
        {
         Splitter.locs = (init_loc, prog_loc,constr_loc,_) ;
         name = name ;
         info = info ; _
       }  =
      let init =
        call_parser_loc "init"
          chan init_loc SL.token StateParser.init in
      let prog =
        call_parser_loc "prog" chan prog_loc L.lexer L.parser in
      let prog = List.map CAstUtils.strip_pointers prog in
      let procs = check_procs prog in
      let (locs,filter,final,_quantifiers) =
        call_parser_loc "final"
          chan constr_loc SL.token StateParser.constraints in
      check_regs procs init locs final ;
      let all_locs =
        let open MiscParser in
        LocSet.union
          (LocationsItem.fold_locs LocSet.add locs LocSet.empty)
          (get_locs final) in
      let parsed =
        {
         MiscParser.info; init; prog = prog;
         filter = filter;
         condition = final;
         locations = locs;
         extra_data = MiscParser.empty_extra;
       } in
      let name  = name.Name.name in
      let parsed =
        match O.check_cond name  with
        | None -> parsed
        | Some k ->
            let cond = parse_cond (Lexing.from_string k) in
            { parsed with
              MiscParser.condition = cond ;} in
      let parsed =
        match O.check_kind name  with
        | None -> parsed
        | Some k ->
            { parsed with
              MiscParser.condition =
              ConstrGen.set_kind k parsed.MiscParser.condition; } in
      let parsed =
        match MiscParser.get_hash parsed with
        | None ->
            MiscParser.set_hash parsed (D.digest init prog all_locs)
        | Some _ ->
            parsed in
      parsed
  end
