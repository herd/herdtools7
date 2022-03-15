(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Lexing

(* Configuration, to change kinds and condition *)
module type Config = sig
  val debuglexer : bool
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
  val macros : string option
  val libfind : string -> string
end

module DefaultConfig = struct
  let debuglexer    = false
  let check_kind _  = None
  let check_cond _  = None
  let macros        = None
  let libfind       = Misc.identity
end

(* input signature, a lexer and a parser for a given architecture *)
module type LexParse = sig
  type token
  type pseudo
  val lexer : Lexing.lexbuf -> token
  val parser : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> JavaAst.thread_body list
end

(* Output signature *)
module type S = sig
  type pseudo = JavaBase.pseudo
  type init = MiscParser.state
  type prog = (MiscParser.proc * pseudo list) list
  type locations = MiscParser.LocSet.t

  val parse : in_channel -> (* input channel *)
              Splitter.result ->  (* result from splitter *)
              pseudo MiscParser.t (* Result of generic parsing *)

  val parse_string : string -> 
              Splitter.result -> 
              pseudo MiscParser.t
end

module DumpJava = struct
  open JavaAst
  type code = MiscParser.proc * JavaBase.pseudo list
  let dump_prog ((proc, _, _) , body) =
    let body_str =
      let unwrapped = (List.map (fun ps -> match ps with
                                          | JavaBase.Instruction i -> i
                                          | _ -> assert false ) body) in
      String.concat "\n" (List.map (JavaBase.dump_instruction) unwrapped) in

    [ (Printf.sprintf "Thread%d {\n%s\n}" proc body_str) ]

  let dump_prog_lines prog =
      let pp = List.map dump_prog prog in
      let pp = List.concat pp in
      List.map (Printf.sprintf "%s\n") pp

  let print_prog chan prog =
    let pp = dump_prog_lines prog in
    List.iter (Printf.fprintf chan "%s") pp
end


module Make
    (O:Config)
    (A:ArchBase.S)
    (L: LexParse with type pseudo = A.pseudo) : S = 
struct

    type pseudo     = JavaBase.pseudo
    type init       = MiscParser.state
    type prog       = (MiscParser.proc * pseudo list) list
    type locations  = MiscParser.LocSet.t

    (****************)
    (* Basic Checks *)
    (****************)
    module U = GenParserUtils

    let call_parser = U.call_parser

    (* Check whether the thread numbers starts from 0 and are consecutive *)
    let check_procs procs =
      Misc.iteri
        (fun k p ->
          if k <> p then
            Warn.fatal "Processes must be Thread0, Thread1, ...")
        procs

    let check_regs = U.check_regs



(***********)
(* Parsing *)
(***********)

(* Lexers *)
    module LexConfig = struct let debug = O.debuglexer end
    module LU = LexUtils.Make (LexConfig)
    module SL = StateLexer.Make (LexConfig)

    let parse_cond lexbuf =
      let cond =  call_parser "cond" lexbuf SL.token StateParser.main_constr in
        cond

(* Compute hash as litmus does *)
(* we are just using the CTestHash for now *)
    module D = CTestHash.Make(DumpJava)

    module Do
         (I:sig
      	    type src
      	    val call_parser_loc :
                string ->             (* name of the section *)
                src ->                (* input channel *)
                Pos.pos2 ->           (* position of the section, see splitter's result *)
                'a ->                 (* a lexer *)
                ('a -> Lexing.lexbuf -> 'b) -> (* a parser *)
                'b                    (* AST list *)
          end) = 
    struct

    let parse chan (* input channel *)
              {
                Splitter.locs = (init_loc, prog_loc, constr_loc, _) ; (* The four sections of a litmus file *)
                name = name ;  (* All names of the test grouped *)
                info = info ; _(* Additional information as (key * value) pairs *)
        }  =

    (* Initial state specification. *)
    let init =
      I.call_parser_loc "init"
		      chan init_loc SL.token StateParser.init in

    (* Program *)
    let prog =
      I.call_parser_loc "prog" chan prog_loc L.lexer L.parser in

    (* check the list of thread numbers 0, ..., n *)
    let procs = List.map (fun p -> p.JavaAst.proc) prog in
      check_procs procs ;

    (* Condition to be checked after the run. *)
    let (locs, filter, final, _quantifiers) =
      I.call_parser_loc "final"
		      chan constr_loc SL.token StateParser.constraints in

    (* Check *)
      check_regs procs init locs final ;

    let all_locs = U.get_visible_locs locs final in

    (* transform from JavaAST.threadBody to a pair with the same content *)
    let prog =  List.map (fun p -> ((p.JavaAst.proc, None, MiscParser.Main), p.JavaAst.body)) prog in


    (* The output of the parser *)
    let parsed = {
        MiscParser.info;
        init = init;
        prog = prog;
        filter = filter;
        condition = final;
        locations = locs;
        extra_data = MiscParser.NoExtra
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
            let info = parsed.MiscParser.info in
            { parsed with MiscParser.info =
	       ("Hash", D.digest info init prog all_locs)::info ; }
        | Some _ -> parsed in

    parsed

end


(*************************************************************************)


  let parse chan x =
    let module Src = struct
      type src = in_channel
      let call_parser_loc name chan loc =
        let lexbuf = LU.from_section loc chan in
        call_parser name lexbuf
    end in
    let module P = Do(Src) in
    P.parse chan x

  let parse_string s x =
    let module Src = struct
      type src = string
      let call_parser_loc name s loc =
        let lexbuf = LU.from_section_string loc s in
        call_parser name lexbuf
    end in
    let module P = Do(Src) in
    P.parse s x

end
