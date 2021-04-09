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

(* Configuration, to change kinds and condition *)
module type Config = sig
  val debuglexer : bool
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
  val macros : string option
  val libfind : string -> string
end

module DefaultConfig = struct
  let debuglexer = false
  let check_kind _ = None
  let check_cond _ = None
  let macros = None
  let libfind = Misc.identity
end
(* input signature, a lexer and a parser for a given architecture *)
module type LexParse = sig
  type token
  type pseudo

  val deep_lexer : Lexing.lexbuf -> token
  val deep_parser :
        (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	  pseudo list CAst.test list

  val shallow_lexer : Lexing.lexbuf -> token
  val shallow_parser :
        (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	  string CAst.t list

  type macro
  val macros_parser :
      (Lexing.lexbuf -> token) -> Lexing.lexbuf -> macro list
  val macros_expand : macro list -> pseudo -> pseudo
end

(* Output signature *)
module type S = sig
  type pseudo
  type init = MiscParser.state
  type prog = (MiscParser.proc * pseudo list) list
  type locations = MiscParser.LocSet.t

  val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
  val parse_string : string -> Splitter.result ->  pseudo MiscParser.t
end


module Make
    (O:Config)
    (A:ArchBase.S)
    (L: LexParse
    with type pseudo = A.pseudo) : S with type pseudo = A.pseudo =
  struct
    type pseudo = A.pseudo
    type init = MiscParser.state
    type prog = (MiscParser.proc * pseudo list) list
    type locations = MiscParser.LocSet.t

(****************)
(* Basic Checks *)
(****************)
    module U = GenParserUtils
    let call_parser = U.call_parser

    let check_procs procs =
      Misc.iteri
        (fun k p ->
          if k <> p then
            Warn.fatal "Processes must be P0, P1, ...")
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
module D = CTestHash.Make(DumpCAst)

module Do
         (I:
	    sig
	      type src
	      val call_parser_loc :
                string ->
                src -> Pos.pos2 ->
                'a -> ('a -> Lexing.lexbuf -> 'b) -> 'b
            end) = struct
  let parse chan
            {
              Splitter.locs = (init_loc, prog_loc,constr_loc,_) ;
              name = name ;
              info = info ; _
	    }  =
    let init =
      I.call_parser_loc "init"
		      chan init_loc SL.token StateParser.init in
    let prog =
      I.call_parser_loc "prog" chan prog_loc L.deep_lexer L.deep_parser in
    let prog_litmus =
      I.call_parser_loc "prog_litmus" chan prog_loc L.shallow_lexer L.shallow_parser in
    (* Add parameter passsing as inits *)
    let full_init =
      let open CAst in
      List.fold_left
        (fun env f -> match f with
		      | Global _ -> env
		      | Test t ->
			 let p = t.proc in
			 List.fold_left
			   (fun env param ->
			    let loc = param.param_name in
			    let ty = TestType.TyDef in
			    (MiscParser.Location_reg (p,loc),
			     (ty,ParsedConstant.nameToV loc))::env)
			   env t.params)
        init prog_litmus in
    let procs = List.map (fun p -> p.CAst.proc) prog in
    check_procs procs ;
    let params =  List.map (fun p -> p.CAst.params) prog in

    let expand_body = match O.macros with
    | None -> Misc.identity
    | Some fmacros ->
        let fname =  O.libfind fmacros in
        let ms =
          Misc.input_protect
            (fun chan ->
              let buff = Lexing.from_channel chan in
              LexMisc.init_file fname buff ;
              call_parser "macros"
                buff
                L.deep_lexer
                L.macros_parser)
            fname in
        List.map (L.macros_expand ms) in

    let prog =  List.map (fun p -> (p.CAst.proc,None),expand_body p.CAst.body) prog in
(*
    List.iter
      (fun (p,code) ->
        Printf.eprintf "++++++ %i\n" p ;
        List.iter
          (fun p ->
            A.pseudo_iter
              (fun i -> Printf.eprintf "%s\n" (A.dump_instruction i))
              p)
          code)
      prog ;
*)
    let (locs,filter,final,_quantifiers) =
      I.call_parser_loc "final"
		      chan constr_loc SL.token StateParser.constraints in
    check_regs procs init locs final ;
    let all_locs = U.get_visible_locs locs final in
    let parsed =
      {
        MiscParser.info; init=full_init; prog = prog;
        filter = filter;
        condition = final;
        locations = locs;
        extra_data = MiscParser.CExtra params;
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
    let parsed = match MiscParser.get_hash parsed with
        | None ->
            let info = parsed.MiscParser.info in
            { parsed with MiscParser.info =
	       ("Hash",
		(* For computing hash, we must parse as litmus does.
                This includes stripping away toplevel '*' of types *)
		let prog = List.map CAstUtils.strip_pointers prog_litmus in
		D.digest info init prog all_locs)::info ; }
        | Some _ -> parsed in
    parsed
end


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
