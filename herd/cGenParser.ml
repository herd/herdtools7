(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

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
  type pseudo

  val deep_lexer : Lexing.lexbuf -> token
  val deep_parser :
        (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	  (int * pseudo list) list * MiscParser.gpu_data option

  val shallow_lexer : Lexing.lexbuf -> token
  val shallow_parser :
        (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	  string CAst.t list
end

(* Output signature *)
module type S = sig
  type pseudo
  type init = MiscParser.state
  type prog = (int * pseudo list) list
  type locations = MiscParser.LocSet.t

  val parse_init : Lexing.lexbuf -> init
  val parse_cond : Lexing.lexbuf -> MiscParser.constr

  val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
end


module Make
    (O:Config)
    (A:ArchBase.S)
    (L: LexParse
    with type pseudo = A.pseudo) : S with type pseudo = A.pseudo =
  struct
    type pseudo = A.pseudo
    type init = MiscParser.state
    type prog = (int * pseudo list) list
    type locations = MiscParser.LocSet.t


(************************)
(* Various basic checks *)
(************************)

let check_procs procs =
  Misc.iteri
    (fun k p ->
      if k <> p then
        Warn.fatal "Processes must be P0, P1, ...")
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

let check_regs procs init locs final =
  List.iter (fun (loc,_) -> check_loc procs  loc) init ;
  List.iter (fun (loc,_) -> check_loc procs  loc) locs ;
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

let get_locs c = ConstrGen.fold_constr get_locs_atom c MiscParser.LocSet.empty

(* Lexers *)
    module LexConfig = struct let debug = O.debuglexer end
    module LU = LexUtils.Make (LexConfig)
    module SL = StateLexer.Make (LexConfig)
(*  module STL = ScopeTreeLexer.Make (LexConfig) *)

    let parse_init lexbuf =
      call_parser "init" lexbuf SL.token StateParser.init

    (* let parse_prog lexbuf =
      let procs,prog,_ = 
        call_parser "prog" lexbuf L.lexer L.parser in
      check_procs procs ;
      let prog = transpose procs prog in
      let prog = expn_prog prog in
      prog
    *)
    let parse_cond lexbuf =
      let cond =  call_parser "cond" lexbuf
          SL.token StateParser.constr in
      cond

    let call_parser_loc name chan loc =
      let lexbuf = LU.from_section loc chan in
      call_parser name lexbuf

(* Compute hash as litmus does *)
    module D = CTestHash.Make(DumpCAst)

    let parse chan
        {
         Splitter.locs = (init_loc, prog_loc,constr_loc,_) ;
         name = name ;
         info = info ; _
       }  =
      let init =
	call_parser_loc "init"
	  chan init_loc SL.token StateParser.init in
      let prog,gpu_data =
	call_parser_loc "prog" chan prog_loc L.deep_lexer L.deep_parser in
      let prog_litmus =
        call_parser_loc "prog_litmus" chan prog_loc L.shallow_lexer L.shallow_parser in
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
                   let ty = MiscParser.TyDef in
                   (MiscParser.Location_reg (p,loc),
                    (ty,SymbConstant.nameToV loc))::env)
                 env t.params)
          init prog_litmus in
      let procs = List.map fst prog in 
      check_procs procs ;
      let (locs,final,_quantifiers) =
	call_parser_loc "final"
	  chan constr_loc SL.token StateParser.constraints in
      check_regs procs init locs final ;
      let all_locs =
        MiscParser.LocSet.union
          (MiscParser.LocSet.of_list (List.map fst locs))
          (get_locs final) in
      let parsed =
        {
         MiscParser.info; init=full_init; prog = prog;
         condition = final; 
         locations = locs;
         gpu_data;
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
        let info = parsed.MiscParser.info in
        { parsed with MiscParser.info =
            ("Hash",
             (* For computing hash, we must parse as litmus does.
                This includes stripping away toplevel '*' of types *)
             let prog = List.map CAstUtils.strip_pointers prog_litmus in
             D.digest init prog all_locs)::info ; } in
      parsed
  end
         
