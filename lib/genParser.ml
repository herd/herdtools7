(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
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
  type instruction

  val lexer : Lexing.lexbuf -> token
  val parser :
        (Lexing.lexbuf -> token) -> Lexing.lexbuf ->
	  int list * instruction list list * MiscParser.gpu_data option
end

(* Output signature *)
module type S = sig
  type pseudo
  type init = MiscParser.state
  type prog = (int * pseudo list) list
  type locations = MiscParser.LocSet.t

  val parse_init : Lexing.lexbuf -> init
  val parse_prog : Lexing.lexbuf -> prog
  val parse_cond : Lexing.lexbuf -> MiscParser.constr

  val parse : in_channel -> Splitter.result ->  pseudo MiscParser.t
end


module Make
    (O:Config)
    (A:ArchBase.S)
    (L: LexParse
    with type instruction = A.pseudo) : S with type pseudo = A.pseudo =
  struct
    type pseudo = A.pseudo
    type init = MiscParser.state
    type prog = (int * pseudo list) list
    type locations = MiscParser.LocSet.t


(*
  Transpose the instructions:
  a list of rows -> a list of columns (each being the program
  for a given processor
*)
    let transpose procs prog =
      try
	let prog = Misc.transpose prog in
	List.combine procs prog 
      with
      |  Misc.TransposeFailure | Invalid_argument "List.combine" ->
	  Warn.fatal "mismatch in instruction lines"


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

(*******************)
(* Macro expansion *)  
(*******************)

    let rec expn  = function
      | [] -> []
      | A.Macro (name,regs)::k ->
          let f =
            try A.get_macro name
            with Not_found -> Warn.fatal "macro not found: %s" name in
          f regs (expn k)
      | i::k -> i::expn k

    let expn_prog =
      Label.reset () ;
      List.map (fun (p,code) -> p,expn code)


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

    let parse_prog lexbuf =
      let procs,prog,_ = 
        call_parser "prog" lexbuf L.lexer L.parser in
      check_procs procs ;
      let prog = transpose procs prog in
      let prog = expn_prog prog in
      prog

    let parse_cond lexbuf =
      let cond =  call_parser "cond" lexbuf
          SL.token StateParser.constr in
      cond

    let call_parser_loc name chan loc =
      let lexbuf = LU.from_section loc chan in
      call_parser name lexbuf

    module D = TestHash.Make(A)

    let parse chan
        {
         Splitter.locs = (init_loc, prog_loc,constr_loc,_) ;
         name = name ;
         info = info ; _
       }  =
      let init =
	call_parser_loc "init"
	  chan init_loc SL.token StateParser.init in
      let procs,prog,gpu_data =
	call_parser_loc "prog" chan prog_loc L.lexer L.parser in
      check_procs procs ;
      let prog = transpose procs prog in
      let prog = expn_prog prog in
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
         MiscParser.info; init; prog = prog;
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
        { parsed with
          MiscParser.info =
            ("Hash",D.digest init prog all_locs)::info ; } in
      parsed
  end
          
