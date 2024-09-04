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
  val verbose : int
  val check_kind : string -> ConstrGen.kind option
  val check_cond : string -> string option
end

module DefaultConfig = struct
  let debuglexer = false let verbose = 0
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
          MiscParser.proc list * instruction list list * MiscParser.extra_data
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
    with type instruction = A.parsedPseudo) : S with type pseudo = A.pseudo =
  struct
    type pseudo = A.pseudo
    type init = MiscParser.state
    type prog = (MiscParser.proc * pseudo list) list
    type locations = MiscParser.LocSet.t

    let call_parser = GenParserUtils.call_parser
    module U = GenParserUtils

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
      |  Misc.TransposeFailure | Invalid_argument _ ->
          Warn.fatal "mismatch in instruction lines"


(************************)
(* Various basic checks *)
(************************)

    let check_procs procs =
      let rec iter_rec i xs = match xs with
        | [] -> ()
        | (p,_,MiscParser.Main)::xs ->
           if i = p then
             iter_rec (i + 1) xs
           else
             Warn.fatal "Processes must be P0, P1, ..."
        | (p,_,MiscParser.FaultHandler)::xs ->
           if i >= p then
             iter_rec i xs
           else
             Warn.fatal "Fault Handler for an undefined process"
      in iter_rec 0 procs

 let check_regs procs = U.check_regs (List.map (fun (p,_,_) -> p) procs)

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

(* Translation from parsed instruction to internal ones *)
    let check_and_tr p =
      let p = A.pseudo_parsed_tr p in
      if A.pseudo_exists (fun p -> not (A.is_valid p)) p then
        Warn.user_error "Illegal instruction '%s'"
          (A.pseudo_dump A.dump_instruction p)
      else p

    let parsed_tr prog =
      List.map
        (List.map check_and_tr)
        prog

(***********)
(* Parsing *)
(***********)

(* Lexers *)
    module LexConfig = struct let debug = O.debuglexer end
    module LU = LexUtils.Make (LexConfig)
    module SL = StateLexer.Make (LexConfig)


    let parse_cond lexbuf =
      let cond =  call_parser "cond" lexbuf
          SL.token StateParser.main_constr in
      cond

    module D = TestHash.Make(A)

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
      MiscParser.check_env_for_dups init ;
      let procs,prog,extra_data =
        I.call_parser_loc "prog" chan prog_loc L.lexer L.parser in
      check_procs procs ;
      let prog = parsed_tr prog in
      let prog = transpose procs prog in
      let prog = expn_prog prog in
      let (locs,filter,final,_quantifiers) =
        I.call_parser_loc "final"
          chan constr_loc SL.token StateParser.constraints in
      check_regs procs init locs final ;
      let all_locs = U.get_visible_locs locs final in
      let parsed =
        {
         MiscParser.info; init; prog = prog;
         filter = filter;
         condition = final;
         locations = locs;
         extra_data = extra_data ;
       } in
      let name  = name.Name.name in
      let parsed =
        match O.check_cond name  with
        | None -> parsed
        | Some k ->
            let cond =
              let cond = parse_cond (Lexing.from_string k) in
              try (* Apply mapping as condition may be expressed with external
                     registers *)
                let map = List.assoc OutMapping.key info in
                let map = try LexOutMapping.parse map with _ -> assert false in
                let map = OutMapping.locmap_inverse map in
                if O.verbose > 0 then
                  MiscParser.LocMap.iter
                    (fun k v ->
                      Printf.eprintf "Loc %s -> %s\n"
                        (MiscParser.dump_location k)
                        (MiscParser.dump_location v))
                    map ;
                let map_loc loc = MiscParser.LocMap.safe_find loc loc map in
                let open ConstrGen in
                let map_rloc rloc = match rloc with
                  | Loc loc -> Loc (map_loc loc)
                  | Deref (loc,i) -> Deref (map_loc loc,i) in
                let open MiscParser in
                let map_atom = function
                  | LV (loc,v) -> LV (map_rloc loc,v)
                  | LL (loc1,loc2) ->  LL (map_loc loc1,map_loc loc2)
                  | FF (_,None,_) as a -> a
                  | FF (p,Some x,ft) ->
                      begin match map_loc (Location_global x) with
                      | Location_global x -> FF (p,Some x,ft)
                      | _ -> assert false
                      end
                in
                ConstrGen.map_constr map_atom cond
              with Not_found -> cond in
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
             { parsed with
               MiscParser.info =
               ("Hash",D.digest info init prog all_locs)::info ; }
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
