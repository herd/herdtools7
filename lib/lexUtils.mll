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

{
 open Lexing
 open LexMisc

module type Config = sig
  val debug : bool
end

module Default = struct
  let debug = false
end

module Make(O:Config) = struct

  let do_from_section seek input (start_pos,end_pos) ic =
    if O.debug then begin
      Printf.eprintf
        "Section: %a -> %a\n" Pos.debug_pos start_pos Pos.debug_pos end_pos ;
    end ;
    (* Start at start_pos *)
    seek ic start_pos.pos_cnum ;

    (* Will hold position of the next refill *)
    let next_pos = ref start_pos.pos_cnum in

    let lexbuf =
      from_function
        (fun buf n -> (* refill function *)
	  if !next_pos >= end_pos.pos_cnum then 0 (* pretend eof *)
	  else
	    let n_read = input ic buf 0 n in
            if O.debug then begin
              Printf.eprintf "** Refill: %i**\n%!" n_read ;
              Printf.eprintf
                "<<%s>>\n"
                (String.escaped (Bytes.sub_string buf 0 n_read))
            end ;
	    next_pos := !next_pos + n_read ;
	    (* will trigger refill as soon as end_pos is reached by lexer *)
	    if !next_pos > end_pos.pos_cnum then
	      n_read - (!next_pos - end_pos.pos_cnum)
	    else
	      n_read) in
    if O.debug then begin
      Printf.eprintf "start_pos=%a\n" Pos.debug_pos start_pos
    end ;
    (* Initialize position information maintained by lexing engine *)
    lexbuf.lex_curr_p <- start_pos ;
    (* lex_abs_pos is the absolute index of next refill, the lexing
       engine needs this information *)
    lexbuf.lex_abs_pos <- start_pos.pos_cnum ;
    lexbuf

  let from_section pos2 ic = do_from_section seek_in input pos2 ic

  module Source = struct
    type t =  { mutable pos : int ; b : string }

    let create s = { pos = 0  ; b = s; }

    let seek b pos = b.pos <- pos

    let input b buff p sz =
      let cur_pos = b.pos in
      let last_pos = String.length b.b in
      if cur_pos >= last_pos then 0 (* Passed end of buffer *)
      else begin
        let sz =
          if cur_pos + sz >= last_pos then last_pos - cur_pos
          else sz in
        String.blit b.b b.pos buff p sz ;
        b.pos <- cur_pos + sz ;
        sz
      end
  end

  let from_section_string pos2 s =
    let ic = Source.create s in
    do_from_section Source.seek Source.input pos2 ic

}

let digit = ['0'-'9']
let num = digit+
let blank = ['\t'' ']
rule skip_comment i = parse 
  | '\n' { incr_lineno lexbuf; skip_comment i lexbuf }   
  | "(*" { skip_comment (i+1) lexbuf }
  | "*)" 
      { if i > 1 then skip_comment (i-1) lexbuf}
  | eof { error "eof in skip_comment" lexbuf }
  | _ { skip_comment i lexbuf}

and skip_c_comment = parse
  | '\n' { incr_lineno lexbuf; skip_c_comment lexbuf }   
  | "*/" { () }
  | eof { error "eof in skip_c_comment" lexbuf }
  | _ { skip_c_comment lexbuf}

and skip_c_line_comment = parse
  | '\n' { incr_lineno lexbuf }   
  | eof { () }
  | _ { skip_c_line_comment lexbuf}

and skip_string = parse 
  | '\n' 	{ error "newline in skip_string" lexbuf }   
  | "\""	{ () }
  | eof 	{ error "eof in skip_string" lexbuf }
  | _ 		{ skip_string lexbuf}

{

let skip_comment = skip_comment 1
end
}
