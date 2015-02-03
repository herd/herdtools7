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

  let from_section (start_pos,end_pos) ic =
    if O.debug then begin
      Printf.eprintf
        "Section: %a -> %a\n" Pos.debug_pos start_pos Pos.debug_pos end_pos ;
    end ;
    (* Start at start_pos *)
    seek_in ic start_pos.pos_cnum ; 

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
