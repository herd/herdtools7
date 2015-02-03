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


(*  Result of splitter *)
type result =
  {
    arch : Archs.t ;
    name : Name.t ;
    info : (string * string) list ;
    locs : Pos.pos2 * Pos.pos2 * Pos.pos2 *Pos.pos2 ;
    start : Lexing.position ;
  }

module type Config = sig
  include LexUtils.Config
  val check_rename : string -> string option
end

module Default = struct
  include LexUtils.Default
  let check_rename _ = None
end

module Make(O:Config) = struct
  module LU = LexUtils.Make(O)
  open LU
}

let digit = [ '0'-'9' ]
let alpha = [ 'a'-'z' 'A'-'Z']
let blank = [' ' '\t' '\r']
let bool = "true" | "false"
let name  = alpha (alpha|digit|'_' | '/' | '.' | '-')*
 (* yes some test names are such *)
let testname  = (alpha|digit|'_' | '/' | '.' | '-' | '+'|'['|']')+
let num = digit+


rule main_pos = parse
| ""
    {
     let start = lexeme_start_p lexbuf  in
     main start lexbuf
    }
and main start = parse
| blank* (alpha|digit|'_')+ as arch
  blank+
  (testname as tname)
  blank*
  ('(' (name as texname) ')' blank*) ?
  ( ('\n'? as line) blank*'"'([^'"']* as doc) '"' blank*) ? (* '"' *)
  ';' ?
  { begin match line with Some _ -> incr_lineno lexbuf | None -> () end ;
    let arch = Archs.lex arch in
    let (info,init1,(init2,prog1)) = find_init [] lexbuf in
    let is_empty,prog2 = inside_prog lexbuf in
    let constr2 =
      if is_empty then prog2 else inside_constr lexbuf in
    let loc_eof = find_eof lexbuf in
    let tname =
      if tname = "--" then
        Filename.chop_extension
          (Filename.basename init1.pos_fname)
      else if Filename.check_suffix tname ".litmus" then
 (* GRR follow litmus here *)
        Filename.chop_suffix tname ".litmus"
      else
        tname in
    let tname = match O.check_rename tname with
    | None -> tname
    | Some n -> n in
    let names =
      { Name.name = tname ;
        file = init1.pos_fname ;
        texname = Misc.proj_opt tname texname ;
        doc = Misc.proj_opt "" doc ; } in
    { arch = arch ;
      name = names ;
      info = info ;
      locs = ((init1,init2),(prog1,prog2),(prog2,constr2),(constr2,loc_eof)) ;
      start = start ;
    }
  }
| "" { error "first line" lexbuf }

and find_init info = parse
| '\n'  { incr_lineno lexbuf ;  find_init info lexbuf }
| '{'
    { let loc1 = lexeme_end_p lexbuf in
      let loc2 = inside_init lexbuf in
      List.rev info,loc1,loc2 }
| (name as key) blank* '=' blank* ([^'\n']* as value) '\n'
  { incr_lineno lexbuf ; find_init ((key,value)::info) lexbuf }
| [^'\n''{']+  { find_init info lexbuf }
| "" { error "find init section" lexbuf }

and inside_init = parse
| '\n'  { incr_lineno lexbuf ;  inside_init lexbuf }
| '}'   { lexeme_start_p lexbuf,lexeme_end_p lexbuf }
| [^'\n''}']+   { inside_init lexbuf }
| "" { error "inside init section" lexbuf }

and inside_prog  = parse
| '\n'  { incr_lineno lexbuf ;  inside_prog lexbuf }
 (* Had to erase comments to handle C-code *)
| "\""  { skip_string lexbuf ; inside_prog lexbuf }
(* | "<<" *)  (* Had to erase this to handle C-code *)
|eof
    { true,lexeme_start_p lexbuf } (* boolean -> empty constraint *)
(* | "+" *) (* Had to erase this to handle C-code, why here ?? *)
| "final"
| "forall"
| ('~' blank* "exists" )
| "exists"
| "cases" (* not sure if this line should still be here *)
| "observed"|"Observed"
| "locations"
   { false,lexeme_start_p lexbuf }
 (* name is for longest match to avoid confusion, in case of eg. forallx *)
| (name | _)  { inside_prog lexbuf }
| "" { error "inside_prog" lexbuf }

and inside_constr  = parse
| '\n'  { incr_lineno lexbuf ;  inside_constr lexbuf }
| "(*"  { skip_comment lexbuf ; inside_constr lexbuf }
| "<<"| eof  { lexeme_start_p lexbuf }
| _  { inside_constr lexbuf }
| "" { error "inside_constr" lexbuf }

and find_eof = parse
| '\n' { incr_lineno lexbuf ; find_eof lexbuf }
| [^'\n']+ { find_eof lexbuf }
| eof {  lexeme_start_p lexbuf }

{

(* Useful for debug *)
(*
 let pp_opt chan = function
   | None -> ()
   | Some s -> output_string chan s
*)

let pp_loc chan (i1,i2) =
  Printf.fprintf chan "%i-%i" i1.pos_cnum i2.pos_cnum

let show r =
  let loc_init,loc_prog,loc_constr,loc_cfgs = r.locs in
  Printf.eprintf
    "Test (arch=%s, name=%s, texname=%s, doc=%s)\nSplited as: init=%a, prog=%a, constr=%a, cfgs=%a\n"
    (Archs.pp r.arch)
    r.name.Name.name
    r.name.Name.texname
    r.name.Name.doc
    pp_loc loc_init
    pp_loc loc_prog
    pp_loc loc_constr
    pp_loc loc_cfgs

let split name chan =
  let lexbuf = from_channel chan in
  lexbuf.lex_curr_p <-
    {pos_fname = name; pos_lnum = 1;
     pos_bol = 0; pos_cnum = 0};
  let r =
    try main_pos lexbuf
    with
    | LexMisc.Error (msg,loc) ->
	Printf.eprintf "%a: splitter error in sublexer %s\n"
	  Pos.pp_pos loc msg ;
	raise Misc.Exit (* silent, message printed above *)
    | e ->
	Printf.eprintf
	  "%a: Uncaught exception in splitter %s\n"
	  Pos.pp_pos lexbuf.lex_curr_p
	  (Printexc.to_string e) ;
	assert false in
  if O.debug then show r ;
  r

end
}
