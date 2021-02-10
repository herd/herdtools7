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


(*  Result of splitter *)
type info = string * string

type result =
  {
    arch : Archs.t ;
    name : Name.t ;
    info : info list ;
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

let add_info buff k v =
  Buffer.add_string buff k ;
  Buffer.add_char buff '=' ;
  Buffer.add_string buff v ;
  Buffer.add_char buff '\n'

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
    let do_skip_comments = match arch with
    | `C|`CPP -> false (* We can't skip comments in C, because "(*" is legal and frequent syntax *)
    | _ -> true in
    let is_empty,prog2 = inside_prog do_skip_comments lexbuf in
    let constr2 =
      if is_empty then prog2 else inside_constr lexbuf in
    let loc_eof = find_eof lexbuf in
    let tname =
      if tname = "--" then
        Filename.chop_extension
          (Filename.basename init1.pos_fname)
      else
 (* GRR follow litmus here *)
        Misc.clean_name tname in
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
| "(*" { skip_comment lexbuf ;  find_init info lexbuf }
| '{'
    { let loc1 = lexeme_end_p lexbuf in
      let loc2 = inside_init (Buffer.create 16) 0 lexbuf in
      List.rev info,loc1,loc2 }
| (name as key) blank* '=' blank* ([^'\n''\r']* as value) '\r'?'\n'
  { incr_lineno lexbuf ; find_init ((key,value)::info) lexbuf }
| [^'\n''{']+  { find_init info lexbuf }
| "" { error "find init section" lexbuf }

(* Courtesy of Luc - remove this comment when upstreaming *)
and inside_init buff depth = parse
| '{'   { Buffer.add_char buff '{'; inside_init buff (depth+1) lexbuf }
| '\n'  { incr_lineno lexbuf ;  inside_init buff depth lexbuf }
| '}'   { if depth > 0 then begin
            Buffer.add_char buff '}';
            inside_init buff (depth -1) lexbuf
          end else
            lexeme_start_p lexbuf,lexeme_end_p lexbuf
          }
| "" { error "inside init section" lexbuf }
| _ as c { Buffer.add_char buff c; inside_init buff depth lexbuf }

and inside_prog do_skip_comments = parse
| '\n'  { incr_lineno lexbuf ;  inside_prog do_skip_comments lexbuf }
(* Had to erase comments to handle C-code *)
| "(*"
    { if  do_skip_comments then  begin
      skip_comment lexbuf
    end ;
    inside_prog do_skip_comments lexbuf }
| "\""  { skip_string lexbuf ; inside_prog do_skip_comments lexbuf }
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
| "filter"
   { false,lexeme_start_p lexbuf }
 (* name is for longest match to avoid confusion, in case of eg. forallx *)
| (name | _)  { inside_prog do_skip_comments lexbuf }
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

(* Change info in the init section *)
and change_main buff p = parse
| blank* (alpha|digit|'_')+
  blank+
  testname
  blank*
  ('(' name ')' blank*) ?
  ( ('\n'? as line) blank*'"'[^'"']* '"' blank*) ? (* '"' *)
  [^'\n']* '\n' as lexed
  { begin match line with Some _ -> incr_lineno lexbuf | None -> () end ;
    incr_lineno lexbuf ;
    Buffer.add_string buff lexed ;
    change_info false p buff lexbuf }

and change_info found p buff = parse
| eof
    { () }
| '\n'
    { incr_lineno lexbuf ;
      Buffer.add_char buff '\n' ;
      change_info found p buff lexbuf }
| '\n'* '{' as lexed
    { incr_lineno lexbuf ;
      if not found then begin
        let k,v = p in
        add_info buff k v
      end ;
      Buffer.add_string buff lexed ;
      change_info found p buff lexbuf }
| (name as key) blank* '=' blank* [^'\n']* '\n' as line
  { incr_lineno lexbuf ;
    let k,v = p in
    if O.debug then Printf.eprintf "Found key: %s\n%!" key ;
    let found =
      if k = key then begin
        if not found then add_info buff k v ;
        true
      end else begin
        Buffer.add_string buff line ;
        found
      end in
    change_info found p buff lexbuf }
| [^'\n''{']+  as lexed
    { Buffer.add_string buff lexed ;
      change_info found p buff lexbuf }
| "" { error "change info" lexbuf }


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

let split_lexbuf name lexbuf =
  lexbuf.lex_curr_p <-
    {pos_fname = name; pos_lnum = 1;
     pos_bol = 0; pos_cnum = 0};
  let r =
    try main_pos lexbuf
    with
    | LexMisc.Error (msg,loc) ->
       failwith (Printf.sprintf "%s: splitter error in sublexer %s" (Pos.str_pos loc) msg)
    | Assert_failure _ as e ->  raise e
    | e ->
       failwith (Printf.sprintf "%s: Uncaught exception in splitter %s" (Pos.str_pos lexbuf.lex_curr_p) (Printexc.to_string e))
  in
  if O.debug then show r ;
  r

let reinfo p lexbuf =
  let buff = Buffer.create 32 in
  change_main buff p lexbuf ;
  Buffer.contents buff

let rehash v lexbuf = reinfo (MiscParser.hash_key,v) lexbuf

let split name chan =
  let lexbuf = from_channel chan in
  split_lexbuf name lexbuf

let split_string name s =
  let lexbuf = from_string s in
  split_lexbuf name lexbuf
end
}
