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


{
open Printf

module type Config = sig
  val verbose : int
  val ppinfo : Lexing.position -> string -> unit
  val env : string StringMap.t
  val map : (string->string) StringMap.t
  val check_name : string -> bool
end

module Make(C:Config) = struct
open LexMisc

}

let digit = [ '0'-'9' ]
let num = digit+
let hexa = ['0'-'9' 'a'-'f' 'A'-'F' ]
let hexanum = "0x" hexa+
let alpha = [ 'a'-'z' 'A'-'Z']
let name = alpha (alpha|digit)*
let loc = name | ('$' (alpha+|digit+))
let testname  = (alpha|digit|'_' | '/' | '.' | '-' | '+' | '[' | ']')+
let nl = '\n'|"\r\n"
let blank = [' ' '\t']
let validation = "Undef"|"Succeeded"|"Failed"|"Ok"|"No"|"??"

rule main keep same out name = parse
| ("Test" blank+ (testname as t) ((blank+ (name))| ("")) as line) nl
    {let t = Misc.clean_name t in
     let keep = C.check_name t in
     out keep line ;
     incr_lineno lexbuf ;
    main keep same out (Some t) lexbuf }
| ("States" (blank+ (digit+ as _x))? | "Histogram" (blank+  '(' (digit+ as _x) blank+ "states" blank* ')')? as line)  nl
   { out keep line ;
     incr_lineno lexbuf ;
     let map = match name with
     | None -> None
     | Some n ->
         try Some (StringMap.find n C.map) with
         | Not_found -> None in
     let same = match map with
     | None -> same
     | Some map ->
         plines keep out map lexbuf ; (* If same name and some map -> rewrite *)
         if same then not keep else false in
     main keep same out name lexbuf }
| ("Hash" blank* '=' blank* (hexa* as hash) blank* as line) nl
  {
   let same =
     match name with
     | Some n ->
         begin try
           let env_hash = StringMap.find n C.env in
           if hash <> env_hash then begin
             let pos = lexbuf.Lexing.lex_curr_p in
             C.ppinfo pos n ;
             out keep (sprintf "Hash=%s" env_hash) ;
             (* If output then hash changed... *)
             if same then not keep else false
           end else begin
             out keep line ;
             same
           end with Not_found ->
             let pos = lexbuf.Lexing.lex_curr_p in
             if C.verbose > 0 then
               eprintf "%a: No hash for test %s\n" Pos.pp_pos pos n ;
             out keep line ;
           same
         end
     | None ->
         if C.verbose > 0 then begin
           let pos = lexbuf.Lexing.lex_curr_p in
           eprintf "%a: erasing hash\n" Pos.pp_pos pos
         end ;
         false in
   incr_lineno lexbuf ;
   main keep same out None lexbuf
 }
| nl
    {
     let line = "" in
     let same = match name with
     | Some n ->
          begin try
            let env_hash = StringMap.find n C.env in
            let pos = lexbuf.Lexing.lex_curr_p in
            C.ppinfo pos n ;
            out keep (sprintf "Hash=%s" env_hash) ;
            (* If output then hash changed... *)
            if same then not keep else false
          with Not_found ->
             let pos = lexbuf.Lexing.lex_curr_p in
             if C.verbose > 0 then
               eprintf "%a: No hash for test %s\n" Pos.pp_pos pos n ;
             out keep line ;
           same
         end
     | None -> same in
     out keep line ;
     incr_lineno lexbuf ;
     main keep same out name lexbuf
   }
| [^'\r''\n']*  as line nl
 { out keep line ; incr_lineno lexbuf ; main keep same out name lexbuf }
| eof { same }
| "" { error "LexHashLog.main" lexbuf }

and plines keep out map = parse
| (num blank* (":>"|"*>"))? as tok
  {
    let b = Buffer.create 16 in
    Buffer.add_string b tok ;
    pline (Buffer.add_string b) map lexbuf ;
    let line = Buffer.contents b in
    out keep line ; incr_lineno lexbuf ;
    plines keep out map lexbuf
  }
(* Over *)
|  ("Loop" blank+)? (((validation) ([^'\r''\n']*)) |("")) as line nl
  { out keep line ; incr_lineno lexbuf }
| eof { }

and pline out map = parse
| num ':' loc as loc
  { out (map loc) ;  pline out map lexbuf  }
| "=4294967295;"
  { out "=-1;" ;  pline out map lexbuf  }
| nl { () }
| _ as tok
  { out (String.make 1 tok) ; pline out map lexbuf  }
| "" { error "pline" lexbuf }

{
let call_lexer out fname =
  Misc.input_protect
    (fun chan ->
      let lexbuf = Lexing.from_channel chan in
      LexMisc.init_file fname lexbuf ;
      try main true true out None lexbuf with
      | LexMisc.Error (msg,pos) ->
          eprintf "%a: %s\n" Pos.pp_pos pos msg ;
          raise Exit)
    fname

let no_out _ _ = ()

let check fname = ignore (call_lexer no_out fname)

let rewrite fname =
  let outname = sprintf "%s.tmp" fname in
  let same =
    Misc.output_protect
      (fun ochan ->
        let out b line = if b then MySys.output_line ochan line in
        call_lexer out fname)
      outname in
  if not same then  begin
    if C.verbose > 0 then eprintf "File changed\n" ;
    MySys.move outname fname
  end else MySys.remove outname

  let check_chan chan =
    let lexbuf = Lexing.from_channel chan in
    LexMisc.init_file "*stdin*" lexbuf ;
    ignore (main true true no_out None lexbuf)

  let rewrite_chan chan =
    let lexbuf = Lexing.from_channel chan in
    LexMisc.init_file "*stdin*" lexbuf ;
    ignore begin
      main true true
        (fun b line -> if b then MySys.output_line stdout line)
        None lexbuf
    end
end
}
