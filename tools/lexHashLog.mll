(*********************************************************************)
(*                       DIY                                         *)
(*                                                                   *)
(*               Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


{
open Printf

module type Config = sig
  val verbose : int
  val ppinfo : Lexing.position -> string -> unit
  val env : string StringMap.t
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
let testname  = (alpha|digit|'_' | '/' | '.' | '-' | '+' | '[' | ']')+
let nl = '\n'|"\r\n"
let blank = [' ' '\t']

rule main same out name = parse
| ("Test" blank+ (testname as t) ((blank+ (name))| ("")) as line) nl 
  { out line ;
    incr_lineno lexbuf ;
    main same out (Some t) lexbuf }
| ("Hash" blank* '=' blank* (hexa+ as hash) blank* as line)  nl 
  {   
   let name = match name with
   | Some n -> n
   | None -> error "hash with no name" lexbuf in
   let same =
     try
       let env_hash = StringMap.find name C.env in
       if hash <> env_hash then begin
         let pos = lexbuf.Lexing.lex_curr_p in
         C.ppinfo pos name ;
         out (sprintf "Hash=%s" env_hash) ;
         false
     end else begin
       out line ;
       same
     end with Not_found ->
       let pos = lexbuf.Lexing.lex_curr_p in
       if C.verbose > 0 then
         eprintf "%a: No hash for test %s\n" Pos.pp_pos pos name ;
       out line ;
       same in
   incr_lineno lexbuf ;
   main same out None lexbuf
  }
| [^'\r''\n']*  as line nl
 { out line ; incr_lineno lexbuf ; main same out name lexbuf }
| eof { same }
| "" { error "LexHashLog.main" lexbuf }
{

let call_lexer out fname =
  Misc.input_protect
    (fun chan ->
      let lexbuf = Lexing.from_channel chan in
      LexMisc.init_file fname lexbuf ;
      main true out None lexbuf)
    fname

let no_out _ = ()

let check fname = ignore (call_lexer no_out fname)

let rewrite fname =
  let outname = sprintf "%s.tmp" fname in
  let same =
    Misc.output_protect
      (fun ochan ->
        let out = MySys.output_line ochan in
        call_lexer out fname)
      outname in
  if not same then  begin
    if C.verbose > 0 then eprintf "File changed\n" ;
    MySys.move outname fname
  end else MySys.remove outname

  let check_chan chan =
    let lexbuf = Lexing.from_channel chan in
    LexMisc.init_file "*stdin*" lexbuf ;
    ignore (main true no_out None lexbuf)

  let rewrite_chan chan =
    let lexbuf = Lexing.from_channel chan in
    LexMisc.init_file "*stdin*" lexbuf ;
    ignore (main true  (MySys.output_line stdout) None lexbuf)

end
}
