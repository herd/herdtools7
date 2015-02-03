(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

{
  open Printf
  open AutoOpt

  exception Error of string
  let error msg = raise (Error msg)

  let get_int arg =
    try int_of_string arg with _ ->
    error "integer argument expected"

  let  get_bool arg = match arg with
  | "true" -> true
  | "false" -> false
  | _ ->  error "boolean argument expected"   
                                                                      
}

let blank = [' ''\t''\r']
let not_blank = [^' ''\t''\n''\r']
let alpha = ['A'-'Z' 'a'-'z']
let arg = 
  blank* '=' blank* 
 (not_blank | not_blank [^'\n']* not_blank as arg)
  blank* '\n'

rule lex_diy = parse
| "Generator produced" ' '+ (['0'-'9']+ as x)
  { try int_of_string x with _ -> assert false }
| ""
  { error "diy output" }

and lex_list = parse
| ([^',']* as arg)
    { arg::lex_list lexbuf }
| ',' { lex_list lexbuf }
| eof { [] }
| "" { error "lex_list" }


and lex_conf cfg = parse
| ('#'|'%') [^'\n']+ '\n'
| '\n' { lex_conf cfg lexbuf }
| "arch" arg
  {
   lex_conf
     (match Archs.parse arg with
       | None -> error (sprintf "unkown architecture: %s" arg)
       | Some a -> { cfg with arch =a; } )
     lexbuf
  } 
| "testing" blank* '=' blank* '\n'
    { lex_conf { cfg with testing = Some "" ; } lexbuf }
| "testing" arg
    { lex_conf { cfg with testing = Some arg ; } lexbuf }
| "safe" arg
    { lex_conf { cfg with safe = Some arg ; } lexbuf }
| "mode" arg
    { lex_conf (set_mode arg cfg) lexbuf }
| ("mach"|"run") arg
    {
     let m = parse_mach arg in
     match m with
     | None -> error (sprintf "bad mach: %s" arg)
     | Some m ->
         lex_conf { cfg with mach = m ; } lexbuf
    }
| "work_dir" arg
    { lex_conf { cfg with work_dir = arg; } lexbuf }
| "nprocs" arg
    { lex_conf (set_nprocs (get_int  arg) cfg) lexbuf }
| "diy_sz" arg
    { lex_conf { cfg with diy_sz = Some (get_int  arg); } lexbuf }
| "diy_opts" arg
    { lex_conf (set_diy_opts arg cfg) lexbuf }
| "litmus_opts" arg
    { lex_conf { cfg with litmus_opts = arg; } lexbuf }
| "run_opts" blank* '=' blank* '\n'
    { lex_conf { cfg with run_opts = [""] ; } lexbuf }
| "run_opts" arg
    {
      let opts = lex_list (Lexing.from_string arg) in
      lex_conf { cfg with run_opts = opts; } lexbuf
    }
| "cycle" arg
    { match parse_interpretation arg with
    | Some i -> lex_conf { cfg with interpretation = i ; } lexbuf
    | None -> error (sprintf "bad cycle interpretation: %s" arg) }
| "interactive" arg
    { lex_conf (AutoOpt.set_interactive (get_bool arg) cfg) lexbuf }
| "transitive" arg
    { lex_conf (AutoOpt.set_transitive (get_bool arg) cfg) lexbuf }
| "compress" arg
    { lex_conf (AutoOpt.set_compress (get_bool arg) cfg) lexbuf }
| "build" arg
    { lex_conf {cfg with build = arg; } lexbuf }
| "distrm" arg
    { lex_conf {cfg with distrm = arg; } lexbuf }
| "distaddpath" arg
    {
     let p = cfg.distaddpath in
     lex_conf  {cfg with distaddpath = p@[arg]; } lexbuf }
| "stabilise" arg
    { lex_conf { cfg with stabilise = get_int  arg; } lexbuf }
| eof { cfg }
| [^'\n']* as line '\n'?
  { error (sprintf "bad line in configuration: %s" line) }
{
 let diy chan =  lex_diy (Lexing.from_channel chan)

 let conf opt name =
   try
     Misc.input_protect
       (fun chan -> lex_conf opt (Lexing.from_channel chan))
       name
   with
   | Error msg -> Warn.fatal "%s" msg
}
