{
open Lexing
open Printf

let csname ochan suf key = fprintf ochan "\\%s%s" key suf
let begin_name ochan key = csname ochan "Begin" key
let end_name ochan key = csname ochan "End" key
let begin_command ochan key num =
  fprintf ochan "\\newcommand{%a}{%d}\n" begin_name key num
let end_command ochan key num =
  fprintf ochan "\\newcommand{%a}{%d}\n" end_name key num
let outcode ochan code = fprintf ochan "%s\n" code

let get_lineno lexbuf = lexbuf.lex_curr_p.pos_lnum
let incr_lineno = LexMisc.incr_lineno

}

let alpha = ['a'-'z''A'-'Z']
let num = ['0'-'9']
let name = alpha (alpha|num)*

rule outside ochan = parse
| ' '* "(*" ' '+ "Begin" ' '+
    (name '.')? (name as key) ' '+ "*)" [^'\n']* '\n' 
    {
     incr_lineno lexbuf ;
     begin_command ochan key (get_lineno lexbuf) ;
     inside ochan key lexbuf
    }
| [^'\n']* '\n'
    { incr_lineno lexbuf ;
      outside ochan lexbuf }
|  [^'\n']* eof
    { () }

and inside ochan key = parse 
| ' '* "(*" ' '+ "Begin" ' '+
    (name '.')? (name as key2) ' '+ "*)" [^'\n']* '\n' 
(* End marker ommitted *)
    {
     end_command ochan key (get_lineno lexbuf-1) ;
     incr_lineno lexbuf ;
     begin_command ochan key2 (get_lineno lexbuf) ;
     inside ochan key2 lexbuf
   }
| ' '* "(*" ' '+ "End" ' '+ [^'\n']* '\n'
    {
     end_command ochan key (get_lineno lexbuf-1) ;
     incr_lineno lexbuf ;
     outside ochan lexbuf
    }
|  [^'\n']* '\n'
    {
     incr_lineno lexbuf ;
     inside ochan key lexbuf
    }
| ""
    { LexMisc.error (sprintf "Begin %s pending" key) lexbuf }

{
   let scan_chan ochan chan =
     outside ochan (Lexing.from_channel chan)

    let zyva ochan =
     if Array.length Sys.argv  <= 1 then
        scan_chan ochan stdin
      else
        Misc.input_protect (scan_chan ochan) Sys.argv.(1)

    let () = zyva stdout ; exit 0
}
