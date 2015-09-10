(* Find header files *)

{
open Printf
let verbose = false
}

rule main = parse
| ' '* '\n' { main lexbuf }
| "%{" { main lexbuf }
| "(*"|"/*"   as lxm
    { if verbose then eprintf "%s" lxm ; inside false lexbuf }
| "" { false }


and inside seen = parse
| ('i'|'I')('n'|'N')('r'|'R')('i'|'I')('a'|'A') as lxm
 { if verbose then eprintf "%s" lxm ; inside true lexbuf }
| "*)"|"*/" as lxm { if verbose then eprintf "%s\n" lxm ; outside seen lexbuf }
| _   as lxm { if verbose then eprintf "%c" lxm ; inside seen lexbuf }

and outside seen = parse
| ' '* '\n'         { outside seen lexbuf }
| "(*"|"/*" as lxm  { if verbose then eprintf "%s" lxm ; inside seen lexbuf }
| "" { seen }

{

let zyva chan = main (Lexing.from_channel chan)

let zyva fname =  Misc.input_protect zyva fname

let () =
  for i = 1 to Array.length Sys.argv-1 do
    let fname = Sys.argv.(i) in
    if not (zyva fname) then printf "%s\n" fname
  done
}
