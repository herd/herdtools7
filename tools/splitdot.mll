{
(* Split a multi-image dot file into one dot file per image *)
open Printf
let eof = ref false
}
 rule main c = parse
| "digraph" [' ']+ [^'\n']* '\n' as lxm
  {
   if c <= 0 then []
   else
     let b = Buffer.create 32 in
     Buffer.add_string b lxm ;
     image b lexbuf ;
     Buffer.contents b :: main (c-1) lexbuf
 }
| [^'\n']* '\n'  { main c lexbuf }
| eof { eof := true ; [] }


and image b = parse
| ' '* '}' ' '* '\n' as lxm
  { Buffer.add_string b lxm }
|  [^'\n']* '\n'  as lxm
    { Buffer.add_string b lxm  ; image b lexbuf }

{
 
let args = ref []
let max = ref 16

let prog = if Array.length Sys.argv > 0 then Sys.argv.(0) else "splitdot"

let () =
  Arg.parse
    ["-max",Arg.Int (fun i -> max := i),
     sprintf "<n> at most <n> images, default %i" !max]
    (fun s -> args := !args @ [s])
    (sprintf "usage: %s [options] filename" prog)

let name = match !args with
| [name] -> name
| _ -> eprintf "usage: %s [options] filename" prog ; exit 2

let dir = Filename.dirname name
let base = Filename.basename name
let base = Filename.chop_extension base

let chan = open_in name
let imgs = main !max (Lexing.from_channel chan)
let () = close_in chan

let c = ref 0

let () =
  List.iter
    (fun txt ->
      let x = !c in
      incr c ;
      let name =
        (Filename.concat dir (sprintf "%s-%02i.dot" base x)) in
      let chan = open_out name in
      output_string chan txt ;
      close_out chan ;
      printf "%s\n" name)
    imgs ;
  if not !eof then printf "MORE\n" ;
  ()

}
