(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Introduce Indent/DeIndent Lexemes *)

{

module
  Make (O:sig val verbose : bool end) =
struct
  type t = Indent | DeIndent | Level

  let indent = "<+>"
  let deindent = "<->"

  let out pp st c = match st with
    | []|[_] when not O.verbose -> ()
    | _ -> pp c

  let out_char = out print_char
  let out_string = out print_string

  let out_indent =
    if O.verbose then
      fun st id -> out_string st ("\n" ^ id)
    else out_string

  let out_indent st = function
    | Indent -> out_indent st indent
    | DeIndent -> out_indent st deindent
    | Level -> out_indent st "<=>"

  let check_indent st n =
    match st with
    | [] -> [n;],Indent
    | m::_ when n > m -> n::st,Indent
    | m::_ when n = m -> n::st,Level
    | _::(m::_ as st) when n=m -> st,DeIndent
    | _ -> failwith "Bad indentation"

  let pp_st st =
    Printf.eprintf "[%s]\n%!"
      (List.map (Printf.sprintf "%d") st |> String.concat "; ")

}

rule main st = parse
| _ as c { out_char st c ; main st lexbuf }
| '\n'+ [^' '] [^'\n']*  { main st lexbuf }
| '\n'+ ([' ']* as sp)
  { let st,indent = check_indent st (String.length sp) in
    if O.verbose then pp_st st ;
    out_indent st indent ;
    main st lexbuf }
| eof { () }

{

  let zyva lexbuf = main [] lexbuf 

end

open Printf

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "lexindent7"


let verbose = ref false
let arg = ref None

let () =
  try
    Arg.parse
        ["-v", Arg.Set verbose, "be verbose";]
          (fun s -> arg := Some s)
          (sprintf
             "Usage: %s [options] [file], insert indent/deindent coded lexemes"
             prog)
  with
  | Misc.Fatal msg -> eprintf "%s: %s\n%!" prog msg


let arg = !arg

module Zyva =
  Make
    (struct
      let verbose = !verbose
    end)

let main_chan chan = Lexing.from_channel chan |> Zyva.zyva

let () =
  match arg with
  | None -> main_chan stdin
  | Some name -> Misc.input_protect main_chan name
}

