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

(** Extract definitions *)
{
open Printf

let verbose = ref false
let outdir = ref None
let ndefs = ref 0

let w,ws =
  ["effect"; "Effect";],
  [["if";]; ["one"; "all"; "any";]; ["of";]; ["applies:"; "apply:";];]
}

let nonl = [^'\n']
let nocol = [^':''.']
let eol = ' '* '\n'

rule main = parse
|'\n' { main lexbuf }
| _ as c
  {
    let buff = Buffer.create 128 in
    Buffer.add_char buff c ;
    match definition w ws buff lexbuf with
    | None -> main lexbuf
    | Some line ->
        begin
          match !outdir with
          | None ->
             printf "%s\n" line ;
             items stdout lexbuf ;
             print_endline ""
          | Some d ->
              let fname = Filename.concat d (sprintf "def%02d.txt" !ndefs) in
              incr ndefs ;
              Misc.output_protect
                (fun chan ->
                   fprintf chan "%s\n" line ;
                   items chan lexbuf)
                fname
         end ;
         main lexbuf
   }
| eof { () }

(* Find successive words w::ws in a paragraph *)
and definition w ws buff = parse
| ([' ']+|'\n'|"\n\n"|[^' ''\n']+) as lxm
   {
     if !verbose then eprintf "Lxm: %S\n%!" lxm;
     match lxm with
     | "\n\n" -> None
     | _ ->
        begin
          Buffer.add_string buff lxm ;
          if List.exists (String.equal lxm) w then begin
             if !verbose then eprintf "Ok: %S\n%!" lxm;
             match ws with
             | [] -> Some (Buffer.contents buff)
             | w::ws -> definition w ws buff lexbuf
          end else definition w ws buff lexbuf
        end
     }
| "" { None }

and items chan = parse
| ("o"|"-"|"--") [^'.']+ '.' as line eol
  {
     fprintf chan "%s\n" line ;
     items chan lexbuf
  }
| eol { items chan lexbuf }
| ""|eof { () }

{
  let prog =
    if Array.length Sys.argv > 0 then
      Filename.basename Sys.argv.(0)
  else "extractdefs7"

 let arg = ref None

 let options =
   [
     ("-version", Arg.Unit
     (fun () -> printf "%s, Rev: %s\n" Version.version Version.rev ; exit 0),
     " show version number and exit") ;
     ("-v", Arg.Set verbose,"be verbose") ;
     ("-o", Arg.String (fun s -> outdir := Some s),
      "<dir> output definitions in directory <dir>")
   ]

 let () =
  Arg.parse
    options
    (fun s -> arg := Some s)
    (sprintf "Usage: %s [option] [file]" prog)

  let zyva chan = Lexing.from_channel chan |> main

  let arg = !arg

  let () =
      match arg with
      | None -> zyva stdin
      | Some name -> Misc.input_protect zyva name

}
