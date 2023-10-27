(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2019-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "cat2html7"

module Make
    (O:sig
      val verbose : int
      val name : string
      val inp : in_channel
      val out : out_channel
      val css : string
      val lexer : LexItem.collect -> Lexing.lexbuf -> unit
    end) =
  struct
    let () = ignore O.verbose
    let p fmt = fprintf O.out fmt
    let pl = p "%s\n"

    let prelude () =
      pl "<!DOCTYPE html>" ;
      pl "<html>" ;
      pl "<head>" ;
      pl "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=US-ASCII\">" ;
      p "<meta name=\"generator\" content=\"%s\">\n" prog ;
      p "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\">\n" O.css ;
      p  "<title>%s</title>\n" (Filename.basename O.name) ;
      pl "</head>" ;
      pl "<body>" ;
      pl "<div class=\"show\">" ;
      ()

    let postlude () =
      pl "</div>";
      pl "</body>" ;
      pl "</html>" ;
      ()
    open LexItem

    let classof = function
      | Keyword -> "kwd"
      | Comment -> "comment"
      | String -> "string"
      | _ -> ""

    let linkof s =
      let len = String.length s in
      if len < 6 then None
      else if s.[0] <> '"' || s.[len-1] <> '"' then None
      else
        let s = String.sub s 1 (len-2) in
        if Filename.check_suffix s ".cat" then Some (s ^ ".html")
        else None

    let out_token cls s =
      if cls = "" then output_string O.out s
      else fprintf O.out "<span class=\"%s\">%s</span>" cls s

    let zyva () =
      prelude () ;
      let q = Queue.create () in
      let f k s = Queue.add (k,s) q in
      let lexbuf = Lexing.from_channel O.inp in
      let rec lexstep lexbuf =
        O.lexer f lexbuf ;
        let rec over () =
          try
            let k,s = Queue.take q in
            match k with
            | Eof -> true
            | _   ->
                let cls = classof k in
                begin match k  with
                | String ->
                    begin match linkof s with
                    | None -> out_token cls s
                    | Some link ->
                        fprintf O.out "<a href=\"%s\">" link ;
                        out_token cls s ;
                        output_string O.out "</a>"
                    end
                | _ -> out_token cls s
                end ;
                over ()
          with Queue.Empty -> false in
        if not (over ()) then lexstep lexbuf in
      lexstep lexbuf ;
      postlude ()
  end

let outname =  ref None
let verbose = ref 0
let arg = ref None
let setarg name = match !arg with
| None -> arg := Some name
| Some _ -> raise (Arg.Bad "One argument at most")

let opts =
  [
   "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
   "-o",
   Arg.String (fun name -> outname := Some name),
   "<name> overide default filename"
  ]

let () =
  Arg.parse opts setarg
    (sprintf "Usage: %s [options]* [test]" prog)

let tr_name name =
  Misc.input_protect
    (fun inp ->
      let oname = match !outname with
      | None -> sprintf "%s.html" name
      | Some name ->  name in
      try
        Misc.output_protect
          (fun out ->
            let module Lex = ModelLexer.Make(struct let debug = false end) in
            let module M =
              Make
                (struct
                  let verbose = !verbose
                  let name = name
                  let inp = inp
                  let out = out
                  let css = "cat.css"
                  let lexer = Lex.token_fun
                end) in
            M.zyva ())
          oname
      with e -> MySys.remove oname ; raise e)
    name

let tr_name name =
  try tr_name name
  with  LexMisc.Error (msg,pos) ->
    Printf.eprintf
      "%a: Lex error %s\n" Pos.pp_pos { pos with Lexing.pos_fname=name } msg ;
    exit 2

let () = match !arg with
| None -> ()
| Some name ->
    if Filename.check_suffix name ".cat" then tr_name name
