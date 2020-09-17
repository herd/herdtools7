(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2020-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** List files imported from a cat file *)

open Printf

let prog =
  if Array.length Sys.argv > 0 then
    Filename.basename Sys.argv.(0)
  else "cat2includes7"

module Make
    (O:sig
      val verbose : int
      val includes : string list
    end) =
  struct
    module ML =
      MyLib.Make
        (struct
          let includes = O.includes
          let env =  Some "HERDLIB"
          let libdir = Filename.concat Version.libdir "herd"
          let debug = O.verbose > 0
        end)

    module ParserConfig = struct
      let debug = O.verbose > 1
      let libfind = ML.find
    end

    module Parser = ParseModel.Make(ParserConfig)

    let rec get_ast indent fname =
        let fname0,(_,_,ast)  = Parser.find_parse fname in
        printf "%sBEGIN %s\n" indent fname0 ;
        get_imports (indent^"  ") ast ;
        printf "%sEND   %s\n" indent fname0 ;
        ()

    and get_imports indent ast = List.iter (get_ins indent) ast

    and get_ins indent ins =
      let open AST in
      match ins with
      | Include (_,fname) -> get_ast indent fname
      | _ -> ()

    let zyva name =
      try get_ast "" name
      with
      | Misc.Fatal msg -> printf "ERROR %s\n%!" msg
      | Misc.Exit -> ()
  end

let verbose = ref 0
let includes = ref []
let arg = ref []

let setarg name = arg := !arg @ [name]

let opts =
  [
   "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
   "-I",Arg.String (fun s -> includes := !includes @ [s]),
   "<dir> add <dir> to search path";
  ]

let () =
  Arg.parse opts setarg
    (sprintf "Usage: %s [options]* cats*" prog)


module Z =
  Make
    (struct
      let verbose = !verbose
      let includes = !includes
    end)

let () = List.iter Z.zyva !arg ; exit 0
