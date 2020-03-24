(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2017-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)


(* Some utilities *)

module Hash(O:Warn.Config) =
  struct
    open Answer
    module W = Warn.Make(O)

    let mk_hash_info fname info =
      try
          let hash = List.assoc MiscParser.hash_key info in
          { filename=fname; hash=hash;}
      with Not_found -> assert false

    let hash_ok env tname hash =
      try
        let ohash = StringMap.find tname env in
        if String.compare hash.hash ohash.hash <> 0 then begin
          Warn.user_error "Unconsistent hashes for test %s, previous file %s"
            tname ohash.filename
        end else begin
          if  ohash.filename <> hash.filename then
            W.warn  "Duplicate occurrence of test %s (%s,%s)"
              tname ohash.filename hash.filename
          else
            W.warn "File %s is referenced more then once"
              ohash.filename
        end ;
        false
      with Not_found ->  true

  end

module Pseudo(A:Arch_litmus.S) = struct
  
  type code = MiscParser.proc * A.pseudo list

  let rec fmt_io io = match io with
  | A.Nop -> ""
  | A.Instruction ins -> A.dump_instruction ins
  | A.Label (lbl,io) -> lbl ^ ": " ^ fmt_io io
  | A.Symbolic _ -> assert false (*no symbolic in litmus *)
  | A.Macro (f,regs) ->
      Printf.sprintf
        "%s(%s)"
        f
        (String.concat "," (List.map A.pp_reg regs))

  let dump_prog (p,is) = MiscParser.pp_proc  p::List.map fmt_io is

  let dump_prog_lines prog =
    let pp = List.map dump_prog prog in
    let pp = Misc.lines_of_prog pp in
    List.map (Printf.sprintf "%s;") pp

  let print_prog chan prog =
    let pp = List.map dump_prog prog in
    Misc.pp_prog chan pp

  let rec find_code p = function
    | [] -> assert false
    | ((q,_),is)::rem ->
        if Misc.int_eq p q then is else find_code p rem

  let find_offset code p lbl =
    let is = find_code p code in
    A.find_offset lbl is
end
