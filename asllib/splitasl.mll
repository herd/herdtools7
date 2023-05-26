(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2023-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
let dump _n k =
  String.concat ""
    (List.rev_map (fun line -> Printf.sprintf "%s\n" line) k)
}

rule main n k = parse
| "//" ' '+ '='+ '\n'    
    {
      let  chunk = dump n k in
      fun () -> Seq.Cons (chunk,main (n+1) [] lexbuf)
    }
| "//" [^'\n']* '\n'  { main n k lexbuf }
| [^'\n']* as line '\n' { main n (line::k) lexbuf }
| [^'\n']* as line eof
    {
     let k =
       match line with
       | "" -> k
       | _ -> line::k in
     match k with
     | [] -> Seq.empty
     | _::_ -> Seq.return (dump n k)
   }


{

 let split lexbuf = main 1 [] lexbuf

}
