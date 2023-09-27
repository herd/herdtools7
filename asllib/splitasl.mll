(******************************************************************************)
(*                                ASLRef                                      *)
(*                                                                            *)
(* Copyright (c) 2022-present, Arm Limited or its affiliates.                 *)
(* All rights reserved.                                                       *)
(*                                                                            *)
(* SPDX-License-Identifier: Apache-2.0                                        *)
(******************************************************************************)
(* Authors:                                                                   *)
(* Hadrien Renaud, University College London, UK.                             *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)
{
let dump _n start k =
  start,
  String.concat ""
    (List.rev_map (fun line -> Printf.sprintf "%s\n" line) k)

let get_lnum lb = lb.Lexing.lex_curr_p.Lexing.pos_lnum
}

rule main n st k = parse
| "//" ' '+ '='+ '\n'
    { let new_st = get_lnum lexbuf in
      Lexing.new_line lexbuf ;
      let  chunk = dump n st k in
      fun () -> Seq.Cons (chunk,main (n+1) new_st [""] lexbuf)
    }
| "//" [^'\n']* '\n'  { Lexing.new_line lexbuf ; main n st (""::k) lexbuf }
| [^'\n']* as line '\n' {  Lexing.new_line lexbuf ; main n st (line::k) lexbuf }
| [^'\n']* as line eof
    {
     let k =
       match line with
       | "" -> k
       | _ -> line::k in
     match k with
     | [] -> Seq.empty
     | _::_ -> Seq.return (dump n st k)
   }


{

 let split lexbuf = main 1 (get_lnum lexbuf) [] lexbuf

}
