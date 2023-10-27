(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)

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
