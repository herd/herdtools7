(*
   Taken from https://gitlab.inria.fr/fpottier/menhir/-/blob/master/demos/calc/lexer.mll
   Edited to simplify the lexer further to tokenization functions only.
   menhir2bnfc could be imporved to handle this, but it's out of scope at the moment
*)
{
  open Parser

  exception Error of string

}

(* ==== Unhandled by menhir2bnfc

(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)

rule line = parse
| ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
| eof
    (* Normal case: no data, eof. *)
    { None, false }
| ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }

 ==== *)

(* This rule analyzes a single line and turns it into a stream of
   tokens. *)

rule token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOL }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIV }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
| eof
    { raise (Error (Printf.sprintf "At offset %d: unexpected end of input.\n" (Lexing.lexeme_start lexbuf))) }
