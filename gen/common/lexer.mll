{
open Parser
exception Eof
(* if there is a relexation before,
   the blank will be treated as comma, hence sequence *)
let has_previous_relaxation = ref [false]
let push t stack = stack := (t :: !stack)
let pop stack = match !stack with
      | [] -> Warn.fatal "error in has_previous_relaxation in lexer.\n"
      | hd :: tail -> stack := tail; hd
let peak stack = match !stack with
      | [] -> Warn.fatal "error in has_previous_relaxation in lexer.\n"
      | hd :: _ -> hd
let modify t stack = match !stack with
      | [] -> Warn.fatal "error in has_previous_relaxation in lexer.\n"
      | _ :: tail -> stack := t :: tail
}

let blank = [' ''\t''\n''\r']
let relexation = ['A'-'Z' 'a'-'z' '0'-'9' '-' '.' '*']+

rule token = parse
| eof { EOF }
(* - operands consumes necessary blanks
   - `[....]` create a seperate scope
     hence a separate `has_previous_relaxation`. *)
| '[' blank* { push false has_previous_relaxation; LEFT_SQUIRE }
| blank* ']' {
  ignore (pop has_previous_relaxation);
  modify true has_previous_relaxation;
  RIGHT_SQUIRE
}
| blank* ',' blank* { COMMA }
| blank* '|' blank* { CHOICE_BAR }
| (relexation as lxm) {
  modify true has_previous_relaxation;
  RELAXATION lxm
}
| blank+ {
  if peak has_previous_relaxation then COMMA
  else token lexbuf
}
