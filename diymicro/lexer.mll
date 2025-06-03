{
  open Parser
  exception Eof
}

let alpha = (['A'-'Z' 'a'-'z'])
let number = ['0'-'9']
let id = (alpha | number | '_')+
let filename = id '.' id

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | "Rf" { RF }
  | "Fr" { FR }
  | "Ws" { WS }
  | "Po" { PO }
  | "Dp" { DP }
  | "iico" { IICO }

  | 'R' { R }
  | 'W' { W }
  | 'i' { INT }
  | 'e' { EXT }
  | 's' { SAME }
  | 'd' { DIFFERENT }
  | "Addr" { ADDR }
  | "Data" { DATA }
  | "Ctr"  { CTR }

  | '['     { LBRACKET }
  | ']'     { RBRACKET }
  | ':'     { COLON }
  | "->"    { ARROW }

  | filename as s { ID (s) } (* TODO: what do I need to change to use id directly ? *)
  | _ as c { Warn.fatal "unexpected character: %C" c }
  | eof    { EOF }