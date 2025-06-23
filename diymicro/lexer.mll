{
  open Parser
  exception Eof
}

let alpha = (['A'-'Z' 'a'-'z'])
let number = ['0'-'9']
let id = [^']' '>' ' ']+

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | "Rf" { RF }
  | "Fr" { FR }
  | "Ws" { WS }
  | "Po" { PO }
  | "Dp" { DP }
  | "Rf-reg" { RF_REG }
  | "iico" { IICO }

  | 'R' { RM }
  | 'W' { WM }
  | "r" { R  }
  | 'i' { INT }
  | 'e' { EXT }
  | 's' { SAME }
  | 'd' { DIFFERENT }
  | "Addr" { ADDR }
  | "Data" { DATA }
  | "Ctrl" { CTRL }
  
  | ':' { COLON }
  | 'A' { A }
  | 'L' { L }
  | 'X' { X }

  | '[' (id as s) ' ' (id as src) "->" (id as dst) ']' { IICO_ARGS (s, src, dst) }
  | '[' (id as s) ']'                                  { IICO_ARGS (s, "*", "*")   }
  | _ { raise Error }
  | eof    { EOF }