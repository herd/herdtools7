{
  open Parser
  exception Eof
}

let id = [^']' '>' ' ']+

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | "Rf" { RF }
  | "Fr" { FR }
  | "Ws" { WS }
  | "Po" { PO }
  | "Dp" { DP }
  | "DMB"    { DMB }
  | "Rf-reg" { RF_REG }
  | "iico"   { IICO }

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

  | "LD"   { LD }
  | "ST"   { ST }
  
  | '.' { DOT }
  | ':' { COLON }
  | 'A' { A }
  | 'L' { L }
  | 'X' { X }

  | '[' (id as s) ' ' (id as src) "->" (id as dst) ']' { IICO_ARGS (s, src, dst) }
  | '[' (id as s) ']'                                  { IICO_ARGS (s, "*", "*")   }
  | _ { raise Error }
  | eof    { EOF }