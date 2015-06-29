include CBase


type substitution = 
  | Reg of string * reg
  | Cst of string * int
  | Lab of string * string
  | Addr of string * string
