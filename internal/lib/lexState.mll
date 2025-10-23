{

}

let eq_item = [^'='';'' ']+ '=' [^';']+ ';'

let fault_item = '~'?  (['f''F'] "ault") [^';']+ ';'

rule state = parse
  | ' '+ { state lexbuf }
  | (eq_item|fault_item) as item { item::state lexbuf }
  | eof { [] }
  | ""  { raise Exit }

{
  let as_state line =
    try
      state (Lexing.from_string line)
    with Exit -> []
}
