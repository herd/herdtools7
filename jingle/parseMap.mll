{  
  exception Error of string

  type t = {
    source : Archs.t;
    target : Archs.t;
    conversions : (string * string) list
  }
}

let space = [' ' '\t' '\r']
let blank = (space | '\n')
let archname = ([ '0'-'9' 'a'-'z' 'A'-'Z'])*

rule main = parse
| space* (archname as src) space+ "to" space+ (archname as tgt) space* '\n' blank*
    { let convs = conv [] lexbuf in
      let (src,tgt) = match Archs.parse src,Archs.parse tgt with
	| Some s,Some t -> s,t
	| _ -> raise (Error "Source or target architecture unrecognized.")
      in { 
	source = src;
	target = tgt;
	conversions = List.rev convs
      }
    }
| "" {raise (Error "Source or target architecture unspecified.")}
	
and conv l = parse
    | eof {l}
    | '"' ([^'"']* as left) '"' blank* "->" blank* '"' ([^'"']* as right) '"' blank*
	{
	  conv ((String.trim left, String.trim right)::l) lexbuf
	}
    | "" {raise (Error "Bad syntax in conversion rule.")}

{
  
  let parse chin = main (Lexing.from_channel chin)
    
}
