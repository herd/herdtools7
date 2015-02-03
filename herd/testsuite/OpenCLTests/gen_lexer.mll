{
open Gen_parser
open Gen_ast
}

rule token = parse
| 'a' { ATOMICITY true }
| 'p' { ATOMICITY false }
| "GL" { REGION Both }
| 'G'  { REGION Global }
| 'L'  { REGION Local }
| "na"  { MODE NA }
| "rlx" { MODE Rlx }
| "acq" { MODE Acq }
| "rel" { MODE Rel }
| "ar"  { MODE AR }
| "sc"  { MODE SC }
| 'B'   { B }
| 'F'   { F }
| 'x'   { X }
| 'y'   { Y }
| 'z'   { Z }
| "dev" { SCOPE S_dev }
| "wg"  { SCOPE S_wg }
| ['0'-'9']+ as x { PROC (int_of_string x) }
| '|' { SEP }
| "||" { SEPSEP }
| '_'  { USCORE }
| '-'  { DASH }
| '['  { LBRK }
| ']'  { RBRK }
| eof { EOF }
| _ as x { failwith (Printf.sprintf "Unknown token %c" x) }
