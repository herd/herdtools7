open Lexing

type 't with_pos = 't * position * position
type 't state = 't with_pos array * int ref
type 't lexer = lexbuf -> 't
type 't supplier = unit -> 't with_pos

let get_unsafe ((tokens, i) : 't state) : 't with_pos =
  let res = tokens.(!i) in
  incr i;
  res

let get ((tokens, i) : 't state) : 't with_pos =
  if !i < Array.length tokens then get_unsafe (tokens, i)
  else raise (Invalid_argument "empty lexer")

let of_lexer_lexbuf (is_eof : 't -> bool) (lexer : 't lexer) (lexbuf : lexbuf) :
    't state =
  let rec loop acc =
    let tok = lexer lexbuf in
    let p1 = lexbuf.lex_start_p and p2 = lexbuf.lex_curr_p in
    let acc = (tok, p1, p2) :: acc in
    if is_eof tok then acc |> List.rev |> Array.of_list else loop @@ acc
  in
  (loop [], ref 0)

let to_lexer (state : 't state) : 't lexer =
 fun lexbuf ->
  let tok, p1, p2 = get state in
  lexbuf.lex_start_p <- p1;
  lexbuf.lex_curr_p <- p2;
  tok

let to_supplier (state : 't state) : 't supplier = fun () -> get state
let copy ((tokens, i) : 't state) : 't state = (tokens, ref !i)

let double_lexer is_eof lexer lexbuf : 't lexer * 't lexer =
  let init_state = of_lexer_lexbuf is_eof lexer lexbuf in
  (to_lexer (copy init_state), to_lexer (copy init_state))
