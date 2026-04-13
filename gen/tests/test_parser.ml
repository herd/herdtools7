let pp_list pp xs =
  Printf.sprintf "[%s]" (String.concat ";" (List.map pp xs))

let test_cases =
  [
    "A|B,C|[D,E]?";
    "A|B?";
    "A|B,C";
    "[A|B],C";
    "A,B?";
    "A,[B,C]";
    "[A|B]?";
    "[A,B]?";
    "[A B]";
    "[A,B]";
    "[A B C]";
    "[A,B,C]";
    "[A,B C]";
    "[A B,C]";
    "[A|B|C]";
    "[A|B C]";
    "A,";
  ]

let () =
  List.iter
    (fun input ->
      let ast =
        Lexing.from_string input
        |> LexUtil.parse Parser.main in
      Printf.printf "%s\n" input ;
      Printf.printf "%s\n" (Ast.pp Fun.id ast) ;
      Ast.to_list ast
      |> pp_list (pp_list Fun.id)
      |> Printf.printf "%s\n" ;
      Printf.printf "\n")
    test_cases
