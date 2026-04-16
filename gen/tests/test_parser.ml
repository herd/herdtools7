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
      Printf.printf "%s\n" input ;
      List.iter
        (fun (label, is_backward_compatible) ->
          let ast =
            Lexing.from_string input
            |> LexUtil.parse ~is_backward_compatible Parser.main in
          Printf.printf "%s\n" label ;
          Printf.printf "%s\n" (Ast.pp Fun.id ast) ;
          Ast.expand ast
          |> pp_list (pp_list Fun.id)
          |> Printf.printf "%s\n" )
        [
          "backward_compatibility=true", true;
          "backward_compatibility=false", false;
        ] ;
      Printf.printf "\n")
    test_cases
