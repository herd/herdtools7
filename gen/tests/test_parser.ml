let pp_list pp xs =
  Printf.sprintf "[%s]" (String.concat ";" (List.map pp xs))

let parser_tests =
  [
    "A|B,C|[D,[E,F]?,G]?";
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

let unit_test parser_grammar label input =
  Printf.printf "%s: %s\n" label input ;
  let ast =
    Lexing.from_string input
    |> LexUtil.parse parser_grammar in
  Printf.printf "%s\n" (Ast.pp Fun.id Fun.id ast) ;
  Ast.expand ( fun pred prim -> Printf.sprintf "@%s(%s)" pred prim ) ast
  |> pp_list (pp_list Fun.id)
  |> Printf.printf "%s\n"

let () =
  List.iter
    (fun input ->
      List.iter
        (fun (label, parser_grammar) ->
          unit_test parser_grammar label input
          )
        [
          "default parser (for diyone7)", Parser.main;
          "top level choice parser (for diycross7 and diy7)", Parser.main_top_level_choice;
        ] ;
      Printf.printf "\n")
    parser_tests

let equivalent_syntax_tests = [
  "A [B C] D";
  "A,[B C] D";
  "A [B C] D";
  "A,[B,C] D";
  "A [B C],D";
  "A,[B C],D";
  "A [B,C],D";
  "A,[B,C],D";
]

let () =
  List.iter
    (fun input ->
      unit_test Parser.main_top_level_choice "equivalent syntax test for (diycross7 and diy7)" input;
      Printf.printf "\n")
    equivalent_syntax_tests

let cumul_tests = [
  "A B";
  "A,B";
  "[A,B]";
  "[A,[B]]";
  "A B C";
  "A,[B,C]";
  "[A,[B,C]]";
]

let () =
  List.iter
    (fun input ->
      unit_test Parser.cumul "cumul parser (for -cumul in diy7)" input;
      Printf.printf "\n")
    cumul_tests

module TestAArch64 = AutoArch.Make(AArch64Arch_gen.Make(AArch64Arch_gen.Config))

let remove_invalid_relaxes_inputs = [
  "[Po,Rfe]";
  "[Rfe,Rfe]";
  "[Rfe,Fre]";
]

let remove_invalid_relaxes_test input =
  Printf.printf "remove_invalid_relaxes test (AArch64): %s\n" input ;
  let ast = TestAArch64.R.parse_ast Parser.main input in
  let filtered =
    TestAArch64.R.parse_expand_relaxs ast
    |> TestAArch64.R.remove_invalid_relaxes in
  Printf.printf "%s\n" (pp_list TestAArch64.R.pp_relax filtered)

let () =
  List.iter
    (fun input ->
      remove_invalid_relaxes_test input;
      Printf.printf "\n")
    remove_invalid_relaxes_inputs
