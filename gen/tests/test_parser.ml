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
  Printf.printf "%s\n" (Ast.pp Fun.id ast) ;
  Ast.expand ast
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

(*
module DefaultConfig = struct
  let verbose = 0
  let generator = "test_parser"
  let debug = Debug_gen.none
  let hout = Hint.none
  let cond = Config.Cycle
  let neg = false
  let nprocs = 2
  let eprocs = false
  let do_observers = Config.Avoid
  let obs_type = Config.Straight
  let optcond = true
  let overload = None
  let poll = false
  let optcoherence = false
  let docheck = false
  let typ = TypBase.Int
  let hexa = false
  let variant _ = false
  let cycleonly = false
  let metadata = false
  let show = None
  let same_loc = false
  let unrollatomic = None
  let allow_back = false
  let moreedges = false
  let realdep = false
  let wildcard = true
  let family = None
  let canonical_only = false
  let fmt = 2
  let no = []
  let tarfile = None
  let sufname = None
  let addnum = false
  let numeric = false
  let lowercase = false
  let cpp = false
  let scope = Scope.No
  let info = ([] : MiscParser.info)
  let stdout = true
  let choice = Code.Default
  let prefix = []
  let cumul = Config.Empty
  let max_ins = 2
  let upto = true
  let varatom = []
end

module TestAArch64Builder =
  Top_gen.Make(DefaultConfig)(AArch64Compile_gen.Make(DefaultConfig))

module TestAArch64Diy = Diy.Make(TestAArch64Builder)(DefaultConfig)

let () = ignore TestAArch64Diy.parse_argument_list
*)
