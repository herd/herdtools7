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
    "A B C";
    "[A,[B]]";
    "A,[B,C]";
    "[A,[B,C]]";
    "@before(A)";
    "A @after(B)";
    "[A @before(B)]";
    "@before(A) @before(B)";
    "A @after(B) @after(C)";
    "@before([A B])";
    "@after([A B])";
  ]

let unit_test parser_grammar label input =
  Printf.printf "%s: %s\n" label input ;
  try
    let ast =
      Lexing.from_string input
      |> LexUtil.parse parser_grammar in
    Printf.printf "%s\n" (Ast.pp Fun.id Fun.id ast) ;
    Ast.expand ( fun pred prim -> Printf.sprintf "@%s(%s)" pred prim ) ast
    |> pp_list (pp_list Fun.id)
    |> Printf.printf "%s\n"
  with Parser.Error ->
    Printf.printf "Parser.Error\n"

let parser_configs = [
  "diyone7 parser", Parser.main;
  "diycross7 parser", Parser.diycross7;
  "diy7 parser", Parser.diy7;
  "cumul parser (for -cumul in diy7)", Parser.cumul;
]

let () =
  List.iter
    (fun input ->
      List.iter
        (fun (label, parser_grammar) ->
          unit_test parser_grammar label input)
        parser_configs ;
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
      unit_test Parser.diycross7 "equivalent syntax test for diycross7" input;
      unit_test Parser.diy7 "equivalent syntax test for diy7" input;
      Printf.printf "\n")
    equivalent_syntax_tests

module TestGeneratorConfig = struct
  let verbose = 0
  let generator = "test_parser"
  let debug = Debug_gen.none
  let hout = Hint.none
  let family : string option = None
  let canonical_only = true
  let fmt = 3
  let no : string list = []
  let cond = Config.Cycle
  let tarfile : string option = None
  let sufname : string option = None
  let addnum = true
  let numeric = true
  let lowercase = false
  let stdout = true
  let cycleonly = false
  let metadata = true
  let choice = Code.Default
  let prefix : string list = []
  let variant (_ : Variant_gen.t) = false
  let upto = true
  let varatom : string list = []
  let max_ins = 4
  let overload : int option = None
  let poll = false
  let optcoherence = false
  let optcond = true
  let obs_type = Config.Straight
  let do_observers = Config.Avoid
  let eprocs = false
  let nprocs = 4
  let neg = false
  let cpp = false
  let scope = Scope.No
  let info : MiscParser.info = []
  let docheck = false
  let typ = TypBase.default
  let hexa = false
  let same_loc = false
  let mix = false
  let max_relax = 100
  let min_relax = 1
end

module TestCompileConfig = struct
  let verbose = TestGeneratorConfig.verbose
  let show : ShowGen.t option = None
  let same_loc = TestGeneratorConfig.same_loc
  let unrollatomic : int option = None
  let allow_back = false
  let typ = TestGeneratorConfig.typ
  let hexa = TestGeneratorConfig.hexa
  let moreedges = false
  let realdep = false
  let variant = TestGeneratorConfig.variant
  let wildcard = true
end

module TestBuilder =
  Top_gen.Make(TestGeneratorConfig)(AArch64Compile_gen.Make(TestCompileConfig))

module TestAltConfig = struct
  include TestGeneratorConfig

  let cumul = Config.All
  let wildcard = true

  type fence = TestBuilder.A.fence
end

let remove_invalid_relaxes_inputs_main = [
  "[Po,Rfe]";
  "[Rfe,Rfe]";
  "[Rfe,Fre]";
]

let remove_invalid_relaxes_test_main input =
  Printf.printf "remove_invalid_relaxes test (AArch64): %s\n" input ;
  let ast = TestBuilder.R.parse_ast Parser.main input in
  let filtered =
    TestBuilder.R.parse_expand_relaxs ast
    |> TestBuilder.R.remove_invalid_relaxes in
  Printf.printf "%s\n" (pp_list TestBuilder.R.pp_relax filtered)

let remove_invalid_relaxes_inputs_diy7 = [
  "[PodRW @before(Rfe)]";
  "[@after(PodRW) Rfe]";
  "[PodRW @before(Rfe) @before(Fre)]";
  "[@after(PodRW) @after(Rfe) Fre]";
  "[PodRW @after(Rfe) Fre]";
  "[PodRW @before([Rfe Fre])]";
  "[@before([Rfe PodRW])]";
]

module TestAlt = Alt.Make(TestBuilder)(TestAltConfig)

let remove_invalid_relaxes_test_diy7 input =
  Printf.printf "remove_invalid_relaxes test (AArch64): %s\n" input ;
  let filtered =
    TestAlt.parse_argument input
    |> TestAlt.remove_invalid_relaxes in
  let filtered = List.map TestAlt.plain filtered in
  Printf.printf "%s\n" (pp_list TestBuilder.R.pp_relax filtered)

let () =
  List.iter
    (fun input ->
      remove_invalid_relaxes_test_main input;
      Printf.printf "\n")
    remove_invalid_relaxes_inputs_main ;
  List.iter
    (fun input ->
      remove_invalid_relaxes_test_diy7 input;
      Printf.printf "\n")
    remove_invalid_relaxes_inputs_diy7
