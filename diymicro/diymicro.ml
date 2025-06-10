let () =
  let list_iico = ref false in
  let edges_ref = ref [] in
  (* load iico edges *)
  Iico.init ();
  let parse_edge s =
    try
      edges_ref :=
        (Lexing.from_string s |> Parser.main Lexer.token) :: !edges_ref
    with Parser.Error | Not_found ->
      raise (Arg.Bad (Printf.sprintf "Unknown edge '%s'" s))
  in

  let options_list =
    [
      "-v", Arg.Set Config.verbose, "Display verbose messages";
      "--list-iico", Arg.Set list_iico, "list iico[] edges";
    ]
  in
  let usage = "diymicro [options] <edge 1> <edge 2> <...>" in
  (* message d'accueil, option -help *)

  Arg.parse options_list parse_edge usage;

  if !list_iico then Edge.list_iico_edges ()
  else if !edges_ref = [] then Arg.usage options_list usage
  else
    let edges = List.rev !edges_ref in
    let cycle = Cycle.make_cycle edges in
    let prog = Compile.make_test cycle in
    let instructions = List.map (fun (a, _) -> a) prog in
    let stl = List.map (fun (_, b) -> b) prog in
    Compile.dump_test stl instructions stdout
