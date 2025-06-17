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
      ( "-v",
        Arg.Unit (fun () -> incr Config.verbose),
        "Increase verbosity (use multiple times)" );
      "-list-iico", Arg.Set list_iico, "list iico[] edges";
      ( "-debug",
        Arg.Unit (fun () -> Printexc.record_backtrace true),
        "Print backtrace on crash" );
    ]
  in
  let usage = "diymicro [options] <edge 1> <edge 2> <...>" in
  (* message d'accueil, option -help *)

  Arg.parse options_list parse_edge usage;

  if !list_iico then Edge.list_iico_edges ()
  else if !edges_ref = [] then Arg.usage options_list usage
  else
    let annot_edges = List.rev !edges_ref in
    Compile.to_channel annot_edges stdout
