
let () =
  let edges_ref = ref [] in
  let parse_edge s =
    try edges_ref := (Lexing.from_string s |> Parser.main Lexer.token)::!edges_ref
    with Parser.Error -> failwith ("Invalid edge '"^s^"'") in

  let options_list = [
   ("-v",Arg.Set Config.verbose, "Display verbose messages")
  ] in
  let usage = "Bienvenue à bord." in  (* message d'accueil, option -help *)

  Arg.parse options_list parse_edge usage;

  let edges = List.rev !edges_ref in
  let _ = Cycle.make edges in ()
