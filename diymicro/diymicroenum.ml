module E = struct
  include Edge
end

let make_all_tests edge edge_name output_dir =
  let tests =
    (* TODO It would be cleaner and less bug-prone to write the edges in OCaml directly, but much more cumbersome *)
    match E.edge_direction edge with
    | E.RegEvent, E.RegEvent ->
        [
          ( "LB+rel+" ^ edge_name,
            ["DpAddrdrW"; "Rfe"; "PodRW:L"; "Rfe"; "basic_depRr"] );
          ( "MP+rel+" ^ edge_name,
            ["DpAddrdrR"; "Fre"; "PodWW:L"; "Rfe"; "basic_depRr"] );
        ]
    | E.Rm _, E.RegEvent ->
        [
          "LB+rel+" ^ edge_name, ["DpAddrdrW"; "Rfe"; "PodRW:L"; "Rfe"];
          "MP+rel+" ^ edge_name, ["DpAddrdrR"; "Fre"; "PodWW:L"; "Rfe"];
        ]
    | E.RegEvent, E.Wm _ ->
        [
          ( "LB+rel+" ^ edge_name,
            ["PosWR"; "DpAddrdW"; "Rfe"; "PodRW:L"; "Rfe"; "basic_depRr"] );
          ( "MP+rel+" ^ edge_name,
            ["PosWR"; "DpAddrdR"; "Fre"; "PodWW:L"; "Rfe"; "basic_depRr"] );
        ]
    | E.Rm _, E.Wm _ ->
        [
          ( "LB+rel+" ^ edge_name,
            ["PosWR"; "DpAddrdW"; "Rfe"; "PodRW:L"; "Rfe"; "DpDatadW"; "PosWR"]
          );
          ( "MP+rel+" ^ edge_name,
            ["PosWR"; "DpAddrdR"; "Fre"; "PodWW:L"; "Rfe"; "DpDatadW"; "PosWR"]
          );
        ]
    | dir1, dir2 ->
        Warn.fatal "make_all_tests: LB/MP not implemented for %s -> %s"
          (Edge.pp_direction dir1) (Edge.pp_direction dir2)
  in

  List.map
    (fun (test_name, edges) ->
      let cycle =
        (edge, E.AnnotNone)
        :: List.map
             (fun s -> Lexing.from_string s |> Parser.main Lexer.token)
             edges
      in
      let out_channel =
        open_out (Filename.concat output_dir (test_name ^ ".litmus"))
      in
      "Writing to " ^ test_name ^ ".litmus\n" |> Utils.verbose_print 0;
      Compile.to_channel cycle ?name:(Some test_name) out_channel;
      close_out out_channel;
      test_name)
    tests

let compile_edge_enum iico inputs outputs output_dir =
  let test_name src dst =
    String.uppercase_ascii iico.Edge.instruction_name
    ^ "-"
    ^ String.capitalize_ascii src
    ^ String.capitalize_ascii dst
  in

  List.map
    (fun (src, dst) ->
      make_all_tests
        (Edge.iico_to_edge iico src dst)
        (test_name src dst) output_dir)
    (Utils.cartesian2 inputs outputs)
  |> List.flatten
  |> List.map (fun name -> name ^ ".litmus")

let () =
  let output_dir = ref "." in
  let list_iico = ref false in
  let edges_ref = ref [] in

  (* load iico edges *)
  Iico.init ();
  let parse_edge s =
    try
      edges_ref :=
        (Lexing.from_string s |> Parser.parse_iico Lexer.token) :: !edges_ref
    with Parser.Error | Not_found ->
      raise (Arg.Bad (Printf.sprintf "Unknown edge '%s'" s))
  in

  let options_list =
    [
      ( "-output",
        Arg.Set_string output_dir,
        "Output directory (default '" ^ !output_dir ^ "')" );
      ( "-v",
        Arg.Unit (fun () -> incr Config.verbose),
        "Increase verbosity (use multiple times)" );
      "-list-iico", Arg.Set list_iico, "list iico[] edges";
      ( "-debug",
        Arg.Unit (fun () -> Printexc.record_backtrace true),
        "Print backtrace on crash" );
    ]
  in
  let usage =
    "diymicroenum [options] <iico[edge src->dst] (use 'edge' as shorthand for \
     'edge *->*')> <iico[edge2]> ..."
  in
  (* message d'accueil, option -help *)

  Arg.parse options_list parse_edge usage;

  if not (Sys.file_exists !output_dir) then Sys.mkdir !output_dir 0o775;
  if !list_iico then Edge.list_iico_edges ()
  else if !edges_ref = [] then Arg.usage options_list usage
  else
    let channel_all = open_out (Filename.concat !output_dir "@all") in
    "# " ^ Config.prog_name ^ " "
    ^ (Sys.argv |> Array.to_list |> List.tl |> String.concat " ")
    ^ "\n"
    |> output_string channel_all;

    List.map
      (fun (iico, inputs, outputs) ->
        compile_edge_enum iico inputs outputs !output_dir)
      !edges_ref
    |> List.flatten |> String.concat "\n" |> output_string channel_all;

    output_string channel_all "\n";
    close_out channel_all
