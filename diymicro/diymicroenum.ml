module E = struct
  include Edge
end

let make_all_tests edge base_filename output_dir =
  let tests =
    (* TODO Should we write the edges in OCaml directly ? *)
    match E.edge_direction edge with
    | E.RegEvent, E.RegEvent ->
        [
          ( "LB+rel+" ^ base_filename,
            ["DpAddrdrW"; "Rfe"; "PodRW:L"; "Rfe"; "Rf-reg"] );
          ( "MP+rel+" ^ base_filename,
            ["DpAddrdrR"; "Fre"; "PodWW:L"; "Rfe"; "Rf-reg"] );
        ]
    | E.Rm _, E.RegEvent ->
        [
          "LB+rel+" ^ base_filename, ["DpAddrdrW"; "Rfe"; "PodRW:L"; "Rfe"];
          "MP+rel+" ^ base_filename, ["DpAddrdrR"; "Fre"; "PodWW:L"; "Rfe"];
        ]
    | E.RegEvent, E.Wm _ ->
        [
          ( "LB+rel+" ^ base_filename,
            ["PosWR"; "DpAddrdW"; "Rfe"; "PodRW:L"; "Rfe"; "Rf-reg"] );
          ( "MP+rel+" ^ base_filename,
            ["PosWR"; "DpAddrdR"; "Fre"; "PodWW:L"; "Rfe"; "Rf-reg"] );
        ]
    | E.Rm _, E.Wm _ ->
        [
          ( "LB+rel+" ^ base_filename,
            ["PosWR"; "DpAddrdW"; "Rfe"; "PodRW:L"; "Rfe"; "DpDatadW"; "PosWR"]
          );
          ( "MP+rel+" ^ base_filename,
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
  let base_filename src dst =
    (* INSTR-SrcDst *)
    String.uppercase_ascii iico.Edge.instruction_name
    ^ "-"
    ^ String.capitalize_ascii src
    ^ String.capitalize_ascii dst
  in

  List.map
    (fun (src, dst) ->
      make_all_tests
        (Edge.iico_to_edge iico src dst)
        (base_filename src dst) output_dir)
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
      "-list-iico", Arg.Set list_iico, "List iico[] edges";
      ( "-debug",
        Arg.Unit (fun () -> Printexc.record_backtrace true),
        "Print backtrace on crash" );
    ]
  in
  (* message d'accueil, option -help *)
  let usage =
    "diymicroenum [options] <iico[instr src->dst] (use 'instr' as shorthand \
     for 'instr *->*')> <iico[instr2]> ..."
  in

  Arg.parse options_list parse_edge usage;

  if !list_iico then Edge.list_iico_edges ()
  else if !edges_ref = [] then Arg.usage options_list usage
  else (
    if not (Sys.file_exists !output_dir) then Unix.mkdir !output_dir 0o775;
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
    close_out channel_all)
