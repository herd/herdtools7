let () =
  let file_path = ref None in
  let usage = "Usage: litmus2desc [options] FILE" in

  let libdir = ref None in
  let describe_regs = ref false in
  let options =
    [
      ( "-set-libdir",
        Arg.String (fun s -> libdir := Some s),
        "<path> set libdir" );
      ( "--describe-regs",
        Arg.Unit (fun () -> describe_regs := true),
        "enable descriptions of register events. Default = disabled." );
    ]
  in
  let process_arg arg =
    match !file_path with
    | None -> file_path := Some arg
    | Some _ ->
        prerr_endline "Only one FILE argument is allowed";
        Arg.usage options usage;
        exit 2
  in

  Arg.parse options process_arg usage;

  let file_path =
    match !file_path with
    | Some p -> p
    | None ->
        prerr_endline "Missing FILE argument.";
        Arg.usage options usage;
        exit 2
  in

  print_endline
    (Litmus2desc.Explainer.explain_test_path ~libdir:!libdir
       ~describe_regs:!describe_regs file_path)
