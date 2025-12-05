(* Create a set of relaxations for diy using a cat file *)

module Opts = struct
  type t = {
    log_level : Logs.level;
    lets_to_print : string list;
    variants : string list;
    unroll : int;
    libdir : string option;
    dump : StringSet.t;
    log : (string * Logs.level option) list;
    conf : bool;
  }

  let valid_dump_opts = [ "tree"; "origin" ]

  let parse_dump_opt (opts : StringSet.t) (s : string) : StringSet.t =
    if List.mem s valid_dump_opts then StringSet.add s opts
    else raise (Arg.Bad "Wrong value for --dump")

  let should_dump_tree (o : t) : bool = StringSet.mem "tree" o.dump
  let should_dump_origin (o : t) : bool = StringSet.mem "origin" o.dump

  let parse_log_opt : string -> string * Logs.level option =
    let level_of_string s : Logs.level option =
      match String.lowercase_ascii s with
      | "debug" -> Some Logs.Debug
      | "info" -> Some Logs.Info
      | "warn" | "warning" -> Some Logs.Warning
      | "error" -> Some Logs.Error
      | "none" -> None
      | _ ->
          let err = Format.sprintf "-log: unrecognized level `%s`" s in
          raise (Arg.Bad err)
    in
    fun source ->
      match String.split_on_char ':' (String.trim source) with
      | [ src ] ->
          let src = String.trim src in
          (src, Some Logs.Info)
      | [ src; lvl ] ->
          let src = String.trim src in
          let lvl = String.trim lvl in
          (src, level_of_string lvl)
      | _ -> raise (Arg.Bad "Wrong value for --log")

  let parse () : t * string =
    let log_level = ref Logs.Error in
    let lets_to_print = ref [] in
    let variants = ref [] in
    let unroll = ref 1 in
    let file_paths = ref [] in
    let add_file_path fp = file_paths := !file_paths @ [ fp ] in
    let libdir = ref None in
    let dump = ref StringSet.empty in
    let log = ref [] in
    let conf = ref false in
    let valid_srcs = Logs.Src.list () |> List.map Logs.Src.name in
    let log_opt_msg =
      Format.sprintf "<%s>  Fine-grained logging control for specific modules."
        (String.concat "|" valid_srcs)
    in
    let opts =
      [
        ("-v", Arg.Unit (fun () -> log_level := Logs.Info), "  Be verbose.");
        ( "--let",
          Arg.String (fun s -> lets_to_print := !lets_to_print @ [ s ]),
          "<str>  Print out selected let statements." );
        ( "--variant",
          Arg.String (fun s -> variants := !variants @ [ s ]),
          "<str>  Enable variant. Can be set multiple times." );
        ( "--unroll",
          Arg.Int (fun i -> unroll := i),
          "<n>  Specify how many times reflexive and transitive closure \
           operators should be unrolled. Default = 1." );
        ( "--set-libdir",
          Arg.String (fun s -> libdir := Some s),
          "<path>  Set location of libdir to <path>." );
        ( "--dump",
          Arg.String (fun s -> dump := parse_dump_opt !dump s),
          Format.sprintf "<%s>  Dump info on parsed model."
            (String.concat "|" valid_dump_opts) );
        ( "--log",
          Arg.String (fun s -> log := parse_log_opt s :: !log),
          log_opt_msg );
        ( "--conf",
          Arg.Unit (fun () -> conf := true),
          " Causes the tool to print its output in diy7 configuration file \
           format." );
      ]
    in
    let prog =
      if Array.length Sys.argv > 0 then Filename.basename Sys.argv.(0)
      else "cat2config7"
    in
    let () =
      Arg.parse opts add_file_path
        (Format.sprintf "Usage: %s [OPTIONS] FILE" prog)
    in
    let file_path =
      match !file_paths with
      | [ fp ] -> fp
      | _ -> raise (Arg.Bad "Must provide exactly one input file")
    in
    let opts =
      {
        log_level = !log_level;
        lets_to_print = !lets_to_print;
        variants = !variants;
        unroll = !unroll;
        libdir = !libdir;
        dump = !dump;
        log = !log;
        conf = !conf;
      }
    in
    (opts, file_path)
end

module Make_parser (O : sig
  val debug : bool
  val libdir : string option
end) =
struct
  module ML = MyLib.Make (struct
    let includes = []
    let env = Some "HERDLIB"

    let libdir =
      match O.libdir with
      | Some libdir -> libdir
      | None -> Filename.concat Version.libdir "herd"

    let debug = O.debug
  end)

  module ParserConfig = struct
    let debug = false
    let libfind = ML.find
  end

  module Parser = ParseModel.Make (ParserConfig)

  let get_includes : AST.ins list -> string list =
    List.filter_map (function
      | AST.Include (_, fname) -> Some fname
      | _ -> None)

  (* Parse a cat model by file path, recursively following included iles. *)
  let rec find_parse_deep (file_path : string) : AST.ins list =
    let _, (_, _, ast) = Parser.find_parse file_path in
    let includes = get_includes ast in
    let included_asts = Util.List.concat_map find_parse_deep includes in
    included_asts @ ast
end

module Nf = Normalization.Make (Ir)

let extract_let_binding (ins : AST.ins) :
    (string * Normalization.binding) option =
  let open AST in
  match ins with
  | Rec (_, (_, Pvar (Some name), body) :: _, _) ->
      Some (name, { body; is_recursive = true })
  | Let (_, (_, Pvar (Some name), body) :: _) ->
      Some (name, { body; is_recursive = false })
  | _ -> None

let run ~(opts : Opts.t) (tree : AST.ins list) =
  let bindings = List.filter_map extract_let_binding tree in
  let norm_config : Normalization.config =
    { variants = opts.variants; unroll_depth = opts.unroll }
  in
  let nf_map = Nf.normalize_bindings ~config:norm_config bindings in
  let requested_bindings :
      (string * (Translation.relax list * AST.exp) list) list =
    opts.lets_to_print
    |> List.filter_map (fun var ->
        match Nf.find_opt var nf_map with
        | None ->
            Logs.err (fun m -> m "Requested let binding `%s` not found.@." var);
            None
        | Some [] ->
            Logs.err (fun m -> m "Failed to evaluate let binding `%s`.@." var);
            None
        | Some nfs ->
            (if Opts.should_dump_tree opts then
               let compressed =
                 Ir.rel_union_l (List.map fst nfs) |> Ir.compress
               in
               Format.printf "(%s)@.  %a@." var Ir.pp_rel_nf compressed);
            let translated =
              nfs
              |> List.map (fun (nf, ast_e) ->
                  let relaxs = Translation.translate ~binding:var nf in
                  (relaxs, ast_e))
            in
            Some (var, translated))
  in
  if opts.conf then
    let open Format in
    requested_bindings
    |> List.iteri (fun i (var, relaxss) ->
        if i <> 0 then printf "@.";
        printf "### %s@." var;
        let _ =
          List.fold_left
            (fun acc (relaxs, ast_e) ->
              let print_cat_rel () =
                if Opts.should_dump_origin opts then
                  printf "## %a@." AstUtils.pp_exp ast_e
              in
              let relaxs = List.filter (fun r -> not (List.mem r acc)) relaxs in
              if relaxs <> [] then (
                print_cat_rel ();
                printf "-safe %s@."
                  (String.concat " " (List.map Translation.pp_relax relaxs)));
              acc @ relaxs)
            [] relaxss
        in
        ())
  else
    requested_bindings
    |> List.iter (fun (_, relaxss) ->
        relaxss
        |> List.iter (fun (relaxs, _) ->
            relaxs
            |> List.iter (fun relax ->
                Format.printf "%s@." (Translation.pp_relax relax))))

let () =
  Logs.set_reporter (Logs.format_reporter ());
  let opts, file_path =
    try Opts.parse ()
    with Arg.Bad msg ->
      Logs.err (fun m -> m "%s" msg);
      exit 1
  in
  Logs.set_level (Some opts.log_level);
  Logs.Src.list ()
  |> List.iter (fun src ->
      let src_name = Logs.Src.name src in
      match List.assoc_opt src_name opts.log with
      | Some lvl -> Logs.Src.set_level src lvl
      | None -> ());
  let module Parser = Make_parser (struct
    let debug = false
    let libdir = opts.libdir
  end) in
  let tree = Parser.find_parse_deep file_path in
  run ~opts tree
