[@@@warning "-40-42"]

(* Create a set of relaxations for diy using a cat file *)

module Arg = struct
  type opts = {
    log_level : Logs.level;
    lets_to_print : string list;
    conds : string list;
    unroll : int;
    libdir : string option;
    dump : StringSet.t;
    log : (string * Logs.level option) list;
    conf : bool;
  }

  let valid_dump_opts = [ "tree"; "origin" ]

  let parse_dump_opt (opts : StringSet.t) (s : string) : StringSet.t =
    if List.mem s valid_dump_opts then StringSet.add s opts
    else raise (Arg.Bad "Wrong value for -dump")

  let should_dump_tree (o : opts) : bool = StringSet.mem "tree" o.dump
  let should_dump_origin (o : opts) : bool = StringSet.mem "origin" o.dump

  let parse_log_opt : string -> (string * Logs.level option) list =
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
    fun input ->
      String.split_on_char ',' input
      |> List.map (fun source ->
          match String.split_on_char ':' (String.trim source) with
          | [ src ] ->
              let src = String.trim src in
              (src, Some Logs.Info)
          | [ src; lvl ] ->
              let src = String.trim src in
              let lvl = String.trim lvl in
              (src, level_of_string lvl)
          | _ -> raise (Arg.Bad "Wrong value for -log"))

  let parse : unit -> opts * string list =
   fun () ->
    let log_level = ref Logs.Error in
    let lets_to_print = ref [] in
    let conds = ref [] in
    let unroll = ref 1 in
    let file_paths = ref [] in
    let add_file_path fp = file_paths := !file_paths @ [ fp ] in
    let libdir = ref None in
    let dump = ref StringSet.empty in
    let log = ref [] in
    let conf = ref false in
    let opts =
      [
        ("-v", Arg.Unit (fun () -> log_level := Logs.Info), " be verbose");
        ( "-let",
          Arg.String (fun s -> lets_to_print := !lets_to_print @ [ s ]),
          "<statement> print out selected let statements" );
        ( "-conds",
          Arg.String (fun s -> conds := !conds @ [ s ]),
          "<cond> choose what variant conditions to set" );
        ( "-unroll",
          Arg.Int (fun i -> unroll := i),
          "<unroll> choose how many times reflexive and transitive operators \
           unroll. Default = 1" );
        ( "-set-libdir",
          Arg.String (fun s -> libdir := Some s),
          "<path> set location of libdir to <path>" );
        ( "-dump",
          Arg.String (fun s -> dump := parse_dump_opt !dump s),
          Format.sprintf "<%s> dump info on parsed model"
            (String.concat "|" valid_dump_opts) );
        ( "-log",
          Arg.String (fun s -> log := parse_log_opt s),
          "<src1,src2,...> fine-grained logging control for specific modules" );
        ( "-conf",
          Arg.Bool (fun b -> conf := b),
          "<true|false> print output as diy configuration file. Default = \
           false." );
      ]
    in
    let prog =
      if Array.length Sys.argv > 0 then Filename.basename Sys.argv.(0)
      else "cat2config7"
    in
    let () =
      Arg.parse opts add_file_path
        (Format.sprintf "Usage: %s [options]* cats*" prog)
    in
    let opts =
      {
        log_level = !log_level;
        lets_to_print = !lets_to_print;
        conds = !conds;
        unroll = !unroll;
        libdir = !libdir;
        dump = !dump;
        log = !log;
        conf = !conf;
      }
    in
    (opts, !file_paths)
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
    let included_asts = List.concat_map find_parse_deep includes in
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

let run ~(opts : Arg.opts) (tree : AST.ins list) =
  let bindings = List.filter_map extract_let_binding tree in
  let norm_config : Nf.config =
    {
      conditions = opts.conds;
      unroll_depth = opts.unroll;
      set_var = Ir.parse_set_id;
      rel_var = Ir.parse_rel_id;
    }
  in
  let nf_map = Nf.normalize_bindings ~config:norm_config bindings in
  let requested_bindings =
    opts.lets_to_print
    |> List.filter_map (fun var ->
        match StringMap.find_opt var nf_map with
        | None ->
            Logs.err (fun m -> m "Requested let binding `%s` not found.@." var);
            None
        | Some [] ->
            Logs.err (fun m -> m "Failed to evaluate let binding `%s`.@." var);
            None
        | Some nfs ->
            if Arg.should_dump_tree opts then (
              let compressed = Ir.union_l (List.map fst nfs) |> Ir.compress in
              Format.printf "(%s)@.  %a@." var Ir.pp_rel_nf compressed;
              ());
            let nfs =
              nfs
              |> List.map (fun (nf, ast_expr) ->
                  (* Format.printf "Base: %a@." Ir.pp_rel_nf nf; *)
                  let nf = Ir.expand_acq_rel nf in
                  (* Format.printf "A/L expanded: %a@." Ir.pp_rel_nf nf; *)
                  let nf = Ir.expand_domain_range nf in
                  (* Format.printf "Domain/range expanded: %a@." Ir.pp_rel_nf nf; *)
                  (nf, ast_expr))
            in
            Some (var, nfs))
  in
  if opts.conf then
    let open Format in
    requested_bindings
    |> List.iter (fun (var, nfs) ->
        printf "@.";
        printf "### %s@." var;
        let _ =
          List.fold_left
            (fun acc (nf, ast_e) ->
              let print_cat_rel () =
                if Arg.should_dump_origin opts then
                  printf "## %a@." Ast_utils.pp_exp ast_e
              in
              List.fold_left
                (fun acc seq ->
                  let relaxs = Translation.try_translate_seq seq in
                  let relaxs =
                    List.filter (fun r -> not (List.mem r acc)) relaxs
                  in
                  if relaxs <> [] then begin
                    print_cat_rel ();
                    printf "-safe %s@."
                      (String.concat " " (List.map Translation.pp_relax relaxs))
                  end;
                  acc @ relaxs)
                acc (Ir.get_union nf))
            [] nfs
        in
        ())
  else
    requested_bindings
    |> List.iter (fun (_, nfs) ->
        nfs
        |> List.iter (fun (nf, _) ->
            let relaxs =
              Ir.get_union nf
              |> Util.List.concat_map (fun seq ->
                  Translation.try_translate_seq seq)
            in
            let relaxs = Util.List.uniq ~eq:( = ) relaxs in
            relaxs
            |> List.iter (fun relax ->
                Format.printf "%s@." (Translation.pp_relax relax))))

let () =
  Logs.set_reporter (Logs.format_reporter ());
  let opts, file_paths = Arg.parse () in
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
  file_paths
  |> List.iter (fun file_path ->
      let tree = Parser.find_parse_deep file_path in
      run ~opts tree)
