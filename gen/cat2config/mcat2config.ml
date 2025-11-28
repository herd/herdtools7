[@@@warning "-40-42"]

(* Create a set of relaxations for diy using a cat file *)

module StringMap = Map.Make (String)

module Arg = struct
  type show = Tree | TreeOnly | Lets

  type opts = {
    verbose : int;
    lets_to_print : string list;
    conds : string list;
    unroll : int;
    libdir : string option;
    show : show option;
    conf : bool;
    show_rels : bool;
  }

  let parse_show : string -> show = function
    | "tree" -> Tree
    | "tree-only" -> TreeOnly
    | "lets" -> Lets
    | _ -> raise (Arg.Bad "Wrong value for -show")

  let parse : unit -> opts * string list =
   fun () ->
    let verbose = ref 0 in
    let lets_to_print = ref [] in
    let conds = ref [] in
    let unroll = ref 1 in
    let file_paths = ref [] in
    let add_file_path fp = file_paths := !file_paths @ [ fp ] in
    let libdir = ref None in
    let show = ref None in
    let conf = ref false in
    let show_rels = ref false in
    let opts =
      [
        ("-v", Arg.Unit (fun () -> incr verbose), " be verbose");
        ( "-let",
          Arg.String (fun s -> lets_to_print := !lets_to_print @ [ s ]),
          "<statement> print out selected let statements" );
        ( "-conds",
          Arg.String (fun s -> conds := !conds @ [ s ]),
          "<cond> choose what variant conditions to set" );
        ( "-unroll",
          Arg.Int (fun i -> unroll := i),
          "<unroll> choose how many times transitive and reflexive and \
           transitive operators unroll. Default = 1" );
        ( "-set-libdir",
          Arg.String (fun s -> libdir := Some s),
          "<path> set location of libdir to <path>" );
        ( "-show",
          Arg.String (fun s -> show := Some (parse_show s)),
          "<tree|tree-only|lets> show info on parsed model" );
        ( "-conf",
          Arg.Bool (fun b -> conf := b),
          "<true|false> print output as diy configuration file. Default = \
           false." );
        ( "-showrels",
          Arg.Bool (fun b -> show_rels := b),
          "<true|false> include source cat relations in configuration file. \
           Default = false." );
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
        verbose = !verbose;
        lets_to_print = !lets_to_print;
        conds = !conds;
        unroll = !unroll;
        libdir = !libdir;
        show = !show;
        conf = !conf;
        show_rels = !show_rels;
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

module Log = struct
  let verbose : int ref = ref 0

  (* Conditionally print if global verbosity level is at least the required level. *)
  let fprintv :
      'a. Format.formatter -> int -> ('a, Format.formatter, unit) format -> 'a =
   fun f required_level fmt ->
    let open Format in
    if required_level <= !verbose then fprintf f fmt else ifprintf f fmt

  let printv : 'a. int -> ('a, Format.formatter, unit) format -> 'a =
   fun required_level fmt -> fprintv Format.std_formatter required_level fmt

  let eprintv : 'a. int -> ('a, Format.formatter, unit) format -> 'a =
   fun required_level fmt -> fprintv Format.err_formatter required_level fmt
end

module Nf = Normalization.Make (Ir) (Log)

let extract_let_binding (ins : AST.ins) =
  let open AST in
  match ins with
  | Rec (_, (_, Pvar (Some varname), expression) :: _, _)
  | Let (_, (_, Pvar (Some varname), expression) :: _) ->
      Some (varname, expression)
  | _ -> None

let run ~(opts : Arg.opts) (tree : AST.ins list) =
  let bindings = List.filter_map extract_let_binding tree in
  let norm_config : Nf.norm_config =
    {
      conditions = opts.conds;
      unroll_depth = opts.unroll;
      set_var = Ir.parse_set_id;
      rel_var = Ir.parse_rel_id;
    }
  in
  let nf_map = Nf.normalize_bindings ~config:norm_config bindings in
  let results =
    opts.lets_to_print
    |> List.map (fun var ->
        let nfs = nf_map var in
        if nfs = [] then (
          Log.eprintv 0 "Failed to evaluate let binding: `%s`@." var;
          exit 1)
        else
          let nfs =
            nfs
            |> List.map (fun (nf, ast_expr) ->
                let nf = Ir.expand_acq_rel nf in
                let compressed = Ir.compress nf in
                let expanded = Ir.expand_domain_range compressed in
                (compressed, expanded, ast_expr))
          in
          (var, nfs))
  in
  if opts.conf then (
    let open Format in
    printf "-arch AArch64@.";
    printf "-nprocs 2@.";
    printf "-size 6@.";
    printf "-name cat2config-conf@.";
    results
    |> List.iter (fun (var, nfs) ->
        printf "@.";
        printf "### %s@." var;
        let _ =
          List.fold_left
            (fun acc (_, expanded, ast_e) ->
              let print_cat_rel () =
                if opts.show_rels then
                  match ast_e with
                  | Some e -> printf "## %a@." Ast_utils.pp_exp e
                  | None -> ()
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
                acc (Ir.get_union expanded))
            [] nfs
        in
        ()))
  else
    results
    |> List.iter (fun (_, nfs) ->
        nfs
        |> List.iter (fun (compressed, expanded, _) ->
            if opts.show = Some Tree then
              Format.printf "%a@." Ir.pp_rel_nf compressed;
            let relaxs =
              Ir.get_union expanded
              |> Util.List.concat_map (fun seq ->
                  Translation.try_translate_seq seq)
            in
            let relaxs = Util.List.uniq ~eq:( = ) relaxs in
            relaxs
            |> List.iter (fun relax ->
                Format.printf "%s@." (Translation.pp_relax relax))))

let () =
  let opts, file_paths = Arg.parse () in
  let module Parser = Make_parser (struct
    let debug = opts.verbose > 2
    let libdir = opts.libdir
  end) in
  Log.verbose := opts.verbose;
  file_paths
  |> List.iter (fun file_path ->
      let tree = Parser.find_parse_deep file_path in
      run ~opts tree)
