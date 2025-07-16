(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2013-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Entry point to Herd  *)

open Printf
open Archs
open Opts
open OptNames

(* Command line arguments *)
let args = ref []
let get_cmd_arg s = args := s :: !args

(* Helpers *)

open ArgUtils


(* Option list *)

let load_config s =
  LexConf_herd.lex
    (Opts.libfind !includes !debug.Debug_herd.files s)

let pp_default_model a =
  sprintf "%s=%s" (Archs.pp a)
    (Model.pp (Model.get_default_model !Opts.variant a))

let gen_model_opt s =
  parse_tag
    s
    (fun tag -> match Model.parse tag with
    | None -> false
    | Some _ as m -> model :=  m ; true)
    Model.tags
    (sprintf " select model, defaults %s, %s, %s, %s, %s, %s, %s"
       (pp_default_model x86)
       (pp_default_model x86_64)
       (pp_default_model ppc)
       (pp_default_model arm)
       (pp_default_model aarch64)
       (pp_default_model riscv)
       (pp_default_model Archs.c))

let general_options = [
(* Basic *)
  ("-version", Arg.Unit
     (fun () -> printf "%s, Rev: %s\n" Version.version Version.rev ; exit 0),
   " show version number and exit") ;
  ("-libdir", Arg.Unit (fun () -> print_endline !Opts.libdir; exit 0),
    " show installation directory and exit");
  ("-set-libdir", Arg.String (fun s -> Opts.libdir := s),
    "<path> set installation directory to <path>");
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
  ("-q", Arg.Unit (fun _ -> verbose := -1; debug := Debug_herd.none),
   "<default> do not show diagnostics");
  ("-I", Arg.String (fun s -> includes := !includes @ [s]),
   "<dir> add <dir> to search path");
  parse_bool "-exit" Opts.exit_if_failed "exit in case of failure";
  parse_float_opt "-timeout" Opts.timeout "timeout (CPU time)";
  ("-conf",
   Arg.String load_config,
   "<name> read configuration file <name>") ;
  ("-bell",
   Arg.String (fun x -> Opts.bell := (Some x)),
   "<name> read bell file <name>") ;
  ("-macros",
   Arg.String (fun x -> Opts.macros := (Some x)),
   "<name> read macro (.def) file <name>") ;
  ("-o", Arg.String
     (fun s -> match s with
     | "-" -> outputdir := PrettyConf.StdoutOutput
     | _ -> outputdir := PrettyConf.Outputdir s),
   "<dir> generated files will go into <dir>, default: do not generate") ;
  ("-suffix", Arg.String (fun s -> suffix := s),
   "<suf> add <suf> at the end of the base of generated files") ;
  parse_bool "-dumpes" Opts.dumpes "dump event structures";
  begin let module ParseView = ParseTag.Make(View) in
  ParseView.parse_opt "-view" PP.view
    "fork specified viewer to show output graphs" end ;
  ( "-gv",
    Arg.Unit (fun _ -> PP.view := Some View.GV),
    "<non-default>  alias for -view gv") ;
  ( "-evince",
    Arg.Unit (fun _ -> PP.view := Some View.Evince),
    "<non-default>  alias for -view evince") ;
  ( "-preview",
    Arg.Unit (fun _ -> PP.view := Some View.Preview),
    "<non-default>  alias for -view preview") ;
  ("-unroll",
   Arg.Int (fun x -> unroll := Some x),
   sprintf "<int> branch unrolling upper limit, default ASL: %i, others: %i"
     (unroll_default `ASL)  (unroll_default `Others));
  parse_bool "-hexa" PP.hexa "print numbers in hexadecimal";
  ("-web",
   Arg.Unit (fun () -> load_config "web.cfg")," alias for -conf web.cfg");
  ("-c11",
   Arg.Unit (fun () -> load_config "cpp11.cfg")," alias for -conf cpp11.cfg");
  (* Undocumented *)
  parse_tag "-restrict"
    (fun tag -> match Restrict.parse tag with
    | None -> false
    | Some t -> restrict := t ; true)
    Restrict.tags
    (sprintf "restrict outcomes, default %s" (Restrict.pp !restrict));
 parse_tags
    "-debug"
    (fun tag -> match Debug_herd.parse !debug tag with
    | None -> false
    | Some t -> debug := t ; true)
    Debug_herd.tags
    "show debug messages for specific parts" ;
  (* Undocumented *)
  parse_bool "-candidates" candidates
  "show complete candidate count in output" ;
  parse_bool "-outcomereads" outcomereads "include all memory reads in outcomes" ;
 
]

let simulation_options = [
(* undocumented *)
  ("-switch",
   Arg.Unit (fun () -> Misc.switch := true),
   "switch something") ;
  parse_bool "-morefences" (ref false) "does nothing (deprecated)" ;
(* Simulation control *)
  gen_model_opt "-model";
  gen_model_opt "-cat";
  parse_tag
    "-through"
    (fun tag -> match Model.parse_through tag with
    | None -> false
    | Some t -> through :=  t ; true)
    Model.tags_through
    (sprintf
       "what to let through in addition to valid executions, default %s"
       (Model.pp_through !through)) ;
  parse_string_opt
    "-throughflag"
    throughflag
    "let through executions flagged with string, and only those" ;
  ("-skipcheck",
   Arg.String (fun tag -> skipchecks := StringSet.add tag !skipchecks),
   "<name> do not apply check, cumulates") ;
  parse_stringset "-skipchecks" skipchecks "do not apply listed checks, cumulative" ;
  parse_bool "-strictskip" strictskip "retain outcomes allowed by ALL skipped checks" ;
  parse_stringset "-cycles" cycles  "<name1,...,nameN> show failing checks as cycles, cumulates" ;

(* Model control *)
  begin
    let module ParseVariant = ParseTag.MakeS(Opts.OptS) in
    ParseVariant.parse "-variant" variant
      "select an architecture variation" end ;
  begin let module ParseMachSize = ParseTag.Make(MachSize.Tag) in
  ParseMachSize.parse "-machsize" byte "set basic machine size" end ;
  begin let module ParseEndian = ParseTag.Make(Endian) in
  ParseEndian.parse_opt "-endian" endian "set endianness" end ;
  parse_bool "-archcheck" archcheck "check compatibility of test and cat model architectures" ;
  parse_tag "-optace"
    (fun tag -> match OptAce.parse tag with
    | None -> false
    | Some t -> optace := Some t ; true)
    OptAce.tags
    "optimize axiomatic candidate generation, default is iico";
  "-initwrites", Arg.Bool (fun b -> initwrites := Some b),
    "<bool> represent init writes as write events, this option should not be used except for debugging model options";
  parse_tag "-show"
    (fun tag -> match PrettyConf.parse_show tag with
    | None -> false
    | Some t -> show := t ; true)
    PrettyConf.tags_show
    (sprintf "executions shown in figure, default %s"
       (PrettyConf.pp_show !show)) ;
  "-showflag",
  Arg.String  (fun flag -> show := PrettyConf.ShowFlag flag),
  "<string>  show executions flagged by string in figure" ;
  parse_bool "-badexecs" badexecs "give output for tests that have bad executions (see -badflag)" ;
  parse_string_opt "-badflag" badflag "executions with flag <string> are bad" ;
  parse_int_opt
    "-maxphantom" maxphantom "maximum phantom update (per variable)";
  "-statelessrc11",
  Arg.Bool (fun b -> if b then statelessrc11 := true),
  "<bool> enable stateless RC11 model checking, use with -variant normw, SC check can be skipped";
  "-dumpallfaults",
  Arg.Bool (fun b -> dumpallfaults := b),
  "Dump final states with all faults that that happenned regardless of the post-condition";
]

let discard_observations_options = [
(* Discard some observations *)
  parse_tag "-speedcheck"
    (fun tag -> match Speed.parse tag with
    | None -> false
    | Some t -> speedcheck := t ; true)
    Speed.tags
    "aim at checking condition in place of listing final states" ;
  "-nshow",
  Arg.Int (fun n -> nshow := Some n),
  "<n> collect at most <n> pictures, default is to collect all (specified) pictures";
   parse_bool "-checkfilter" check_filter "discard outcomes that negate filter proposition (if any)" ;
(* undocumented *)
  "-showone",
  Arg.Bool (fun b -> if b then nshow := Some 1),
  "<bool> alias for -nshow 1";
]

(************************)
(* Control dot pictures *)
(************************)
let dot_options = [ 
(* General *)
  parse_tag "-graph"
    (fun tag -> match Graph.parse tag with
    | None -> false
    | Some t -> PP.graph := t ; true)
     Graph.tags
     (sprintf "select sort of graph, default %s" (Graph.pp !PP.graph)) ;
  parse_tag "-dotmode"
    (fun tag -> match PrettyConf.parse_dotmode tag with
    | None -> false
    | Some t -> PP.dotmode := t ; true)
    PrettyConf.tags_dotmode
    (sprintf "control text in dot figures, default %s"
       (PrettyConf.pp_dotmode !PP.dotmode)) ;
  parse_tag "-dotcom"
    (fun tag -> match PrettyConf.parse_dotcom tag with
    | None -> false
    | Some _ as t -> PP.dotcom := t ; true)
    PrettyConf.tags_dotcom
    "select command to translate dot, default depends on other modes" ;
  parse_tag "-showevents"
    (fun tag -> match PrettyConf.parse_showevents tag with
    | None -> false
    | Some t -> PP.showevents := t ; true)
     PrettyConf.tags_showevents
     (sprintf "select events shown in figures, default %s"
       (PrettyConf.pp_showevents !PP.showevents)) ;
  parse_bool "-mono" PP.mono "monochrome figures" ;
  parse_float "-scale" PP.scale "global scale factor for graphs" ;
  parse_float "-xscale" PP.xscale
    "global scale factor for graphs, x direction" ;
  parse_float "-yscale" PP.yscale
    "global scale factor for graphs, y direction" ;
  parse_float "-dsiy" PP.dsiy "vertical variation for events generated by the same instruction" ;
  parse_float "-siwidth" PP.siwidth "width occupied by events generated by the same instruction" ;
  parse_float "-ptscale" PP.ptscale "scale factor for points" ;
  parse_float "-boxscale" PP.ptscale "scale factor box width" ;
  parse_bool "-showthread" PP.showthread
    "show thread numbers in figures" ;
  "-shift",
  Arg.String
    (fun tag ->
      let fs = Misc.split_comma tag in
      let fs =
        List.map
          (fun f ->
            try float_of_string f with
            | _ ->
                raise
                  (Arg.Bad
                     (sprintf "bad argument for option -shift: '%s'" tag)))
          fs in
      PP.shift := Array.of_list fs),
  "<float,...,float> add vertical space at thread start (column mode only)";
  parse_bool "-texmacros" PP.texmacros "use latex commands in output";
  parse_bool "-tikz" PP.tikz "generate dot files suitable for processing with TikZ";
  parse_float "-threadposy" PP.threadposy
    "thread number position in the y direction" ;
  parse_bool "-relabel" PP.relabel
    "merge power/arm labels(e.g sync -> sync/dmb)" ;
  parse_bool "-withbox" PP.withbox
    "box together events generated by the same instruction" ;
  parse_bool "-labelbox" PP.labelbox
    "label instruction instruction boxes with instruction" ;
  parse_bool "-movelabel" PP.movelabel
    "apply various tricks to enhance edge label placement in pictures" ;
  ("-dotheader",Arg.String (fun s -> PP.dotheader := Some s),
   "<name> insert the contents of <name> at the beginning of generated dot files");
]

let edge_options =[
  parse_bool "-edgemerge" PP.edgemerge "merge edges, cppmem style" ;
  (* Edge selection *)
  parse_bool "-showpo" PP.showpo "show po edges in pictures" ;
  parse_bool "-showinitrf" PP.showinitrf
    "show read-from edges from initial state in pictures" ;
  parse_bool "-showfinalrf" PP.showfinalrf
    "show read-from edges to final state in pictures" ;
  parse_stringsetfun "-doshow" PP.add_doshow "show those edges";
  parse_stringsetfun "-unshow" PP.add_unshow "do not show those edges" ;
  parse_stringset "-symetric" PP.symetric "declare those edges as symetric" ;
  parse_stringset "-noid" PP.noid "like -symetric, additionally do not show identity edges" ;
  parse_string_opt "-classes" PP.classes "show classes of this equivalence (no not cumulate)" ;
  parse_stringset "-showraw" PP.showraw "do not perform transitivity removal on those edges" ;
]

let node_options = [
(* Node options *)
  parse_bool "-labelinit" PP.labelinit "show labels on the init node" ;
  parse_bool "-squished" PP.squished "limit information in graph nodes" ;
  parse_bool "-fixedsize" PP.fixedsize
    "fixedsize attribute for nodes in graph" ;
  parse_float "-extrachars" PP.extrachars
    "additional space for computing node width, can be negative" ;
  parse_bool "-showobserved" PP.showobserved
    "highlight observed memory reads in pictures" ;
  parse_bool "-brackets" PP.brackets
    "show brackets around locations in pictures" ;
  parse_pos "-initrfpos" PP.initdotpos
    "position of pseudo source event for initial rf" ;
  parse_pos "-finalrfpos" PP.initdotpos
    "position of pseudo target event for final rf" ;
  parse_bool "-oneinit" PP.oneinit
    "show a init writes pseudo-event, with all initial writes grouped" ;
  parse_pos_opt "-initpos" PP.initpos
    "position of the init writes pseudo-event" ;
  parse_bool "-showinitwrites" PP.showinitwrites
    "show init write events in pictures" ;
]

let legend_options = [
(* Legend *)
  parse_bool "-showlegend" PP.showlegend  "show legend in pictures" ;
  parse_bool "-showkind" showkind  "show test kind in legends" ;
  parse_bool "-shortlegend" shortlegend "show test name only in legends";
]
  
let graphviz_options = [
(* DOT contents control *)
  parse_tag "-splines"
    (fun tag -> match Splines.parse tag with
    | None -> false
    | Some Splines.No -> PP.splines := None; true
    | Some t -> PP.splines := Some t ; true)
    Splines.tags
    "specify splines graph attribute, default none" ;
  parse_float_opt "-margin" PP.margin "margin attribute of graphs";
  parse_float_opt "-pad" PP.pad "pad attribute of graphs";
  parse_string_opt "-sep" PP.sep "specify graph sep attribute" ;
  parse_string_opt "-fontname" PP.fontname "fontname attribute in graphs" ;
  parse_int_opt "-fontsize" PP.fontsize "fontsize attribute in graphs" ;
  parse_int "-edgefontsizedelta" PP.edgedelta "value to add to edge fontsize" ;
  parse_float_opt "-penwidth" PP.penwidth "penwidth attribute in graphs" ;
  parse_float_opt "-arrowsize" PP.arrowsize "arrowsize attribute in graphs" ;
  "-edgeattr",
  Arg.String
    (fun tag -> match Misc.split_comma tag with
    | [lbl;a;v;] -> PP.add_edgeattr lbl a v
    | _ ->
        raise
          (Arg.Bad
             (sprintf "bad argument for option -edgeattr: '%s'" tag))),
  "<label,attribute,value> specify an attribute for edges labelled by label";
  parse_string_opt "-overlap" PP.overlap "specify graph overlap attribute" ;
  ]

let input_options = 
(* Select input *)
  parse_noselect
(* Change input *)
  @[( "-kinds",
    Arg.String (fun s -> kinds := !kinds @ [s]),
    "<name> specify kind of tests (can be repeated)");
  ( "-conds",
    Arg.String  (fun s -> conds := !conds @ [s]),
    "<name> specify conditions of tests (can be repeated)");
  ]

(*Usage messages*)
let usg_general = (sprintf "\n Options that change the general behaviour:")
let usg_simulation = (sprintf "\n Options that configure the herd simulator:")
let usg_discard_observations = (sprintf"\n Options to filter execution graphs:")
let usg_dot = (sprintf "\n Options that control the general content of DOT images:")
let usg_edge = (sprintf "\n Options that control the display of edges in DOT images:")
let usg_node = (sprintf "\n Options that control what is shown in nodes and their sizes in DOT images:")
let usg_legend = (sprintf "\n Options that control picture legends:")
let usg_graphviz = (sprintf "\n Options that control Graphviz attributes:")
let usg_input = (sprintf "\n Options that filter tests to run:")
let usg_default = String.concat "\n"
  ["These are the most common options of herd7:
  -variant <variant_name>       Activate variations of models
  -showevents <all|mem|noregs>  Select which events are shown in diagrams,
                                default: noregs 
  -show <prop|neg|all|cond|wit|none> 
                                Select which executions are displayed in diagrams
                                depending on the final condition of the test,
                                default: none
  -through <all|invalid|none>   Allow additional executions to reach the final
                                stage of the simulation in addition to valid ones,
                                default: none
  -v                            Show various diagnostics, repeat to increase
                                verbosity
  -view <gv|evince|preview>     Automatically open output diagrams with
                                specified viewer
  -o <dir>                      Output directory for generated files
  -hexa <bool>                  Print numbers in hexadecimal, default: false
  -doshow <edge_name,...>       Show the specified edges in diagrams
  -unshow <edge_name,...>       Do not show the specified edges in diagrams
  -skipcheck <check_name>       Do not apply named check defined in the model
                                file, can be repeated
  -conf <file>                  Read configuration from the given file, 
                                example: -conf herd/libdir/file.cfg
  -cat <file>                   Select CAT model,
                                example: -cat catalogue/aarch64/file.cat

For advanced options, use `-help <category>`:
  Simulator behaviour:
    general      Control the general behaviour
    simulation   Configure the herd simulator
    discard      Filter execution graphs
    input        Filter tests to run
  Controlling DOT pictures:
    dot          Control the general content of DOT images
    edge         Control the display of edges in DOT images
    node         Control what is shown in nodes and their sizes in DOT images
    legend       Control picture legends in DOT images
    graphviz     Control Graphviz attributes
  Help shortcuts:
    -help        Show this help message
    -help all    Show all available options
    --help       Show all available options
"
]

let show_short_help () =
  print_endline usg_default;
  exit 0

let show_long_help () =
  print_endline "Available options:";
  Arg.usage general_options usg_general;
  Arg.usage simulation_options usg_simulation;
  Arg.usage discard_observations_options usg_discard_observations;
  Arg.usage dot_options usg_dot;
  Arg.usage edge_options usg_edge;
  Arg.usage node_options usg_node;
  Arg.usage legend_options usg_legend;
  Arg.usage graphviz_options usg_graphviz;
  Arg.usage input_options usg_input;
  exit 0

let help_exists () =
  Array.exists (fun s -> s = "-help" || s = "--help") Sys.argv

let help_options =[
  ("-help", Arg.Unit show_short_help, "show option categories");
  ("--help", Arg.Unit show_long_help, "show all options" )
]

let options = general_options @
  simulation_options @
  discard_observations_options @
  dot_options @
  edge_options @
  node_options @
  legend_options @
  graphviz_options @
  input_options @
  help_options

(* Parse command line *)
let () =
  let argv_len = Array.length Sys.argv in
  let arg = Sys.argv.(argv_len -1) in
  try
    if help_exists () then
    match arg with
    | "general" ->
        Arg.usage general_options usg_general; exit 0
    | "simulation" ->
        Arg.usage simulation_options usg_simulation; exit 0
    | "discard" ->
        Arg.usage discard_observations_options usg_discard_observations; exit 0
    | "dot" ->
        Arg.usage dot_options usg_dot; exit 0  
    | "edge" ->
        Arg.usage edge_options usg_edge; exit 0
    | "node" ->
        Arg.usage node_options usg_node; exit 0
    | "legend" ->
        Arg.usage legend_options usg_legend; exit 0
    | "graphviz" ->
        Arg.usage graphviz_options usg_graphviz; exit 0
    | "input" ->
        Arg.usage input_options usg_input; exit 0
    | "all" ->
        show_long_help ();
    |  _ -> 
        if Array.exists (fun s -> s = "-help") Sys.argv then
          show_short_help ()
        else 
          show_long_help ();
    else
      let current = ref 0 in
      match Arg.parse_argv ~current Sys.argv options get_cmd_arg usg_default with
      | () -> ()
      | exception Arg.Bad msg -> 
          begin
            let first_line = String.trim msg |> String.split_on_char '\n' |> List.hd in
            Printf.eprintf "Error: %s\n\n%s\n" first_line usg_default;
            exit 1
          end
  with
  | Misc.Fatal msg -> eprintf "%s: %s\n" prog msg ; exit 2
  | Invalid_argument _ -> eprintf "Invalid command line arguments.\n"; exit 1

(* Read generic model, if any *)

let libfind = Opts.libfind !includes !debug.Debug_herd.files

module ParserConfig = struct
  let debug = !debug.Debug_herd.lexer
  let libfind =  libfind
end

let model,model_opts = match !model with
| Some (Model.File fname) ->
    let module P = ParseModel.Make(ParserConfig) in
    begin try
      let (fname,((b,_,_) as r)) = P.find_parse fname in
      Some (Model.Generic (fname,r)),b
    with
    | Misc.Fatal msg -> eprintf "%s: %s\n" prog msg ; exit 2
    | Misc.Exit ->
        eprintf "Failure of generic model parsing\n" ;
        exit 2 end
| Some _ as m -> m,ModelOption.compat
| None -> None,ModelOption.default

(* Check names, NB no select argument! *)

module Verbose =
  struct let verbose = if !debug.Debug_herd.lexer  then !verbose else 0 end

module Check =
  CheckName.Make
    (struct
      include Verbose
      let rename = !rename
      let select = []
      let names = !names
      let oknames = !oknames
      let excl = !excl
      let nonames = !nonames
    end)

(* Read kinds/conds files *)
module LR = LexRename.Make(Verbose)

let kinds = LR.read_from_files !kinds ConstrGen.parse_kind

let conds = LR.read_from_files !conds (fun s -> Some s)

(* Configure parser/models/etc. *)
let () =
  let module Config = struct
    let timeout = !timeout
    let candidates = !candidates
    let nshow = !nshow
    let restrict = !restrict
    let showkind = !showkind
    let shortlegend = !shortlegend
    let model = model
    let archcheck = !archcheck
    let through = !through
    let skipchecks = !skipchecks
    let strictskip = !strictskip
    let cycles = !cycles
    let outcomereads = !outcomereads
    let show = !show
    let badexecs = !badexecs
    let badflag = !badflag
    let throughflag = !throughflag
    let maxphantom = !maxphantom
    let statelessrc11 = !statelessrc11
    let dumpallfaults = !dumpallfaults

    let check_name = Check.ok
    let check_rename = Check.rename_opt
    let check_kind = TblRename.find_value_opt kinds
    let check_cond =  TblRename.find_value_opt conds
    let libfind = libfind
    let macros = !macros

    let model_enumco = model_opts.ModelOption.co
    let observed_finals_only = not model_enumco
    let initwrites = match !initwrites with
    | None -> model_opts.ModelOption.init
    | Some b -> b
    let check_filter = !check_filter
    let debug = !debug
    let debuglexer = debug.Debug_herd.lexer
    let verbose = !verbose
    let unroll = !unroll
    let speedcheck = !speedcheck
    let optace = match !optace with
    | Some b -> b
    | None -> match model with
      | Some (Model.Generic _|Model.File _)|None -> OptAce.Iico
      | Some (Model.CAV12 _) -> OptAce.False
    let variant = !variant
    let fault_handling = !Refs.fault_handling
    let mte_precision = !Refs.mte_precision
    let sve_vector_length = !Refs.sve_vector_length
    let sme_vector_length = !Refs.sme_vector_length
  let byte = !byte
    let endian = !endian
    let outputdir = !outputdir
    let suffix = !suffix
    let dumpes = !dumpes

    module PC = struct
      let debug = debug.Debug_herd.pretty
      let verbose = verbose
      let dotmode = !PP.dotmode
      let dotcom = !PP.dotcom
      let view = !PP.view
      let showevents = !PP.showevents
      let texmacros = !PP.texmacros
      let tikz = !PP.tikz
      let hexa = !PP.hexa
      let mono = !PP.mono
      let fontname = !PP.fontname
      let fontsize = !PP.fontsize
      let edgedelta = !PP.edgedelta
      let penwidth = !PP.penwidth
      let arrowsize = !PP.arrowsize
      let splines = !PP.splines
      let overlap = !PP.overlap
      let sep = !PP.sep
      let margin = !PP.margin
      let pad = !PP.pad
      let scale = !PP.scale
      let xscale = !PP.xscale
      let yscale = !PP.yscale
      let dsiy = !PP.dsiy
      let siwidth = !PP.siwidth
      let boxscale = !PP.boxscale
      let ptscale = !PP.ptscale
      let squished = !PP.squished
      let graph = !PP.graph
      let showpo = !PP.showpo
      let relabel = !PP.relabel
      let withbox = !PP.withbox
      let labelbox = !PP.labelbox
      let showthread = !PP.showthread
      let showlegend = !PP.showlegend
      let showfinalrf = !PP.showfinalrf
      let showinitrf = !PP.showinitrf
      let finaldotpos = !PP.finaldotpos
      let initdotpos = !PP.initdotpos
      let oneinit = !PP.oneinit
      let initpos = !PP.initpos
      let threadposy = !PP.threadposy
      let showinitwrites = !PP.showinitwrites
      let brackets = !PP.brackets
      let showobserved = !PP.showobserved
      let movelabel = !PP.movelabel
      let fixedsize = !PP.fixedsize
      let extrachars = !PP.extrachars
      let edgeattrs = PP.get_edgeattrs ()
      let doshow = !PP.doshow
      let unshow = !PP.unshow
      let noid = !PP.noid
      let symetric = !PP.symetric
      let classes = !PP.classes
      let showraw = !PP.showraw

      let dotheader = match !PP.dotheader with
      | None -> None
      | Some f ->
          let fname = libfind f in
          try
            Misc.input_protect
              (fun chan ->
                let xs =
                  MySys.read_by_line chan (fun x xs -> x::xs) [] in
                Some (String.concat "\n" (List.rev xs)))
              fname
          with Sys_error msg ->
            eprintf "Cannot read %s: %s\n" f msg ;
            exit 2
      let shift = !PP.shift
      let edgemerge = !PP.edgemerge
      let labelinit = !PP.labelinit
      let variant = variant
    end

  end in

  let bi = match !Opts.bell with
  | None -> None
  | Some fname ->
      let module R =
        ReadBell.Make
          (struct
            let debug_lexer = Config.debug.Debug_herd.lexer
            let debug_model = Config.debug.Debug_herd.barrier
            let debug_files = Config.debug.Debug_herd.files
            let verbose = Config.verbose
            let libfind = libfind
            let compat = Config.variant Variant.BackCompat
            let prog = prog
            let variant = Misc.delay_parse Config.variant Variant.parse
          end) in
      let bi = R.read fname in
      Some (fname,bi) in

  let from_file f =
    let module T =
      ParseTest.Top
        (struct
          let bell_model_info = bi
          include Config end) in
    SymbValue.reset_gensym () ;
    T.from_file f in


(* Just go *)

  let tests = !args in

  let check_exit =
    let b = !Opts.exit_if_failed in
    fun seen -> if b then exit 1 else seen in

  let check_pos0 s =
    String.length s > 5 &&
    begin match s.[0],s.[1],s.[2],s.[3],s.[4] with
    | 'F','i','l','e',' ' -> true
    | _ -> false
    end  in

  let dbg_exc = !Opts.debug.Debug_herd.exc in

  let _seen =

(* If interval timer enabled and triggered,
   then stop test with not output at all *)
    Itimer.set_signal
      Config.timeout
      (fun _ -> raise Misc.Timeout)
      !debug.Debug_herd.timeout;
    Misc.fold_argv_or_stdin
      (fun name seen ->
        try from_file name seen
        with
        | Misc.Timeout -> seen
        | Misc.Exit as e ->
           if dbg_exc then raise e ;
           check_exit seen
        | Misc.Fatal msg as e  ->
           if dbg_exc then raise e ;
           Warn.warn_always "%a: %s" Pos.pp_pos0 name msg ;
           check_exit seen
        | Misc.UserError msg as e ->
           if dbg_exc then raise e ;
           begin if check_pos0 msg then
             Warn.warn_always "%s (User error)" msg
           else
             Warn.warn_always "%a: %s (User error)" Pos.pp_pos0 name msg
           end ;
           check_exit seen
        | Asllib.Error.ASLException e as exc ->
           if dbg_exc then raise exc ;
           Warn.warn_always "%s" (Asllib.Error.error_to_string e);
           check_exit seen
        | e ->
           Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
           raise e)
      tests StringMap.empty in
  exit 0
