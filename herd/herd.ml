(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Entry point to Herd  *)

open Printf
open Archs
open Opts

(* Command line arguments *)
let args = ref []
let get_cmd_arg s = args := s :: !args

(* Helpers *)

let parse_tag opt set tags msg =
  opt,
  Arg.String
    (fun tag -> match set tag with
    | false ->
        raise
          (Arg.Bad
             (sprintf "bad tags for %s, allowed tag are %s"
                opt (String.concat "," tags)))
    | true -> ()),
  sprintf "<%s> %s" (String.concat "|" tags) msg

let badarg opt arg ty =
  raise
    (Arg.Bad
       (sprintf "wrong argument '%s'; option '%s' expects a %s"
          opt arg ty))

let parse_bool opt v msg =
  opt,
  Arg.Bool (fun b -> v := b),
  sprintf "<bool> %s, default %b" msg !v

let parse_int opt v msg =
  opt,
  Arg.Int (fun b -> v := b),
  sprintf "<int> %s, default %i" msg !v

let parse_int_opt opt v msg =
  opt,
  Arg.String
    (fun tag -> match tag with
    | "none" -> v := None
    | _ -> 
        try v := Some (int_of_string tag)
        with _ -> badarg opt tag "integer"),
  sprintf "<int|none> %s" msg

let parse_float opt v msg =
  opt,
  Arg.Float (fun b -> v := b),
  sprintf "<float> %s, default %.1f" msg !v

let parse_float_opt opt v msg =
  opt,
  Arg.String
    (fun tag -> match tag with
    | "none" -> v := None
    | _ ->
        try v := Some (float_of_string tag)
        with _ -> badarg tag opt "float"   ),                
  sprintf "<float|none> %s" msg

let parse_pos opt v msg =
  opt,
  Arg.String
    (fun tag -> match Misc.pos_of_string tag with
    | Some p -> v := p
    | None ->  badarg tag opt "float,float"),
  let x,y = !v in
  sprintf "<float,float> %s, default %.1f,%.1f" msg x y

let parse_posopt opt v msg =
  opt,
  Arg.String
    (fun tag -> match Misc.pos_of_string tag with
    | Some p -> v := Some p
    | None ->  badarg tag opt "float,float"),
  sprintf "<float,float> %s" msg

let parse_string_opt opt v msg =
  opt,
  Arg.String (fun s -> match s with "none" -> v := None | _ -> v := Some s),
  sprintf "<string|none> %s" msg

let parse_string opt v msg =
  opt,
  Arg.String (fun s -> v := s),
  sprintf "<string> %s, default %s" msg !v

let parse_stringsetfun opt f msg =
  opt,
  Arg.String
    (fun tag ->
      let es = Misc.split_comma tag in
      f (StringSet.of_list es)),
  sprintf "<name,..,name> %s" msg

let parse_stringset opt v msg =
  parse_stringsetfun opt (fun s -> v := StringSet.union s !v) msg

(* Option list *)
let load_config s =
  let module ML =
    MyLib.Make
      (struct
        let includes = !includes
        let libdir = Version.libdir
      end) in
  LexConf.lex (ML.find s)

let gen_model_opt s =
  parse_tag
    s
    (fun tag -> match Model.parse tag with
    | None -> false
    | Some _ as m -> model :=  m ; true)
    Model.tags
    (sprintf " select model, defaults X86=%s, PPC=%s, ARM=%s"
       (Model.pp (Model.get_default_model x86))
       (Model.pp (Model.get_default_model ppc))
       (Model.pp (Model.get_default_model arm)))

let options = [
(* Basic *)
  ("-version", Arg.Unit
     (fun () -> printf "%s, Rev: %s\n" Version.version Version.rev ; exit 0),
   " show version number and exit") ;   
  ("-libdir", Arg.Unit (fun () -> print_endline Version.libdir; exit 0),
    " show installation directory and exit");
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
  ("-q", Arg.Unit (fun _ -> verbose := -1; debug := Debug.none),
   "<default> do not show diagnostics");
  ("-I", Arg.String (fun s -> includes := !includes @ [s]),
   "<dir> add <dir> to search path");
  parse_bool "-exit" Opts.exit_if_failed "exit in case of failure";
  ("-conf",
   Arg.String load_config,
   "<name> read configuration file <name>") ;
  ("-bell",
   Arg.String (fun x -> Opts.bell := (Some x)),
   "<name> read bell file <name>") ;
  ("-o", Arg.String (fun s -> outputdir := PrettyConf.Outputdir s),
   "<dir> generated files will go into <dir>, default: do not generate") ;
  ("-suffix", Arg.String (fun s -> suffix := s),
   "<suf> add <suf> at the end of the base of generated files") ;
  parse_bool "-dumpes" Opts.dumpes "dump event structures";
  ( "-gv",
    Arg.Unit (fun _ -> PP.gv := true),
    "<non-default>  fork gv to show output graphs") ;
  ( "-evince",
    Arg.Unit (fun _ -> PP.evince := true),
    "<non-default>  fork evince to show output graphs") ;
  ("-unroll",
   Arg.Int (fun x -> unroll := x),
   sprintf "<int> branch unrolling upper limit, default %i" !unroll);
  parse_bool "-hexa" PP.hexa "print numbers in hexadecimal";
(* undocumented *)
  ("-switch",
   Arg.Unit (fun () -> Misc.switch := true),
   "switch something") ;
  ("-web",
   Arg.Unit (fun () -> load_config "web.cfg")," alias for -conf web.cfg");
  ("-c11",
   Arg.Unit (fun () -> load_config "cpp11.cfg")," alias for -conf cpp11.cfg");
  parse_tag
    "-debug"
    (fun tag -> match Debug.parse !debug tag with
    | None -> false
    | Some t -> debug := t ; true)
    Debug.tags
    "show debug messages for specific parts" ;
(* Engine control *)
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
  ("-skipchecks",
   Arg.String
     (fun s ->
       let tags = Misc.split_comma s in
       List.iter
         (fun tag ->
           skipchecks := StringSet.add tag !skipchecks)
         tags),
   "<name1,...,nameN> do not apply checks, cumulates") ;
  parse_bool "-strictskip" strictskip
   "retain outcomes allowed by ALL skipped checks" ;
(* Model control *)
  "-optace", Arg.Bool (fun b -> optace := Some b),
    "<bool> optimize axiomatic candidate generation, default is true except for the minimal model and all generic models";
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
  parse_bool "-badexecs" badexecs "list results of bad executions" ;
  parse_string_opt "-badflag" badflag "executions with flag <string> are bad" ;
(* undocumented *)
  "-showone",
  Arg.Bool (fun b -> if b then nshow := Some 1),
  "<bool> alias for -nshow 1";

(************************)
(* Control dot pitcures *)
(************************)
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
  parse_bool "-edgemerge" PP.edgemerge "merhe edges, cppmem style" ;
(* Legend *)
  parse_bool "-showlegend" PP.showlegend  "show legend in pictures" ;
  parse_bool "-showkind" showkind  "show test kind in legends" ;
  parse_bool "-shortlegend" shortlegend "show test name only in legends";
(* Nodes *)
 parse_bool "-squished" PP.squished "limit information in graph nodes" ;
  parse_bool "-fixedsize" PP.fixedsize
    "fixedsize attribute for nodes in graph" ;
  parse_float "-extrachars" PP.extrachars
    "additional space for computing node width, can be negative" ;
  parse_bool "-showobserved" PP.showobserved
    "highlight observed memory reads in pictures" ;
  parse_bool "-brackets" PP.brackets
    "show brackets around locations in pictures" ;
  parse_bool "-texmacros" PP.texmacros "use latex commands in output";
  parse_bool "-tikz" PP.tikz "generate dot files suitable for processing with TikZ";
(* Edge selection *)
  parse_bool "-showpo" PP.showpo "show po edges in pictures" ;
  parse_bool "-showinitrf" PP.showinitrf
    "show read-from edges from initial state in pictures" ;
  parse_bool "-showfinalrf" PP.showfinalrf
    "show read-from edges to final state in pictures" ;
  parse_pos "-initrfpos" PP.initdotpos
    "position of pseudo source event for initial rf" ;
  parse_pos "-finalrfpos" PP.initdotpos
    "position of pseudo target event for final rf" ;
  parse_bool "-oneinit" PP.oneinit
    "show a init writes pseudo-event, with all initial writes grouped" ;
  parse_posopt "-initpos" PP.initpos
    "position of the init writes pseudo-event" ;
  parse_bool "-showinitwrites" PP.showinitwrites
    "show init write events in pictures" ;
  parse_float "-threadposy" PP.threadposy
    "thread number position in the y direction" ;
 parse_stringsetfun "-doshow" PP.add_doshow "show those edges";
 parse_stringsetfun "-unshow" PP.add_unshow "do not show those edges" ;
 parse_stringset "-symetric" PP.symetric "declare those edges as symetric" ;
 parse_stringset "-showraw" PP.showraw
    "do not perform transitivity removal on those edges" ;

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
(* Select input *)
  ("-names",
   Arg.String (fun s -> names := !names @ [s]),
   "<filename> execute on tests whose names are listed in <filename>");
(* Change input *)
  ("-rename",
   Arg.String (fun s -> rename := Some s),
   "<name> specify rename mapping");
  ( "-kinds",
    Arg.String (fun s -> kinds := !kinds @ [s]),
    "<name> specify kind of tests (can be repeated)");
  ( "-conds",
    Arg.String  (fun s -> conds := !conds @ [s]),
    "<name> specify conditoins of tests (can be repeated)");
(* Undocumented *)
  parse_bool "-auto" auto 
  "produce output suitable for the dont tool";
  parse_tag "-restrict"
    (fun tag -> match Restrict.parse tag with
    | None -> false
    | Some t -> restrict := t ; true)
    Restrict.tags
    (sprintf "restrict outcomes, default %s" (Restrict.pp !restrict));
  parse_bool "-outcomereads" outcomereads "include all memory reads in outcomes" ;
  parse_string_opt "-overlap" PP.overlap "specify graph overlap attribute" ;
  parse_bool "-relabel" PP.relabel
    "merge power/arm labels(e.g sync -> sync/dmb)" ;
  parse_bool "-withbox" PP.withbox
    "box together events generated by the same instruction" ;
  parse_bool "-labelbox" PP.labelbox
    "label instruction instruction boxes with instruction" ;
  parse_bool "-movelabel" PP.movelabel
    "apply various tricks to enanche edge label placement in pictures" ;
  ("-dotheader",Arg.String (fun s -> PP.dotheader := Some s),
   "<name> insert the contents of <name> at the beginning of generated dot files");
]

(* Parse command line *)
let () =
  try
    Arg.parse options
      get_cmd_arg
      (sprintf "Usage %s [options] [test]*" prog)
  with
  | Misc.Fatal msg -> eprintf "%s: %s\n" prog msg ; exit 2

(* Read names *)
let names = match !names with
| [] -> None
| names -> Some (ReadNames.from_files names StringSet.add StringSet.empty)

(* Read generic model, if any *)
let libfind =
  let module ML =
    MyLib.Make
      (struct
        let includes = !includes
        let libdir = Version.libdir
      end) in
  ML.find

module ParserConfig = struct
  let debug = !debug.Debug.lexer
  let libfind =  libfind
end

let model,model_opts = match !model with
| Some (Model.File fname) ->
    let module P = ParseModel.Make(ParserConfig) in
    begin try
      let (b,_,_) as r = P.parse fname in
      Some (Model.Generic r),b
    with
    | Misc.Fatal msg -> eprintf "%s: %s\n" prog msg ; exit 2
    | Misc.Exit ->
        eprintf "Failure of generic model parsing\n" ;
        exit 2 end
| Some _ as m -> m,ModelOption.compat
| None -> None,ModelOption.default

(* Read kinds/conds files *)
module LR = LexRename.Make
  (struct let verbose = if !debug.Debug.lexer  then !verbose else 0 end)

let rename = match !rename with
| None -> TblRename.empty
| Some fname -> LR.read_from_file fname (fun s -> Some s)

let kinds = LR.read_from_files !kinds ConstrGen.parse_kind 

let conds = LR.read_from_files !conds (fun s -> Some s)

(* Configure parser/models/etc. *)
let () =
  let module Config = struct
    let auto = !auto
    let nshow = !nshow
    let restrict = !restrict
    let showkind = !showkind
    let shortlegend = !shortlegend
    let model = model
    let through = !through
    let skipchecks = !skipchecks
    let strictskip = !strictskip
    let outcomereads = !outcomereads
    let show = !show
    let badexecs = !badexecs 
    let badflag = !badflag
    let throughflag = !throughflag

    let check_name = match names with
    | None -> fun _ -> true
    | Some names -> (fun name -> StringSet.mem name names)
    let check_rename = TblRename.find_value_opt rename
    let check_kind = TblRename.find_value_opt kinds
    let check_cond =  TblRename.find_value_opt conds
    let libfind = libfind

    let model_enumco = model_opts.ModelOption.co
    let observed_finals_only = not model_enumco
    let initwrites = match !initwrites with
    | None -> model_opts.ModelOption.init
    | Some b -> b
    let debug = !debug
    let debuglexer = debug.Debug.lexer
    let verbose = !verbose
    let unroll = !unroll
    let speedcheck = !speedcheck
    let optace = match !optace with
    | Some b -> b
    | None -> match model with
      | Some (Model.Minimal b) -> b
      | Some (Model.Generic _|Model.File _) -> false
      | _ -> false
    let outputdir = !outputdir
    let suffix = !suffix
    let dumpes = !dumpes

    module PC = struct
      let debug = debug.Debug.pretty
      let verbose = verbose
      let dotmode = !PP.dotmode
      let dotcom = !PP.dotcom
      let gv = !PP.gv
      let evince = !PP.evince
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
      let symetric = !PP.symetric
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
    end

  end in

  let bi = match !Opts.bell with
  | None -> None
  | Some fname ->
      let module R =
        ReadBell.Make
          (struct
            let debug_lexer = Config.debug.Debug.lexer
            let debug_model = Config.debug.Debug.barrier
            let verbose = Config.verbose
            let libfind = libfind
            let prog = prog
          end) in
      let bi = R.read fname in
      Some (fname,bi) in

  let from_file =
    let module T =
      ParseTest.Top (struct let bell_model_info = bi include Config end) in
    T.from_file in


(* Just go *)

  let tests = !args in
  let check_exit =
    let b = !Opts.exit_if_failed in
    fun seen -> if b then exit 1 else seen in
  let _seen =
    Misc.fold_argv
      (fun name seen ->
        try from_file name seen
        with
        | Misc.Exit -> check_exit seen
        | Misc.Fatal msg ->
            Warn.warn_always "%a: %s" Pos.pp_pos0 name msg ;
             check_exit seen
        | Misc.UserError msg ->
            Warn.warn_always "%a: %s (User error)" Pos.pp_pos0 name msg ;
             check_exit seen
        | e ->
	    Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
	    raise e)
      tests StringMap.empty in
  exit 0
