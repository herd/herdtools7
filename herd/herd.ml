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

let load_config s = LexConf_herd.lex (Opts.libfind !includes s)

let pp_default_model a = sprintf "%s=%s" (Archs.pp a) (Model.pp (Model.get_default_model a))

let gen_model_opt s =
  parse_tag
    s
    (fun tag -> match Model.parse tag with
    | None -> false
    | Some _ as m -> model :=  m ; true)
    Model.tags
    (sprintf " select model, defaults %s, %s, %s, %s, %s, %s"
       (pp_default_model x86)
       (pp_default_model ppc)
       (pp_default_model arm)
       (pp_default_model aarch64)
       (pp_default_model riscv)
       (pp_default_model Archs.c))

let options = [
(* Basic *)
  ("-version", Arg.Unit
     (fun () -> printf "%s, Rev: %s\n" Version_herd.version Version_herd.rev ; exit 0),
   " show version number and exit") ;
  ("-libdir", Arg.Unit (fun () -> print_endline Version_herd.libdir; exit 0),
    " show installation directory and exit");
  ("-v", Arg.Unit (fun _ -> incr verbose),
   "<non-default> show various diagnostics, repeat to increase verbosity");
  ("-q", Arg.Unit (fun _ -> verbose := -1; debug := Debug_herd.none),
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
    (fun tag -> match Debug_herd.parse !debug tag with
    | None -> false
    | Some t -> debug := t ; true)
    Debug_herd.tags
    "show debug messages for specific parts" ;
   parse_bool "-morefences" moreedges "consider complete set of fences" ;
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
  parse_stringset "-skipchecks" skipchecks "do not apply listed checks, cumulative" ;
  parse_bool "-strictskip" strictskip "retain outcomes allowed by ALL skipped checks" ;
  parse_stringset "-cycles" cycles  "<name1,...,nameN> show failing checks as cycles, cumulates" ;

(* Model control *)
  begin let module ParseVariant = ParseTag.MakeS(Variant) in
  ParseVariant.parse "-variant" variant "select an architecture variation" end ;
  begin let module ParseMachSize = ParseTag.Make(MachSize) in
  ParseMachSize.parse "-machsize" byte "set basic machine size" end ;
  begin let module ParseEndian = ParseTag.Make(Endian) in
  ParseEndian.parse_opt "-endian" endian "set endianness" end ;

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
  parse_bool "-badexecs" badexecs "give output for tests that have bad executions (see -badflag)" ;
  parse_string_opt "-badflag" badflag "executions with flag <string> are bad" ;
  parse_bool "-checkfilter" check_filter "discard outcomes that negate filter proposition (if any)" ;
(* undocumented *)
  "-showone",
  Arg.Bool (fun b -> if b then nshow := Some 1),
  "<bool> alias for -nshow 1";

  "-statelessrc11",
  Arg.Bool (fun b -> if b then statelessrc11 := true),
  "<bool> enable stateless RC11 model checking, use with -variant normw, SC check can be skipped";

(************************)
(* Control dot pictures *)
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
  parse_bool "-edgemerge" PP.edgemerge "merge edges, cppmem style" ;
  parse_bool "-labelinit" PP.labelinit "show labels on the init node" ;
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
 parse_string_opt "-classes" PP.classes "show classes of this equivalence (no not cumulate)" ;
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
  CheckName.parse_names names ;
  CheckName.parse_excl excl ;
(* Change input *)
  CheckName.parse_rename rename ;
  ( "-kinds",
    Arg.String (fun s -> kinds := !kinds @ [s]),
    "<name> specify kind of tests (can be repeated)");
  ( "-conds",
    Arg.String  (fun s -> conds := !conds @ [s]),
    "<name> specify conditoins of tests (can be repeated)");
(* Undocumented *)
  parse_bool "-auto" auto
  "produce output suitable for the dont tool";
  parse_bool "-candidates" candidates
  "show complete candidate count in output" ;
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
    "apply various tricks to enhance edge label placement in pictures" ;
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

(* Read generic model, if any *)

let libfind = libfind !includes

module ParserConfig = struct
  let debug = !debug.Debug_herd.lexer
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
      let excl = !excl
    end)

(* Read kinds/conds files *)
module LR = LexRename.Make(Verbose)

let kinds = LR.read_from_files !kinds ConstrGen.parse_kind

let conds = LR.read_from_files !conds (fun s -> Some s)

(* Configure parser/models/etc. *)
let () =
  let module Config = struct
    let auto = !auto
    let candidates = !candidates
    let nshow = !nshow
    let restrict = !restrict
    let showkind = !showkind
    let shortlegend = !shortlegend
    let model = model
    let through = !through
    let skipchecks = !skipchecks
    let strictskip = !strictskip
    let cycles = !cycles
    let outcomereads = !outcomereads
    let show = !show
    let badexecs = !badexecs
    let badflag = !badflag
    let throughflag = !throughflag

    let statelessrc11 = !statelessrc11

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
      | Some (Model.Minimal b) -> b
      | Some (Model.Generic _|Model.File _) -> false
      | _ -> false
    let variant = !variant
    let byte = !byte
    let endian = !endian
    let outputdir = !outputdir
    let suffix = !suffix
    let dumpes = !dumpes
    let moreedges = !moreedges
    module PC = struct
      let debug = debug.Debug_herd.pretty
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
            let verbose = Config.verbose
            let libfind = libfind
            let compat = Config.variant Variant.BackCompat
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

  let check_pos0 s =
    String.length s > 5 &&
    begin match s.[0],s.[1],s.[2],s.[3],s.[4] with
    | 'F','i','l','e',' ' -> true
    | _ -> false
    end  in

  let _seen =
    Misc.fold_argv_or_stdin
      (fun name seen ->
        try from_file name seen
        with
        | Misc.Exit -> check_exit seen
        | Misc.Fatal msg ->
            Warn.warn_always "%a: %s" Pos.pp_pos0 name msg ;
             check_exit seen
        | Misc.UserError msg ->
            begin if check_pos0 msg then
              Warn.warn_always "%s (User error)" msg
            else
              Warn.warn_always "%a: %s (User error)" Pos.pp_pos0 name msg
            end ;
            check_exit seen
        | e ->
            Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
            raise e)
      tests StringMap.empty in
  exit 0
