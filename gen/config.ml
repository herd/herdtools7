(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

open Printf
open Code
let verbose = ref 0
let nprocs = ref 4
let size = ref 6
let one = ref false
let arch = ref (`PPC: Archs.t)
let typ = ref TypBase.default
let hexa = ref false
let tarfile = ref None
let prefix = ref None
let safes = ref None
let relaxs = ref None
let name = ref None
let sufname = ref None
let canonical_only = ref true
let conf = ref None
let mode = ref Sc
let mix = ref false
let max_ins = ref 4
let eprocs = ref false
let max_relax = ref 100
type 'a cumul = Empty | All | Set of 'a
let cumul = ref All
let poll = ref false
let docheck = ref false
let fmt = ref 3
let no = ref None
let addnum = ref true
let lowercase = ref false
let optcoherence = ref false
let bell = ref None
let scope = ref Scope.No
let variant = ref (fun (_:Variant_gen.t) -> false)
let mtags = ref false

type do_observers =
  | Avoid   (* was false *)
  | Accept  (* was true  *)
  | Enforce (* is new *)
  | Local   (* Local observer when possible *)
  | Three   (* Accept up to three writes, no observers *)
  | Four    (* Accept up to four writes, no observers  *)
  | Infinity  (* Accept all tests, no observers *)

let do_observers = ref Avoid
type obs_type = Straight | Fenced | Loop
let obs_type = ref Straight

let upto = ref true
let optcond = ref true
let overload = ref None
let neg = ref false
let unrollatomic : int option ref = ref None
let coherence_decreasing = ref false
let same_loc = ref false
type cond = Cycle | Unicond | Observe
let cond = ref Cycle
let hout = ref None
type show = Edges | Annotations | Fences
let show = ref (None:ShowGen.t option)
let debug = ref Debug_gen.none
let moreedges = ref false
let realdep = ref false

let parse_cond tag = match tag with
| "cycle" -> Cycle
| "unicond" -> Unicond
| "observe" -> Observe
| _ -> failwith "Wrong cond, choose cycle, unicond or observe"

let parse_mode s =
  match s with
    | "sc" -> Sc
    | "thin" -> Thin
    | "uni" -> Uni
    | "critical" -> Critical
    | "free" -> Free
    | "ppo" -> Ppo
    | "transitive"|"trans" -> Transitive
    | "total" -> Total
    | "mixed" -> MixedCheck
    | _ -> failwith "Wrong mode, choose sc,thin,uni,critical,free,ppo,total,mixed"

let parse_do_observers s = match s with
| "avoid"|"false" -> Avoid
| "accept"|"true" -> Accept
| "force" -> Enforce
| "local" -> Local
| "three" -> Three
| "four"  -> Four
| "oo"|"infinity" -> Infinity
| _ -> failwith "Wrong observer mode, choose avoid, accept, force, local, three or four"

let parse_obs_type = function
  | "straight" -> Straight
  | "fenced" -> Fenced
  | "loop" -> Loop
  | _ -> failwith "Wrong observer style, choose straight, fenced or loop"

let parse_cumul = function
  | "false" -> Empty
  | "true" -> All
  | s -> Set s


(* Helpers *)

let common_specs =
  ("-v", Arg.Unit (fun () -> incr verbose),"  be verbose")::
  ("-version", Arg.Unit (fun () -> print_endline Version_gen.version ; exit 0),
   " show version number and exit")::
  Util.parse_tag "-debug"
    (fun tag -> match Debug_gen.parse !debug tag with
    | None -> false
    | Some d -> debug := d ; true)
    Debug_gen.tags "specify debugged part"::
  Util.parse_tag
    "-arch"
    (fun tag -> match Archs.parse tag with
    | None -> false
    | Some a -> arch := a ; true)
    Archs.tags "specify architecture"::
  ("-bell",
   Arg.String (fun f -> arch := Archs.lisa ; bell := Some f),
   "<name> read bell file <name>, implies -arch LISA")::

  Util.parse_tag
    "-scopes"
    (fun tag -> match Scope.parse tag with
    | None -> false
    | Some a -> scope := a; true)
   Scope.tags  "<tag> specifiy scope tree"::
  Util.parse_tag
    "-type"
    (fun tag -> match TypBase.parse tag with
    | None -> false
    | Some a -> typ := a ; true)
    TypBase.tags
    (sprintf "specify base type, default %s" (TypBase.pp !typ))::
  Util.parse_tag
    "-variant"
    (fun tag -> match Variant_gen.parse tag with
    | None -> false
    | Some v0 ->
        let ov = !variant in variant := (fun v -> v = v0 || ov v) ;
        true)
    Variant_gen.tags
    (sprintf "specify variant")::
   ("-hexa", Arg.Unit (fun () -> hexa := true),"hexadecimal output")::
   ("-o", Arg.String (fun s -> tarfile := Some s),
    "<name.tar> output litmus tests in archive <name.tar> (default, output in curent directory)")::
  ("-mtags", Arg.Bool (fun b ->  mtags := b),
   sprintf "<bool> initialise tags (default %b)" !mtags)::
   ("-c", Arg.Bool (fun b ->  canonical_only := b),
   sprintf "<b> avoid equivalent cycles (default %b)" !canonical_only)::
  ("-list",
   Arg.Unit (fun () -> show := Some ShowGen.Edges),
   "list accepted edge syntax and exit")::
  ("-show", Arg.String (fun s -> show := Some (ShowGen.parse s)),
    "<edges|annotations|fences> list accepted edges, annotations or fences, and exit")::
  ("-switch", Arg.Set Misc.switch, "switch something")::
  ("-obs",
   Arg.String (fun s -> do_observers := parse_do_observers s),
   "<accept|avoid|force|local> enable observers (default avoid)")::
  ("-obstype",
   Arg.String (fun s -> obs_type := parse_obs_type s),
   "<fenced|loop|straight> style of observers (default fenced)")::
  ("-optcond", Arg.Set optcond, " optimize conditions (default)")::
  ("-nooptcond", Arg.Clear optcond, "do not optimize conditions")::
  ("-optcoherence", Arg.Set optcoherence, " optimize coherence")::
  ("-nooptcoherence", Arg.Clear optcoherence, "do not optimize coherence (default)")::
  ("-moreedges", Arg.Bool (fun b -> moreedges := b),
   Printf.sprintf
     "consider a very complete set of edges, default %b" !moreedges)::
  ("-realdep", Arg.Bool (fun b -> realdep := b),
   Printf.sprintf
     "output \"real\" dependencies, default %b" !moreedges)::
  ("-overload", Arg.Int (fun n -> overload := Some n),
   "<n> stress load unit by <n> useless loads")::
  ("-unrollatomic",Arg.Int (fun i -> unrollatomic := Some i),
   "<n> unroll atomic idioms (default, use loops)")::
  ("-ua",Arg.Int (fun i -> unrollatomic := Some i),
   "<n> shorthand for -unrollatomic <n>")::
  ("-poll",Arg.Bool (fun b -> poll := b),
     "<bool> poll on loaded values, as much as possible")::
  ("-check",Arg.Bool (fun b -> docheck := b),
     "<bool> check loaded values in test code")::
  ("-neg", Arg.Bool (fun b -> neg := b),
    "<bool> negate final condition (default false)")::
  ("-coherence_decreasing", Arg.Set coherence_decreasing,
  " Change value order in coherence orders, for backward compatibility")::
  ("-oneloc", Arg.Set same_loc,
  "Do not fail on tests with one single location (default false)")::
  ("-cond",
   Arg.String (fun s -> cond := parse_cond s),
  "<cycle|unicond|observe> style of final condition, default cycle")::
  ("-unicond", Arg.Unit (fun () -> cond := Unicond),
  "alias for -cond unicond (deprecated)")::
  ("-oh", Arg.String (fun n -> hout := Some n),
   "<fname> save a copy of hints")::
  ("-addnum", Arg.Bool (fun n -> addnum := n),
   sprintf "<bool> complete test name with number when identical (default %b)"
     !addnum)::
   ("-name",Arg.String (fun s -> name := Some s),
     "<s> specify base name of tests")::
   ("-sufname",Arg.String (fun s -> sufname := Some s),
     "<s> specify test name suffix")::
  ("-lowercase", Arg.Bool (fun b -> lowercase := b),
   sprintf "<bool> generate lowercase family names (default %b)" !lowercase)::
   ("-fmt",Arg.Int (fun i -> fmt := i),
     sprintf "<i> size of integer added to base name that yield test names (default %i)" !fmt)::
   ("-no", Arg.String (fun s -> no := Some s),
     "<fname> do not generate tests for these cycles")::
  []

let numeric = ref true

let speclist =
  common_specs @
  ("-num", Arg.Bool (fun b -> numeric := b),
   sprintf "<bool> use numeric names (default %b)" !numeric)::
   ("-mode", Arg.String (fun s -> mode := parse_mode s),
    "<sc|critical|thin|uni|free|transitive|ppo> running mode (default sc). Modes thin and uni are experimental.")::
   ("-cumul", Arg.String (fun b -> cumul := parse_cumul b),
    "<s> allow non-explicit fence cumulativity for specified fenced (default all)")::
   ("-conf", Arg.String (fun s -> conf := Some s), "<file> read configuration file")::
   ("-size", Arg.Int (fun n -> size := n),
    sprintf
      "<n> set the maximal size of cycles (default %i)"
      !size)::
   ("-exact", Arg.Clear upto, " produce cycle of size exactly <n>")::
   ("-nprocs", Arg.Int (fun n -> nprocs := n),
   sprintf "<n> reject tests with more than <n> threads  (default %i)"
      !nprocs)::
   ("-eprocs", Arg.Set eprocs,
    "produce tests with exactly <n> threads (default disabled)")::
   ("-ins", Arg.Int (fun n -> max_ins := n),
    sprintf "<n> max number of edges per proc (default %i)" !max_ins)::
   ("-one", Arg.Unit (fun _ -> one := true),
    "<relax-list> specify a sole cycle")::
  ("-prefix", Arg.String (fun s -> prefix := Some s),
    "<relax-list> specify a prefix for cycles")::
   ("-relax", Arg.String (fun s -> relaxs := Some s),
    "<relax-list> specify a relax list")::
   ("-mix", Arg.Bool (fun b -> mix := b),
    sprintf
      "<bool> mix relaxations when several are given (default %b)" !mix)::
   ("-maxrelax",   Arg.Int (fun n -> mix := true ; max_relax := n),
    sprintf "<n> test relaxation together up to <n> (default %i). Implies -mix true " !max_relax)::
   ("-safe", Arg.String (fun s -> safes := Some s),
    "<relax-list> specify a safe list")::
  []

let varatom = ref ([] : string list)

let varatomspec =
  ("-varatom", Arg.String (fun s -> varatom := !varatom @ [s]),
   "<atom specs> specify atom variations")

let prog = if Array.length Sys.argv > 0 then Sys.argv.(0) else "XXX"
let baseprog = sprintf "%s (version %s)" (Filename.basename prog) (Version_gen.version)

let usage_msg = "Usage: " ^ prog ^   "[options]*"

let read_no fname =
  try
    Misc.input_protect
      (fun chan -> MySys.read_list chan (fun s -> Some s))
      fname
  with _ -> []

let read_bell libfind fname =
  let module R =
    ReadBell.Make
      (struct
        let debug_lexer = false
        let debug_model = false
        let verbose = !verbose
        let libfind = libfind
        let compat = false
        let prog = prog
      end) in
  R.read fname

let parse_annots lines = match lines with
| [] -> None
| _ ->
    let module P =
      Annot.Make
        (struct
          let debug = !debug.Debug_gen.lexer
        end) in
    Some (P.parse lines)
