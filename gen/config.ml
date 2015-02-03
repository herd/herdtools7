(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
open Archs
open Code
let verbose = ref 0
let nprocs = ref 4
let size = ref 6
let one = ref false 
let arch = ref PPC
let typ = ref TypBase.Int
let tarfile = ref None
let prefix = ref None
let safes = ref None 
let relaxs = ref None
let name = ref None
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

type do_observers =
  | Avoid   (* was false *)
  | Accept  (* was true  *)
  | Enforce (* is new *)
  | Local   (* Local observer when possible *)
let do_observers = ref Avoid
type obs_type = Straight | Fenced | Loop
let obs_type = ref Straight

let upto = ref true
let optcond = ref true
let overload = ref None
type fno = FnoNo | FnoPrev | FnoBoth | FnoOnly | FnoRf | FnoAll
let fno = ref FnoNo
let sta = ref false
let neg = ref false
let unrollatomic : int option ref = ref None
let coherence_decreasing = ref false
let same_loc = ref false
type cond = Cycle | Unicond | Observe
let cond = ref Cycle
let hout = ref None
let list_edges = ref false

let parse_cond tag = match tag with
| "cycle" -> Cycle
| "unicond" -> Unicond
| "observe" -> Observe
| _ -> failwith "Wrong cond, choose cycle, unicond or observe"

let parse_fno s = match s with
| "no" -> FnoNo
| "prev" -> FnoPrev
| "both" -> FnoBoth
| "only" -> FnoOnly
| "rf"   -> FnoRf
| "all"  -> FnoAll
| _ -> failwith "Wrong fno, choose no, prev, both, only, rf, or all"

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
    | _ -> failwith "Wrong mode, choose sc,thin,uni,critical,free,ppo,total"

let parse_do_observers s = match s with
| "avoid"|"false" -> Avoid
| "accept"|"true" -> Accept
| "force" -> Enforce
| "local" -> Local
| _ -> failwith "Wrong observer mode, choose avoid, accept, force or local"

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
  ("-version", Arg.Unit (fun () -> print_endline Version.version ; exit 0),
   " show version number and exit")::
  Util.parse_tag
    "-arch"
    (fun tag -> match Archs.parse tag with
    | None -> false
    | Some a -> arch := a ; true)
    Archs.tags "specify architecture"::
  Util.parse_tag
    "-type"
    (fun tag -> match TypBase.parse tag with
    | None -> false
    | Some a -> typ := a ; true)
    TypBase.tags
    (sprintf "specify base type, default %s" (TypBase.pp !typ))::
   ("-o", Arg.String (fun s -> tarfile := Some s),
    "<name.tar> output litmus tests in archive <name.tar> (default, output in curent directory)")::
  ("-c", Arg.Bool (fun b ->  canonical_only := b),
   sprintf "<b> avoid equivalent cycles (default %b)" !canonical_only)::
  ("-list", Arg.Set list_edges,"list accepted edge syntax and exit")::
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
  ("-overload", Arg.Int (fun n -> overload := Some n),
   "<n> stress load unit by <n> useless loads")::
  ("-fno", Arg.String (fun s -> fno := parse_fno s), 
    "<no|prev|both|only|rf|all> fno mode, experimental (default no)")::
  ("-sta", Arg.Bool (fun b -> sta := b),
   "<bool> use atomic store (default false)")::
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
  ("-vars",Arg.Bool (fun _b -> eprintf "option -vars is deprecated, does nothing\n%!"),
   "<bool> deprecated, does nothing")::
  ("-oh", Arg.String (fun n -> hout := Some n),
   "<fname> save a copy of hints")::
  ("-addnum", Arg.Bool (fun n -> addnum := n),
   sprintf "<bool> complete test name with number when identical (default %b)"
     !addnum)::
   ("-name",Arg.String (fun s -> name := Some s),
     "<s> specify base name of tests")::
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

let prog = if Array.length Sys.argv > 0 then Sys.argv.(0) else "XXX"

let usage_msg = "Usage: " ^ prog ^   "[options]*"
                                             
let read_no fname =
  try
    Misc.input_protect
      (fun chan -> MySys.read_list chan (fun s -> Some s))
      fname
  with _ -> []
