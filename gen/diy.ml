(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut Natioal de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf
open Code
open LexUtil
open Archs

module type DiyConfig = sig
  include DumpAll.Config
  val choice : Code.check
  val prefix : LexUtil.t list option
  val cumul :   LexUtil.t list Config.cumul
  val max_ins : int
  val upto : bool
end

module Make(C:Builder.S)(O:DiyConfig) = struct

open C.E
open C.R

let parse_fence s k =  match s with
  | One f -> C.E.parse_fence f::k
  | Seq [] -> k
  | Seq (fs) ->
      List.fold_right
        (fun s k -> C.E.parse_fence s::k)
        fs k

let parse_relaxs = List.map parse_relax
let parse_edges = List.map parse_edge
let parse_fences fs = List.fold_right parse_fence fs []
  

module AltConfig = struct
  include O

  type relax = C.R.relax

  let prefix = match O.prefix with
  | None -> []
  | Some ps -> parse_relaxs ps

  type fence = C.A.fence

  let cumul =
    let open Config in
    match O.cumul with
    | Empty -> Empty
    | All -> All
    | Set ps -> Set (parse_fences ps)

end


  module M =  Alt.Make(C)(AltConfig)

      
  let gen lr ls n =
    let lr = C.R.expand_relax_macros lr
    and ls = C.R.expand_relax_macros ls in
    M.gen ~relax:lr ~safe:ls n

  let er e = ERS [plain_edge e]

  let gen_thin n =
    let lr = [er (Rf Int); er (Rf Ext)]
    and ls = [PPO] in
    M.gen ~relax:lr ~safe:ls n


  let gen_uni n =
    let lr = [er (Rf Int); er (Rf Ext)]
    and ls =
      [er (Ws Int); er (Ws Ext); er (Fr Int);
       er (Fr Ext); er (Po (Same,Irr,Irr))] in
    M.gen ~relax:lr ~safe:ls n


  let parse_edges_opt = function
    | None -> None
    | Some xs -> Some (parse_edges xs)

  let go n (*size*) olr ols (*relax and safe lists*) _olc (*one + arg*) =
    match O.choice with
    | Sc|Critical|Free|Ppo|Transitive|Total ->
        begin match olr,ols with
        | None,None -> M.gen n
        | None,Some ls -> gen [] ls n
        | Some lr,None -> gen lr [] n
        | Some lr,Some ls -> gen lr ls n
        end
    | Thin -> gen_thin n 
    | Uni ->
        begin match olr,ols with
        | None,None -> gen_uni n
        | None,Some ls -> gen [] ls n
        | Some lr,None -> gen lr [] n
        | Some lr,Some ls -> gen lr ls n
        end
end


let split s = match s with
  | None -> None
  | Some s ->
  let splitted = LexUtil.split s in
  Some splitted

let lc = ref []

let get_arg s =
  lc := (!lc)@[s]

let norm_cmd cmd = 
  match cmd with
  | [] -> assert false
  | _::s -> 
    let rec no_conf l = 
      match l with
      | [] -> []
      | "-conf"::_::l -> no_conf l
      | s::l -> s::(no_conf l)
    in no_conf s

let exec_conf s =
  let conf = Misc.input_protect LexConf.conf s in
  let prog = Sys.argv.(0) in
  let cmd = Array.to_list Sys.argv in
  let cmd = norm_cmd cmd in
  if !Config.verbose > 1 then
    eprintf "EXEC: %s %s\n%!" prog (String.concat " " (conf @ cmd)) ;
  ignore (Unix.execvp prog (Array.of_list (prog::conf@cmd))) ;
  ()

let () =
  Arg.parse (Config.speclist) get_arg Config.usage_msg;
  begin
  match !Config.conf with
  | None -> ()
  | Some s -> exec_conf s
  end;
  let relax_list = split !Config.relaxs
  and safe_list = split !Config.safes 
  and one_list = if !Config.one then Some !lc else None in

  let cpp = match !Config.arch with CPP -> true  |  _ -> false in

  let module Co = struct
(* Dump all *)
    let verbose = !Config.verbose
    let hout = match !Config.hout with
    | None -> Hint.none
    | Some n -> Hint.open_out n
    let family = !Config.name
    let canonical_only = !Config.canonical_only
    let fmt = !Config.fmt
    let no = match !Config.no with
    | None -> []
    | Some fname -> Config.read_no fname
    let cond = !Config.cond
    let tarfile = !Config.tarfile
    let addnum = !Config.addnum
    let numeric = !Config.numeric
    let lowercase = !Config.lowercase
(* Specific *)
    open Config
    let choice = !Config.mode
    let prefix =  split !Config.prefix
    let cumul = match !Config.cumul with
    | Empty -> Empty
    | All -> All
    | Set s -> Set (LexUtil.split s)
    let coherence_decreasing = !Config.coherence_decreasing
    let upto = !Config.upto
    let max_ins = !Config.max_ins
    let overload = !Config.overload
    let poll = !Config.poll
    let optcoherence = !Config.optcoherence
    let optcond = !Config.optcond
    let fno = !Config.fno
    let obs_type = !Config.obs_type
    let do_observers = !Config.do_observers
    let eprocs = !Config.eprocs
    let nprocs = !Config.nprocs
    let neg = !Config.neg
    let cpp = cpp
    let docheck = !Config.docheck
 end in
  let module C = struct
    let verbose = !Config.verbose
    let list_edges = !Config.list_edges
    let coherence_decreasing = !Config.coherence_decreasing 
    let same_loc =
      !Config.same_loc ||
      (match Co.choice  with Uni -> true | _ -> false)
    let sta = !Config.sta
    let unrollatomic = !Config.unrollatomic
    let allow_back = match !Config.mode with
    | Sc|Critical|Thin -> false
    | _ -> true          
  end in
  let module T = Top.Make(Co) in
  let f = match !Config.arch with
  | PPC ->
      let module M = Make(T(PPCCompile.Make(C)(PPCArch.Config)))(Co) in
    M.go
  | X86 ->
    let module M = Make(T(X86Compile.Make(C)))(Co) in
    M.go
  | ARM ->
      let module M = Make(T(ARMCompile.Make(C)))(Co) in
      M.go
  | AArch64 ->
      let module M = Make(T(AArch64Compile.Make(C)))(Co) in
      M.go
  | MIPS ->
      let module M = Make(T(MIPSCompile.Make(C)))(Co) in
      M.go
  | C|CPP ->
      let module CoC = struct
        include Co
        include C
        let typ = !Config.typ
      end in
      let module M = Make(CCompile.Make(CoC))(Co) in
      M.go in
  try
    f !Config.size relax_list safe_list one_list ;
    exit 0
  with
  | Misc.Fatal msg | Misc.UserError msg->
    eprintf "%s: Fatal error: %s\n" Config.prog msg ;
    exit 2
