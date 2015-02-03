(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Serves as an example of using DumpAll *)

(*
  "./critical n" outputs critical tests on n procs.
*)

open Misc
open Printf
open Archs

(* Configuration *)
let () = Config.nprocs := 1000
let () = Config.numeric := false

let opts =
  Config.common_specs @
  ("-num", Arg.Bool (fun b -> Config.numeric := b),
   sprintf "<bool> use numeric names, default %b" !Config.numeric)::
  []



module type Config = sig
  include Top.Config
  include DumpAll.Config
  val cpp : bool    
  val docheck : bool
end

module Make(O:Config) (M:Builder.S) =
  struct

(********)
    let rfe = M.E.parse_edge "Rfe"
    let fre = M.E.parse_edge "Fre"
    let coe = M.E.parse_edge "Wse"

(* direct coms *)
    let coms = [rfe; fre; coe ;]
(* coms through a write on another proc *)
    let coms2 = [[coe;rfe;];[fre;rfe]]

    let po =  M.E.parse_edge "Pod**"

    module D = DumpAll.Make(O)(M)

    let zyva n0 =
      let gen kont =
        let rec do_rec k n es =
          if n < 0 then k   (* es is too long *)
          else if n=0 then  (* es is ok *)
            kont es D.no_info D.no_name k
          else              (* recurse *)
            let k =
                List.fold_left
                (fun k com ->
                  do_rec k (n-1) (com::po::es))
                k coms in
            let k =
              List.fold_left
                (fun k com ->
                  do_rec k (n-2) (com@po::es))
                k coms2 in
            k in
        fun k -> do_rec k n0 [] in
      D.all gen

  end

let size = ref 2

let () =
  Util.parse_cmdline
    opts
    (fun x -> size := int_of_string x)

let cpp = match !Config.arch with
| CPP -> true
| _ -> false

let () =
  let module V = SymbConstant in
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
    let coherence_decreasing = !Config.coherence_decreasing
    let optcoherence = !Config.optcoherence
    let optcond = !Config.optcond
    let poll = !Config.poll
    let fno = !Config.fno
    let overload = !Config.overload
    let obs_type = !Config.obs_type
    let do_observers = !Config.do_observers
    let eprocs = !Config.eprocs
    let nprocs = !Config.nprocs
    let neg = !Config.neg
(* Specific *)
    let cpp = cpp
    let docheck = !Config.docheck
  end in
  let module Build = Make(Co) in
  let module C = struct
    let verbose = !Config.verbose
    let list_edges = !Config.list_edges
    let coherence_decreasing = !Config.coherence_decreasing
    let same_loc =
      !Config.same_loc ||
      (match Co.cond with
      | Config.Unicond -> true
      | _ -> false)
    let sta = !Config.sta
    let unrollatomic = !Config.unrollatomic
    let allow_back = true
  end in
  (match !Config.arch with
  | X86 ->
      let module T = Top.Make(Co) in
      let module M = Build(T(X86Compile.Make(V)(C))) in
      M.zyva
  | PPC ->
      let module T = Top.Make(Co) in
      let module M = Build(T(PPCCompile.Make(V)(C)(PPCArch.Config))) in
      M.zyva
  | ARM ->
      let module T = Top.Make(Co) in
      let module M = Build(T(ARMCompile.Make(V)(C))) in
      M.zyva
  | C|CPP as a ->
      let module CoC = struct
        include Co
        include C
        let typ = !Config.typ
        let cpp = match a with CPP -> true | _ -> false
      end in
      let module T = CCompile.Make(CoC) in
      let module M = Build(T) in
      M.zyva
)
  !size

