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

let arch = ref PPC
let names = ref []
let verbose = ref 0

let opts =
  ("-v",Arg.Unit (fun () -> incr verbose),"be verbose")::
  Util.parse_tag
    "-arch"
    (fun tag -> match Archs.parse tag with
    | None -> false
    | Some a -> arch := a ; true)
    Archs.tags
    "specify architecture"::[]

let () =
  Util.parse_cmdline
    opts
    (fun name -> names := name :: !names)

module Top = struct

(* Simplified relaxation: just strings... *)
  
  module R = struct
    open LexUtil

    type relax = LexUtil.t
    let parse r = r

    let pp_relax = function
      | One r -> r
      | Seq rs -> "[" ^ String.concat "," rs ^ "]"

    let rec lex_str r1 r2 = match r1,r2 with
    | [],[] -> 0
    | _::_,[] -> 1
    | [],_::_ -> -1
    | r1::rs1,r2::rs2 ->
        let c = String.compare r1 r2 in
        match c with
        | 0 -> lex_str rs1 rs2
        | _ -> c

    let compare r1 r2 = match r1,r2 with
    | One r1,Seq [r2]
    | Seq [r1],One r2
    | One r1,One r2
    | Seq [r1],Seq [r2] -> String.compare r1 r2
    | One _,Seq _ -> -1
    | Seq _,One _ -> 1
    | Seq r1,Seq r2 -> lex_str r1 r2

    module Set =
      MySet.Make
        (struct
          type t = relax
          let compare = compare
        end)

  let pp_relax_set chan t =
    fprintf chan "{" ;
    Set.pp chan ", "
      (fun chan r -> fprintf chan "%s" (pp_relax r))
      t ;
    fprintf chan "}"    

    module SetSet =  MySet.Make(Set)

    let pp_relax_set_set chan ts = SetSet.pp chan " " pp_relax_set ts

    let pp_nr chan ts = SetSet.pp chan "\n" pp_relax_set ts
  end

  module M = LogRelax.Make(R)
  open M

  let pp_relaxs rs = String.concat " " (List.map R.pp_relax rs)
    
  let pp_os chan =
    List.iter
      (fun o ->
        fprintf chan "%s %s With Safe %s\n"
          o.name
          (pp_relaxs o.relaxs)
          (pp_relaxs o.safes))

  module RelaxMap =
    Map.Make
      (struct
        type t = R.Set.t
        let compare = R.Set.compare
      end)

  let collect_relax os =
    List.fold_left
      (fun k o ->
        let relax = R.Set.of_list o.relaxs in
        let old =
          try RelaxMap.find relax k
          with Not_found -> R.SetSet.empty in
        RelaxMap.add relax
          (R.SetSet.add (R.Set.of_list o.safes) old)
          k)
      RelaxMap.empty os

  let collect_non_relax rmap os =    
    List.fold_left
      (fun k o ->
        let relax = R.Set.of_list o.relaxs in
        try
          ignore (RelaxMap.find relax rmap) ;
          k
        with Not_found -> R.SetSet.add relax k)
      R.SetSet.empty os

  let pp_map chan m =
    RelaxMap.iter
      (fun k v ->
        R.pp_relax_set chan k ;
        output_string chan " With " ;
        R.pp_relax_set_set chan v ;
        output_char chan '\n')
      m

  let pp_sumary chan os = pp_map chan  (collect_relax os)

  let suggest rmap nrset =
    let r =
      RelaxMap.fold
        (fun rs _ k -> R.Set.union rs k)
        rmap R.Set.empty
    and s =
      R.SetSet.fold
          (fun rs k -> R.Set.union rs k)
        nrset R.Set.empty in
    R.Set.diff s r

  let zyva names chan =
    let os = M.add_files names in
    let yes,no = List.partition (fun t -> t.validates) os in
    fprintf chan "** Relaxations **\n" ;
    pp_os chan yes ;
    fprintf chan "** Non-Relaxations **\n" ;
    pp_os chan no ;
    fprintf chan "** Relaxation summary **\n" ;
    let rmap = collect_relax yes in
    pp_map chan rmap ;
    if !verbose > 0 then begin
      fprintf chan "** Non-Relaxation summary **\n" ;
      let nrset = collect_non_relax rmap no in
      R.pp_nr chan nrset ;
      fprintf chan "\n" ;
      let sug = suggest rmap nrset in
      if not (R.Set.is_empty sug) then begin
        fprintf chan "** Safe suggestion **\n" ;
        R.pp_relax_set chan sug ;
        fprintf chan "\n"
      end
    end ;
    ()
    
end

let read_names = function
  | [] ->
      let rec read_rec k =
        let o =
          try Some (read_line ())
          with End_of_file -> None in
        match o with
        | Some x -> read_rec (x::k)
        | None -> k in
      read_rec []
  | xs ->  xs 

let names = read_names !names

let () = Top.zyva names stdout
