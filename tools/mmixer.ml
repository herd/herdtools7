(*********************************************************************)
(*                          DIY                                      *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(*                                                                   *)
(*  Copyright 2014 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Rewrite of mixer from offence, by Luc Maranget and Jade Alglave *)

open Printf
open MixOption

module Top (Opt:MixOption.S) = struct

      module T = struct
        type t = unit
      end


      module Make(A:ArchBase.S) = struct
        module Arch=ArchExtra.Make(Opt)(A)
        module D = Dumper.Make(Arch)
        module M = MixMerge.Make(Opt)(Arch)
        module Alloc = SymbReg.Make(Arch)

        let merge =
          let open Action in
          match Opt.action with
          | Append -> M.append 
          | Mix -> M.merge
          | Action.Cat -> M.cat

        open Name

        let mk_name f = match Opt.name with
        | Some n -> n
        | None -> f ()

        let double_doc doc =
          { doc with
            name = mk_name (fun () -> "2+" ^ doc.name);
            doc = "2+" ^ doc.doc ; }
          
        let mix_doc doc1 doc2 =
          { doc1 with
            name = mk_name (fun () -> doc1.name ^ "+" ^ doc2.name) ;
            doc = doc1.doc ^ "+" ^ doc2.doc ; }

        let merge2 (doc1,t1) (doc2,t2) =
          mix_doc doc1 doc2,merge doc1 t1 doc2 t2

        let rec merges  = function
          | [] -> assert false
          | [dt] -> dt
          | dt::dts -> merge2 dt (merges dts)

        let zyva nps =
          let nps =
            List.map
              (fun (n,parsed) -> n,Alloc.allocate_regs parsed)
              nps in
          let n,p = 
            match nps with
            | [d,_ as dt] -> 
                let _,t = merges [dt;dt] in
                double_doc d,t
            | _ ->  merges nps in
          D.dump stdout n p

      end

      module Z = ToolParse.Tops(T)(Make)

      let zyva = Z.from_files

    end


(* Parse command line *)
let verbose = ref 0
let what = ref Action.Mix
let permut = ref Permut.Random
let cond = ref Cond.Auto
let name = ref None
let arg = ref []

let prog = if Array.length Sys.argv > 0 then Sys.argv.(0) else "mmixer"

let usage = sprintf "Usage: %s [options]* file1 [file2]" prog

let () =
  Arg.parse
    [
     ("-v", Arg.Unit (fun () -> incr verbose), "be verbose");
     ("-name",Arg.String (fun s -> name := Some s), "name of output test");
     begin let module P = ParseTag.Make(Action) in
     P.parse "-a" what "action performed" end ;
     begin let module P = ParseTag.Make(Permut) in
     P.parse "-p" permut "specify permutation" end ;
     begin let module P = ParseTag.Make(Cond) in
     P.parse "-c" cond "specify condition merge" end ;
   ]
    (fun s -> arg := !arg @ [s])
    usage

module X = 
  Top
    (struct
      let verbose = !verbose
      let action = !what
      let permut = !permut
      let cond = !cond
      let name = !name
      let hexa = false
    end)


let () = 
  try X.zyva !arg
  with Misc.Fatal msg -> eprintf "%s: %s\n" prog msg ; exit 2
