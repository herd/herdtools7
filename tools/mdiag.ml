(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2015-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)
open Printf

module Top
    (Opt:
       sig
         val verbose : int
       end) =
  struct
    let () = ignore Opt.verbose

    open TestInfo

    let do_test name k =
      try Z.from_file name::k
      with
      | Misc.Exit -> k
      | Misc.Fatal msg|Misc.UserError msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 name msg ;
          k
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
          raise e

    module TSet = MySet.Make(T)

    let is_singleton s  = match StringSet.as_singleton s with
    | Some _ -> true
    | None -> false

    let add k v m =
      let old = StringMap.safe_find TSet.empty k m in
      StringMap.add k (TSet.add v old) m

    let zyva tests =
      let tests = match tests with
      | [] -> Misc.fold_stdin do_test []
      | _::_ -> Misc.fold_argv do_test tests [] in
      let tests = TSet.of_list tests in
      let byName,byHash =
        TSet.fold
          (fun t (byName,byHash) -> add t.T.tname t byName,add t.T.hash t byHash)
          tests (StringMap.empty,StringMap.empty) in            
      StringMap.iter
        (fun tname ts ->
          let hashes = TSet.fold (fun t k -> t.T.hash::k) ts [] in
          let hashes = StringSet.of_list hashes in
          if not (is_singleton hashes) then begin
            printf "Error: name %s has different hashes\n"  tname ;
            StringSet.iter
                (fun hash ->                  
                  let fnames =
                    try
                      let ts = StringMap.find hash byHash in
                      let fnames =
                        TSet.fold
                          (fun t k ->
                            if t.T.tname = tname then t.T.fname::k else k)
                          ts [] in
                      StringSet.of_list fnames
                    with Not_found -> StringSet.empty in
                  if not (StringSet.is_empty fnames) then
                    printf " %s\n"
                      (StringSet.pp_str "," Misc.identity fnames))
              hashes
          end)
        byName ;
      StringMap.iter
        (fun _hash ts ->
          let names = TSet.fold (fun t k -> t.T.tname::k) ts [] in
          let names = StringSet.of_list names in
          if not (is_singleton names) then
            printf "Warning: tests {%s} are the same test\n"
              (StringSet.pp_str "," Misc.identity names))
        byHash ;
      
      StringMap.iter
        (fun name ts ->
          let fnames = TSet.fold (fun t k -> t.T.fname::k) ts [] in
          let fnames = StringSet.of_list fnames in
          if not (is_singleton fnames) then begin
            printf "Warning: test %s is referenced more than once:\n"
              name ;
            StringSet.iter
              (fun fname -> printf "  %s\n" fname)
              fnames
          end)
        byName ;
      ()
  end


let verbose = ref 0
let arg = ref []
let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mdiag"

let () =
  Arg.parse
    ["-v",Arg.Unit (fun () -> incr verbose), " be verbose";]
    (fun s -> arg := !arg @ [s])
    (sprintf "Usage: %s [options]* [test]*" prog)

let tests = List.rev !arg

module X =
  Top
    (struct
      let verbose = !verbose
    end)

let () = X.zyva tests
