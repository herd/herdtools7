(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2012-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(* Sort a list of litmus tests by increasing number of threads,
   or according to given order.
   Also erase duplicates identified from hashes *)

open Printf

type duplicates = Keep | Comment | Delete

let parse_duplicates = function
  | "keep" -> Keep
  | "comment" -> Comment
  | "delete" -> Delete
  | tag ->
      raise
        (Arg.Bad
           (sprintf
              "wrong tag %s for -dups, allowed tags are <keep|comment|delete>"
              tag))

let pp_duplicates = function
  | Keep -> "keep"
  | Comment -> "comment"
  | Delete -> "delete"


module Top
    (Opt:
       sig
         val verbose : bool
         val duplicates : duplicates
         val reverse : bool
         val cost : string -> int
         val tnames : bool
         val keep_bad : bool
         val name_ok : string -> bool
       end) =
  struct

    module T = struct
      type t =
        { tname : string ;
          cost : int * int ;
          hash : string option; }
    end

    exception NotOk

    module Make(A:ArchBase.S) = struct

      let default_cost pgm =
        let nprocs = List.length pgm in
        let nins =
          List.fold_left
            (fun k (_,code) -> k+A.get_naccesses code)
            0 pgm in
        nprocs,nins

      let zyva name parsed =
        if not (Opt.name_ok  name.Name.name) then raise NotOk ;
        { T.tname = name.Name.name ;
          cost = default_cost parsed.MiscParser.prog ;
          hash = MiscParser.get_hash  parsed; }
    end

    module Z = ToolParse.Top(T)(Make)

    type name = {fname:string; tname:string;}

    let rec compare_names xs ys = match xs,ys with
    | [],[] -> 0
    | [],_::_ -> -1
    | _::_,[] -> 1
    | x::xs,y::ys ->
        begin match String.compare x.tname y.tname with
        | 0 -> compare_names xs ys
        | r -> r
        end

      let get_base f = Filename.chop_extension (Filename.basename f)

      let bigname f =
        let b = get_base f in
        let len = String.length b in
        let rec check i =
          if i >= len then false
          else match b.[i] with
          | 'A'..'Z' -> true
          | _ -> check (i+1) in
        check 0


      let fname_compare f1 f2 = match bigname f1,bigname f2 with
        | true,false -> -1
        | false,true -> 1
        | _,_ ->
            let x1 = String.length (get_base f1)
            and x2 =  String.length (get_base f2) in
            match Misc.int_compare x1 x2 with
            | 0 -> Misc.int_compare (String.length f1) (String.length f2)
            | r -> r


    let add_failed name failed = (* Keep smallest name for non-parsable files *)
      try
        let d = Digest.file name in
        try
          let old = StringMap.find d failed in
          if fname_compare name old < 0 then
            StringMap.add d name failed
          else
            failed
        with Not_found -> StringMap.add d name failed
      with
      | Sys_error _ ->
          Printf.eprintf "Cannot open file \"%s\"\n" name ;
          failed

    let do_test name (k,failed  as st) =
      try
        let {T.tname = tname;
             cost = c ;
             hash = h; } = Z.from_file name in
        let cx =
          try Opt.cost tname
          with Not_found ->
            Warn.fatal "no cost for test %s" tname in
        let cst = (cx,c),h in
        ({fname=name; tname=tname;},cst)::k,failed
      with
      | NotOk -> st
      | Misc.Exit -> k,add_failed name failed
      | Misc.Fatal msg|Misc.UserError msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 name msg ;
          k,add_failed name failed
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 name ;
          raise e

    let zyva tests =
      let empty = [],StringMap.empty in
      let xs,failed = Misc.fold_argv_or_stdin do_test tests empty in

      let get_min cmp = function
        | [] -> assert false
        | x::xs ->
            let rec  get_min_rec x = function
              | [] -> x
              | y::ys -> get_min_rec (if cmp x y < 0 then x else y) ys in
            get_min_rec x xs in


      let xs =
        let t = Hashtbl.create 17 in
        let see h f sz =
          try
            let _sz,fs = Hashtbl.find t h in
            assert (sz = _sz) ;
            Hashtbl.replace t h (sz,f::fs)
          with Not_found -> Hashtbl.add t h (sz,[f]) in
        List.iter (fun (f,(sz,h)) -> see h f sz) xs ;
        if Opt.verbose then begin
          Hashtbl.iter
            (fun _ (_,fs) -> match fs with
            | _::_::_ ->
                let fs = List.map (fun n -> n.fname) fs in
                eprintf "%s\n%!"
                  (String.concat " " fs)
            | _ -> ()) t
        end ;
        let fname_struct_compare f1 f2 = fname_compare f1.fname f2.fname in
        begin  match Opt.duplicates with
        | Keep|Comment ->
            Hashtbl.fold (fun _h (sz,fs) k ->
              (List.sort fname_struct_compare fs,sz)::k)
              t []
        | Delete ->
            Hashtbl.fold
              (fun _h (sz,fs) k -> ([get_min fname_struct_compare fs],sz)::k) t []
        end in

      let do_pint_compare (i1,j1) (i2,j2) =
        match Misc.int_compare i1 i2 with
        | 0 -> Misc.int_compare j1 j2
        | r -> r in

      let do_tint_compare (i1,j1) (i2,j2) =
        match Misc.int_compare i1 i2 with
        | 0 -> do_pint_compare j1 j2
        | r -> r in

      let tint_compare l1 l2 =
        let r = do_tint_compare l1 l2 in
        if Opt.reverse then (0-r) else r in

      let xs = List.sort
          (fun (n1,l1) (n2,l2) ->
            match tint_compare l1 l2 with
            | 0 -> compare_names n1 n2
            | r -> r) xs in

      let () =
        printf "#" ;
        for k = 0 to Array.length Sys.argv-1 do
          printf " %s" Sys.argv.(k)
        done ;
        printf "\n" ;
        let pname =
          if Opt.tnames then (fun n -> n.tname) else (fun n -> n.fname) in
        List.iter
          (fun (ns,(c1,(c2,c3))) ->
            if Opt.verbose then printf "#%i %i %i\n" c1 c2 c3;
            match Opt.duplicates with
            | Delete ->
                begin match ns with
                | n::_ -> printf "%s\n" (pname n)
                | [] -> assert false
                end
            | Keep ->
                List.iter (fun n -> printf "%s\n" (pname n)) ns
            | Comment ->
                begin match ns with
                | [n] ->  printf "%s\n" (pname n)
                | n::ns ->
                    printf "#DUPS\n" ;
                    printf "%s\n" (pname n) ;
                    List.iter (fun n -> printf "#%s\n" (pname n)) ns
                | [] -> assert false
                end)
          xs in
      if Opt.keep_bad && not (StringMap.is_empty failed) then begin
        printf "#Not parsed\n" ;
        StringMap.iter (fun _ fname -> printf "%s\n" fname) failed
      end
  end


let verbose = ref false
let duplicates = ref Delete
let arg = ref []
let orders = ref []
let reverse = ref false
let tnames = ref false
let keep_bad = ref true
let names = ref [] and excl = ref []

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "msort"

let () =
  Arg.parse
    ["-v",Arg.Unit (fun () -> verbose := true), " be verbose";
     "-d",Arg.Unit (fun () -> duplicates := Keep)," keep duplicates";
     "-dups",Arg.String (fun tag -> duplicates := parse_duplicates tag),
     sprintf
       "<keep|comment|delete> what to do with duplicates, default %s"
       (pp_duplicates !duplicates);
     "-r",Arg.Unit (fun () -> reverse := true)," reverse sort";
     "-t",Arg.Unit (fun () -> tnames := true)," output test names";
     "-keepbad",Arg.Bool (fun b -> keep_bad := b),sprintf "keep non-parsable tests, default %b" !keep_bad;
     "-cost",
     Arg.String (fun s -> orders := !orders @ [s]),
     "<name> specify order file";
     CheckName.parse_names names ;
     CheckName.parse_excl excl ;]
    (fun s -> arg := s :: !arg)
    (sprintf "Usage: %s [options]* [test]*" prog)

let tests = !arg

let parse_int s = try Some (int_of_string s) with _ -> None

module L = LexRename.Make(struct let verbose = if !verbose then 1 else 0 end)
let costs = L.read_from_files !orders parse_int

module Check =
  CheckName.Make
    (struct
      let verbose = if !verbose then 1 else 0
      let rename = []
      let select = []
      let names = !names
      let excl = !excl
    end)

module X =
  Top
    (struct
      let verbose = !verbose
      let duplicates = !duplicates
      let reverse = !reverse
      let cost = match !orders with
      | [] -> fun _s -> 0
      | _  -> TblRename.find_value costs
      let tnames = !tnames
      let keep_bad = !keep_bad
      let name_ok = Check.ok
    end)

let () = X.zyva tests
