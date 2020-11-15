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

(* Pretty print tests *)

open Printf


module Top
    (O:
       sig
         val verbose : int
         val withindex : bool
         val ascommands : bool
         val texmacros : bool
         val hexa : bool
         val outputdir : string option
         val mode : OutMode.t
         val transpose : bool
         val alloc : bool
       end) =
  struct

    let () = ignore O.verbose

    module T = struct
      type t = unit
    end

    (* Transpose dump *)
    module Transpose(A:ArchBase.S) = struct

      module D =
        TransposeDumper.Make
          (struct
            module A = A
            type prog =  (MiscParser.proc * A.pseudo list) list

            type v = ParsedConstant.v
            let dump_v = ParsedConstant.pp_v

            let dump_loc = MiscParser.dump_location
            let dump_reg r = r

            let dump_state_atom dump_loc a = MiscParser.dump_state_atom dump_loc dump_v a

            type state = MiscParser.state

            let add_loc v k =
              MiscParser.LocSet.add (MiscParser.Location_global v) k

            let rec get_addrs_ins k = function
              | A.Nop -> k
              | A.Label (_,i) -> get_addrs_ins k i
              | A.Instruction i ->
                  A.fold_addrs add_loc k i
	      | A.Symbolic _
              | A.Macro _ -> assert false


            let dump_global_state prog st =
              let global_st =
                List.filter (fun (loc,_) -> MiscParser.is_global loc) st in
(* Compute global location referenced from code and init *)
              let gs =
                List.fold_left
                  (fun k (_,code) ->
                    List.fold_left get_addrs_ins k code)
                  MiscParser.LocSet.empty prog in
              let gs =
                List.fold_left
                  (fun k (_,(_,v)) -> match v with
                  | Constant.Symbolic _ as loc -> add_loc loc k
                  | _ -> k) gs st in

              let zeros =
                MiscParser.LocSet.fold
                  (fun loc k ->
                    if
                      List.exists
                        (fun (loc0,_) ->
                          MiscParser.location_compare loc loc0 = 0)
                        global_st
                    then k
                    else (loc,(TestType.TyDef,ParsedConstant.intToV 0))::k)
                  gs [] in
              let st = global_st @ zeros in
              String.concat " "
                (List.map
                   (fun a -> sprintf "%s;" (dump_state_atom dump_loc a))
                   st)

            let ignore_reg _r () = ()
            let collect_sym = StringSet.add
            let collect_regs = ignore_reg,collect_sym

            let collect_ins syms i =
              let _,syms =
                A.pseudo_fold
                  (A.fold_regs collect_regs)
                  ((),syms) i in
              syms

            let collect_code code =
              List.fold_left collect_ins StringSet.empty code

            let dump_proc_state p code st =
              let syms = collect_code code in
              let st =
                List.fold_right
                  (fun (loc,v) k ->
                    match MiscParser.as_local_proc p syms loc with
                    | Some reg -> (reg,v)::k
                    | None -> k)
                  st [] in
              match st with
              | [] -> None
              | _ ->
                  let pp =
                    String.concat " "
                      (List.map
                         (fun a -> sprintf "%s;" (dump_state_atom dump_reg a))
                         st) in
                  Some pp

            type prop = MiscParser.prop

            let dump_atom a =
              ConstrGen.dump_atom dump_loc MiscParser.dump_rval
                ParsedConstant.pp_v a

            let dump_prop = ConstrGen.prop_to_string dump_atom
            let dump_constr = ConstrGen.constraints_to_string dump_atom

            type location = MiscParser.location
            let dump_location = dump_loc
          end)

      let dump = D.dump (* Or D.dump_info *)
      let zyva = match O.outputdir with
      | None -> dump stdout
      | Some d ->
          fun name parsed ->
            let fname = name.Name.file in
            let fname = Filename.basename fname in
            let fname = Filename.concat d fname in
            Misc.output_protect
              (fun chan -> dump chan name parsed)
              fname

    end

    (*************)
    (* Text dump *)
    (*************)

    (* No alloc *)
    module Text(A:ArchBase.S) = struct
      module D = DumperMiscParser.Make(O)(SimpleDumper.OutChannel)(A)

      let zyva = match O.outputdir with
      | None -> D.dump_info stdout
      | Some d ->
          fun name parsed ->
            let fname = name.Name.file in
            let fname = Filename.basename fname in
            let fname = Filename.concat d fname in
            Misc.output_protect
              (fun chan -> D.dump_info chan name parsed)
              fname

    end

    (* Some alloc *)
    module TextAlloc(A:ArchBase.S) = struct
      module Arch = ArchExtra_tools.Make(O)(A)
      module Alloc = SymbReg.Make(Arch)
      module D = Dumper.Make(Arch)

      let zyva = match O.outputdir with
      | None ->
          fun name parsed ->
            D.dump_info stdout name (Alloc.allocate_regs parsed)
      | Some d ->
          fun name parsed ->
            let fname = name.Name.file in
            let fname = Filename.basename fname in
            let fname = Filename.concat d fname in
            Misc.output_protect
              (fun chan ->
                D.dump_info chan name (Alloc.allocate_regs parsed))
              fname

    end

    module Latex(A:ArchBase.S) = struct
      module Arch = ArchExtra_tools.Make(O)(A)
      module M = PrettyProg.Make(O)(Arch)
      module Alloc = SymbReg.Make(Arch)

      let zyva name (parsed : A.pseudo MiscParser.t) =
        let parsed = Alloc.allocate_regs parsed in
        M.dump_prog name parsed

    end

    open OutMode

    let zyva =
      if O.transpose then
        let module Z =  ToolParse.Top(T)(Transpose) in
        Z.from_file
      else match O.mode with
      | Txt ->
          if O.alloc then
            let module Z =  ToolParse.Top(T)(TextAlloc) in
            Z.from_file
          else
            let module Z =  ToolParse.Top(T)(Text) in
            Z.from_file
      | LaTeX|HeVeA|HeVeANew ->
          let module Z =  ToolParse.Top(T)(Latex) in
          Z.from_file

  end

(***********************)
let args = ref []
let verbose = ref 0
let texmacros = ref false
let hexa = ref false
let outputdir = ref None
let mode = ref OutMode.LaTeX
let transpose = ref false
let alloc = ref false
let opts =
  [
   "-v",Arg.Unit (fun () -> incr verbose), " be verbose";
   "-texmacros", Arg.Bool (fun b -> texmacros := b),
   (sprintf "<bool> use latex macros in output, default %b" !texmacros);
   "-hexa", Arg.Bool (fun b -> hexa := b),
   (sprintf "<bool> hexadecimal output, default %b" !hexa);
   begin let module P = ParseTag.Make(OutMode) in
   P.parse "-mode" mode "output mode" end ;
   "-transpose", Arg.Bool (fun b -> transpose := b),
   (sprintf "<bool> show code proc by proc, default %b" !transpose);
   "-alloc", Arg.Bool (fun b -> alloc := b),
   (sprintf "<bool> alloc symbolic registers (text mode only), default %b" !alloc);
   "-o", Arg.String (fun s -> outputdir := Some s),
   "<name>  all output in directory <name>";
 ]

let prog =
  if Array.length Sys.argv > 0 then Sys.argv.(0)
  else "mprog"

let () =
  Arg.parse opts
    (fun s -> args := !args @ [s])
    (sprintf "Usage: %s [options]* [test]*" prog)


module X =
  Top
    (struct
      let verbose = !verbose
      let withindex = false
      let ascommands = false
      let texmacros = !texmacros
      let hexa = !hexa
      let outputdir = !outputdir
      let mode = !mode
      let transpose = !transpose
      let alloc = !alloc
    end)

let () =
  Misc.iter_argv_or_stdin
    (fun fname ->
      try X.zyva fname with
      | Misc.Exit -> ()
      | Misc.Fatal msg|Misc.UserError msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 fname msg ;
          ()
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 fname ;
          raise e)
    !args
