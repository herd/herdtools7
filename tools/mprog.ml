(*********************************************************************)
(*                       DIY                                         *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(* Pretty print tests *)

open Printf


module Top
    (O:
       sig
         val verbose : int
         val texmacros : bool
         val hexa : bool
         val outputdir : string option
         val mode : OutMode.t
         val transpose : bool
       end) =
  struct

    module T = struct
      type t = unit
    end

    (* Transpose dump *)
    module Transpose(A:ArchBase.S) = struct

      module D =
        TransposeDumper.Make
          (struct
            module A = A
            type prog =  (int * A.pseudo list) list

            let dump_loc = MiscParser.dump_location
            let dump_reg r = r

            let dump_state_atom dump_loc a =
              MiscParser.dump_state_atom dump_loc SymbConstant.pp_v a

            type state = MiscParser.state

            let add_loc v k =
              MiscParser.LocSet.add (MiscParser.Location_global v) k

            let rec get_addrs_ins k = function
              | A.Nop -> k
              | A.Label (_,i) -> get_addrs_ins k i
              | A.Instruction i ->
                  A.fold_addrs add_loc k i
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
                    else (loc,(MiscParser.TyDef,SymbConstant.intToV 0))::k)
                  gs [] in
              let st = global_st @ zeros in
              String.concat " "
                (List.map
                   (fun a -> sprintf "%s;" (dump_state_atom dump_loc a))
                   st)

            let dump_proc_state p st =
              let st =
                List.fold_right
                  (fun (loc,v) k ->
                    match MiscParser.as_local_proc p loc with
                    | Some reg -> (reg,v)::k
                    | None -> k)
                  st [] in
              String.concat " "
                (List.map
                   (fun a -> sprintf "%s;" (dump_state_atom dump_reg a))
                   st)
                
            type constr = MiscParser.constr
            let dump_atom a =
              let open ConstrGen in
              match a with
              | LV (loc,v) ->
                  sprintf "%s=%s" (dump_loc loc)  (SymbConstant.pp_v v)
              | LL (loc1,loc2) ->
                  sprintf "%s=%s" (dump_loc loc1) (MiscParser.dump_rval loc2)

            let dump_constr = ConstrGen.constraints_to_string dump_atom

            type location = MiscParser.location
            let dump_location = dump_loc
          end)

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

    (* Text dump *)
    module Text(A:ArchBase.S) = struct

      module D =
        SimpleDumper.Make
          (struct
            module A = A

            let dump_loc = MiscParser.dump_location

            let dump_state_atom a =
              MiscParser.dump_state_atom dump_loc SymbConstant.pp_v a

            type state = MiscParser.state

            let dump_state st =
              String.concat " "
                (List.map
                   (fun a -> sprintf "%s;" (dump_state_atom a))
                   st)

                
            type constr = MiscParser.constr
            let dump_atom a =
              let open ConstrGen in
              match a with
              | LV (loc,v) -> dump_state_atom (loc,(MiscParser.TyDef,v))
              | LL (loc1,loc2) ->
                  sprintf "%s=%s" (dump_loc loc1) (MiscParser.dump_rval loc2)

            let dump_constr = ConstrGen.constraints_to_string dump_atom

            type location = MiscParser.location
            let dump_location = dump_loc
          end)

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

    module Latex(A:ArchBase.S) = struct
      module Arch = ArchExtra.Make(O)(A)
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
      let texmacros = !texmacros
      let hexa = !hexa
      let outputdir = !outputdir
      let mode = !mode
      let transpose = !transpose
    end)

let () =
  Misc.iter_argv
    (fun fname ->
      try X.zyva fname with
      | Misc.Exit -> ()
      | Misc.Fatal msg ->
          Warn.warn_always "%a %s" Pos.pp_pos0 fname msg ;
          ()
      | e ->
          Printf.eprintf "\nFatal: %a Adios\n" Pos.pp_pos0 fname ;
          raise e)
    !args

