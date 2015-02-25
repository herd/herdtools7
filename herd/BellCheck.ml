(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* Tyler Sorensen, University College London, UK.                    *)
(*                                                                   *)
(*  Copyright 2015 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module Make(C:sig val bell_model_info : Bell_info.model option end)(A:Arch.S) =
 struct
  let check_regions r bi = 
    let valid_regions = Bell_info.check_regions r bi in
    if not valid_regions then
      Warn.user_error "Regions in test do not match regions in bell" 
	
  let check_scopes s bi = 
    let valid_scopes = Bell_info.check_scopes s bi in
    if not valid_scopes then
      Warn.user_error "Scopes in test do not match scopes in bell" 

  let check_annots t annot_list bell_info i = 
    let valid_annots = Bell_info.check_annots t annot_list bell_info
    in
    if not valid_annots then
      Warn.user_error
        "unable to match instruction in test with bell declaration: %s" 
	(A.dump_instruction i)

  let check_instruction i bell_info = 
    let id,annot_list = A.get_id_and_list i in
    match id with
    | "R"|"W"|"F"|"RMW" ->
	check_annots id annot_list bell_info i          
    | _ -> ()

  let do_check parsed bi = 
    (* checking instructions *)
    List.iter (fun (_,instr) ->
      List.iter (fun i ->
	match i with
	| A.Instruction i -> check_instruction i bi
	| _ -> ()) instr ) parsed.MiscParser.prog; 
    
    let test_bi = parsed.MiscParser.bell_info in            
    let test_bi = match test_bi with
    | Some b -> b
    | None -> Warn.fatal "Error getting bell information from test"
    in
    begin match test_bi.Bell_info.regions with 
    | Some r -> check_regions r bi
    | _ -> ()
    end ;
    begin match test_bi.Bell_info.scopes with 
    | Some s -> check_scopes s bi	  
    | _ -> ()
    end ;
    ()

  let check = match C.bell_model_info with
  | None -> fun _ -> ()
  | Some bi -> fun parsed -> do_check parsed bi
end
