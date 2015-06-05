(*********************************************************************)
(*                        Memevents                                  *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(* Susmit Sarkar, Peter Sewell, University of Cambridge, UK.         *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

(** Constraints in litmus files *)

open Printf

module type Config = sig
  val texmacros : bool
  val hexa : bool
end

module type S = sig
  module A : Arch.S

  type prop = (A.location,A.V.v) ConstrGen.prop

  val ptrue : prop

  type constr = prop ConstrGen.constr

(* Does loc appears in constr ? *)
  val loc_in : A.location -> constr -> bool

  val foralltrue : constr

(* Check state *)
  val check_prop : prop -> A.state -> bool
  val check_constr : constr -> A.state list -> bool

(* Build a new constraint thar checks State membership *)
  val constr_of_finals : A.StateSet.t -> constr

(* Parsable dumping *)
  val dump_as_kind : 'a ConstrGen.constr -> string
  val dump_constraints : out_channel -> constr -> unit
  val constraints_to_string : constr -> string

(* Nice printing *)
  val pp_as_kind :  'a ConstrGen.constr -> string
  val pp_constraints : PPMode.t -> constr -> string

end

open ConstrGen


module Make (C:Config) (A : Arch.S) :
    S with module A = A
	=
      struct
	module A = A
(************ Constraints **************************)
	module V = A.V

	type prop = (A.location,V.v) ConstrGen.prop

	let ptrue : prop = And []

	type constr = prop ConstrGen.constr

	let foralltrue = ForallStates ptrue

        let loc_in_atom loc = function
          | LL (l1,l2) ->
              A.location_compare l1 loc = 0 ||
              A.location_compare l2 loc = 0
          | LV (l,_) ->
              A.location_compare l loc = 0

        let rec loc_in_prop loc p = match p with
        | Atom a -> loc_in_atom loc a
        | Not p -> loc_in_prop loc p
        | And ps|Or ps -> loc_in_props loc ps
        | Implies (p1,p2) ->
            loc_in_prop loc p1 || loc_in_prop loc p2

        and loc_in_props loc =  List.exists (loc_in_prop loc)

        let loc_in loc c = match c with
        | ForallStates p
        | ExistsState p
        | NotExistsState p -> loc_in_prop loc p

	let rec check_prop p state = match p with
	| Atom (LV (l,v)) -> A.state_mem state l v
        | Atom (LL (l1,l2)) ->
            begin try
              let v1 = A.look_in_state state l1
              and v2 = A.look_in_state state l2 in
              A.V.compare v1 v2 = 0
            with A.LocUndetermined -> assert false end
	| Not p -> not (check_prop p state)
	| And ps -> List.for_all (fun p -> check_prop p state) ps
	| Or ps -> List.exists (fun p -> check_prop p state) ps
	| Implies (p1, p2) -> 
	    if check_prop p1 state then check_prop p2 state else true
	      
	let check_constr c states = match c with
	| ForallStates p -> List.for_all (fun s -> check_prop p s) states
	| ExistsState p -> List.exists (fun s -> check_prop p s) states
	| NotExistsState p ->
            not (List.exists (fun s -> check_prop p s) states)	      

        let matrix_of_states fs =
          A.StateSet.fold
            (fun f k -> A.state_to_list f::k)
            fs []

        module OV = struct
          type t = A.V.v
          let compare = A.V.compare
        end

        module VSet = MySet.Make(OV)
        module VMap = Map.Make(OV)

        let best_col m =
          let mt = Misc.transpose m in
          let cs =
            List.map
              (fun col ->
                let vs = List.map (fun (_,v) -> v) col in
                VSet.cardinal (VSet.of_list vs))
              mt in
          let rec best_rec k (kb,b as p) = function
            | [] -> kb
            | c::cs ->
                if c < b then best_rec (k+1) (k,c) cs
                else best_rec (k+1) p cs in
          best_rec 0 (-1,max_int) cs

        let swap_list =
          let rec swap_list k prev xs = match xs with
          | [] -> assert false
          | x::xs ->
              if k <= 0 then
                x::List.rev_append prev xs
              else
                swap_list (k-1) (x::prev) xs in
          fun k xs -> swap_list k [] xs

                  
        let swap_col k m =
          let mt = Misc.transpose m in
          let mt = swap_list k mt in
          Misc.transpose mt
          
        let extract_column xss = match xss with
        | []|[]::_ -> assert false
        | ((loc0,_)::_)::_ ->
            loc0,
            List.map
              (fun row -> match row with
              | (loc,v)::ps ->
                  assert (A.location_compare loc0 loc = 0) ;
                  v,ps
              | [] -> assert false)
              xss

        let group_rows ps =
          List.fold_left
            (fun m (v,ps) ->
              let pss =
                try VMap.find v m
                with Not_found -> [] in
              VMap.add v (ps::pss) m)
            VMap.empty ps
          
        let rec compile_cond m = 
          let k = best_col m in
          let loc,ps = extract_column (swap_col k m) in
          let m = group_rows ps in
          match ps with
          | [] -> assert false
          | (_,[])::_ ->
              Or 
                (VMap.fold
                   (fun v _ k -> Atom (LV (loc,v))::k)
                   m [])
          | _ ->
              Or
                (VMap.fold
                   (fun v m k -> And [Atom (LV (loc,v));compile_cond m]::k)
                   m [])

        let cond_of_finals fs = compile_cond (matrix_of_states fs)
            
        let constr_of_finals fs = ForallStates (cond_of_finals fs)

(* Pretty print *)
        open PPMode
        let pp_equal m = match m with
        | Ascii|Dot -> "="
        | Latex -> "\\mathord{=}"
        | DotFig -> "\\\\mathord{=}"

        let pp_true m = match m with
        | Ascii|Dot -> "true"
        | Latex -> "\\top"
        | DotFig -> "\\\\top"

        let pp_false m = match m with
        | Ascii|Dot -> "false"
        | Latex -> "\\perp"
        | DotFig -> "\\\\perp"

        let pp_not m = match m with
        | Ascii|Dot -> "not"
        | Latex -> "\\neg"
        | DotFig -> "\\\\neg"

        let pp_and m = match m with
        | Ascii -> "/\\"
        | Dot -> "/\\\\"
        | Latex -> "\\mywedge"
        | DotFig -> "\\\\mywedge"

        let pp_or m = match m with
        | Ascii -> "\\/"
        | Dot -> "\\\\/"
        | Latex -> "\\vee"
        | DotFig -> "\\\\vee"

        let pp_implies m = match m with
        | Ascii|Dot -> "=>"
        | Latex -> "\\Rightarrow"
        | DotFig -> "\\\\Rightarrow"

        let mbox m s = match m with
        | Ascii|Dot -> s
        | Latex -> "\\mbox{" ^ s ^ "}"
        | DotFig -> "\\\\mbox{" ^ s ^ "}"
                                        

        let pp_rvalue loc = match loc with
          | A.Location_global _ -> sprintf "*%s" (A.pp_location loc)
          | _ -> A.pp_location loc

        let pp_atom m a =
          match a with
          | LV (loc,v) ->
              mbox m (A.pp_location loc) ^
              pp_equal m ^          
              mbox m
                (let v = V.pp C.hexa v in
                let add_asm =C.texmacros in
                match m,add_asm with
                | ((Ascii|Dot),_)
                | ((Latex|DotFig),false) -> v
                | Latex,true -> sprintf "\\asm{%s}" v
                | DotFig,true -> sprintf "\\\\asm{%s}" v)
          | LL (l1,l2) ->
              mbox m (A.pp_location l1) ^
              pp_equal m ^ 
              mbox m (pp_rvalue l2)
            
(* ascii, parsable dump *)
        let dump_as_kind c = pp_kind (kind_of c)          
        let dump_constraints chan =
          ConstrGen.dump_constraints chan (pp_atom Ascii)
          
        let constraints_to_string =
          ConstrGen.constraints_to_string  (pp_atom Ascii)
(* pretty_print *)

        let arg m =
          { pp_true = pp_true m;
            pp_false = pp_false m;
            pp_not = pp_not m;
            pp_or = pp_or m;
            pp_and = pp_and m;
            pp_implies = pp_implies m;
            pp_mbox = mbox m;
            pp_atom = pp_atom m; }

        let pp_as_kind c = 
          let bodytext = pp_kind (kind_of c) in
          if C.texmacros then
            bodytext ^ " Final State" else bodytext

        let pp_constraints m =
          let endollar m s = match m with
          | Ascii|Dot -> s
          | Latex -> "$" ^ s ^ "$"
          | DotFig -> "$" ^ s ^ "$" in
          let pp_prop p = endollar m (ConstrGen.pp_prop (arg m) p) in
          fun c ->  match c with
          | ExistsState p
          | NotExistsState p
          | ForallStates p ->
              pp_as_kind c ^ ": "^ pp_prop p

      end
