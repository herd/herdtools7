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

open Printf
open OutMode

module type Config = sig
  val mode : OutMode.t
  val chan : out_channel
  val verbose : int
  val show_empty_rows : bool
  val kinds : LogState.kind TblRename.t
  val conds : LogConstr.constr TblRename.t
  val orders : unit TblRename.t
end

type cell = string list
type column = cell array
type matrix = column list



(****************)
(* Simple Build *)
(****************)

module type I = sig
  type info 

(*
 Build information matrix from test result matrix.
 Arguments:
    + log (ie column in log underlying matrix)
    + key (ie characterisation of row)
    + test result itself
*)

  val fmt_cell : LogState.t -> info -> LogState.test -> cell

(* Add function *)
  type v
  val add : v -> LogState.test -> v
end

module NoAdd = struct
  type v = unit
  let add () _t = ()
end

module Build (I:I) = struct

  type key = I.info Key.t
  let nope = ["---"]

  let extract col ks ts =
    let sz_ks = Array.length ks
    and sz_ts = Array.length ts in
    let tout = ExtArray.create () in
    let out x = ExtArray.add tout x in
    let rec loop i_ks i_ts =
      if i_ks < sz_ks then begin
        if i_ts >= sz_ts then begin
          out nope ; loop (i_ks+1) i_ts
        end else begin
          let k = ks.(i_ks) and t = ts.(i_ts) in
          let c = String.compare k.Key.name t.LogState.tname in
          if c < 0 then begin
            out nope ; loop (i_ks+1) i_ts            
          end else if c > 0 then begin
            loop i_ks (i_ts+1)
          end else (* c = 0 *) begin
            out (I.fmt_cell col k.Key.info t) ;
            loop (i_ks+1) (i_ts+1)
          end
        end
      end in
    loop 0 0 ;
    ExtArray.to_array tout
    
  let build keys logs =
    List.map (fun log -> extract log keys log.LogState.tests) logs


  let sum_col v ks ts =
    let sz_ks = Array.length ks
    and sz_ts  =Array.length ts in
    let rec loop v i_ks i_ts =
      if i_ks >= sz_ks || i_ts >= sz_ts then v
      else begin
        let k = ks.(i_ks) and t = ts.(i_ts) in
        let c = String.compare k.Key.name t.LogState.tname in
        if c < 0 then loop v (i_ks+1) i_ts
        else if c > 0 then loop v i_ks (i_ts+1)
        else (* c = 0 *) loop (I.add v t) (i_ks+1) (i_ts+1)
      end in
    loop v 0 0

  let sum keys vs logs =
    List.map2
      (fun v log -> sum_col v keys log.LogState.tests)
      vs logs
end


(********************)
(* Final formatting *)
(********************)

module Dump(Opt:Config) = struct

  open Opt


(* Extract from lists *)
  let extract_list xs = match xs with
  | [] -> None,[]
  | x::xs -> Some x,xs

  let extract_lists cols =
    let cs = List.map extract_list cols in
    List.split cs

(* Decompose columns first item X rest *)

  let extract_col i xs =
    if i < Array.length xs then Some xs.(i) else None

  let extract_col_opt i xs = match xs with
  | None -> None
  | Some xs -> Some (extract_col i xs)

  let extract_cols i cols = List.map (extract_col i) cols

(* Size calculation *)
  let max_array comp_width col =
    let r = ref 0 in
    Array.iter (fun x -> r := max (comp_width x) !r) col ;
    !r

  let max_list comp_width col =
    List.fold_left (fun k x -> max (comp_width x) k) 0 col


  let rec complete_list completion h xs = match xs with
  | [] -> if h <= 0 then [] else completion::complete_list completion (h-1) []
  | x::xs -> x::complete_list completion (h-1) xs
(******************)
(* Dumping proper *)
(******************)

  let hline c n =
    match mode with
    | Txt -> 
        for _k=1 to n do
	  output_char chan c
        done ;
        output_char chan '\n'
    | LaTeX|HeVeA|HeVeANew -> 
        fprintf chan "\\hline\n"

  let dump_row cell1 w1 cell2 w2 cells ws =

    let rec do_rec c1 c2 cs =
      let x1,c1 = extract_list c1
      and x2,c2 = extract_list c2
      and xs,cs = extract_lists cs in
      if Misc.is_none x1 && List.for_all Misc.is_none xs then ()
      else begin
        let beginseparator () =
	  match mode with
	  | Txt -> ()
	  | LaTeX|HeVeA|HeVeANew -> fprintf chan " & "
        in

(* Annoying typechecker wants me to put that annotation exactly there *)
        let fmtstringbegin = 
	  match mode with
	  | Txt -> ("%-*s|" : (int -> string -> unit, out_channel, unit) format)
	  | LaTeX|HeVeA|HeVeANew -> "%-*s" in


        let fmtstringmid = 
	  match mode with 
	  | Txt -> (" %-*s" : (int -> string -> unit, out_channel, unit) format) 
	  | LaTeX|HeVeA|HeVeANew -> "& %-*s" in

        let endline () =
	  match mode with 
	  | Txt -> output_char chan '\n' 
	  | LaTeX|HeVeA|HeVeANew -> fprintf chan " \\\\\n"
        in

        begin match mode with
        | HeVeA|HeVeANew -> begin match x1 with
          | None|Some "" -> ()
          | Some s ->
              fprintf chan "\\handletest{%s}" s
        end
        | LaTeX|Txt ->
            fprintf chan fmtstringbegin w1 (Misc.proj_opt "" x1)
        end ;
        begin match w2 with
        | 0 -> ()
        | _ ->
	    beginseparator ();
	    fprintf chan fmtstringbegin w2 (Misc.proj_opt "" x2) ;
	end;
        List.iter2
          (fun w x -> 
	    let xo = Misc.proj_opt "" x in 
	    (* Ghastly hack to kill column in latex output *)
	    if xo = "kill" then () 
	    else fprintf chan fmtstringmid w xo)
          ws xs ;
        endline (); 
        do_rec c1 c2 cs
      end in

    let h = max (max 1 (List.length cell2)) (max_list List.length cells) in
    let cell1 = complete_list "" h [ cell1 ]
    and cell2 = complete_list "" h cell2
    and cells = List.map (complete_list "" h) cells in
    do_rec cell1 cell2 cells

(* Our order to sort rows, according to first column, complex *)
  let str_compare s1 s2 = match s1,s2 with
  | "","" -> 0
  | "",_ -> 1
  | _,"" -> -1
  | _,_ -> String.compare s1 s2

  let order_order k1 k2 =
    let idx1 = TblRename.find_order Opt.orders k1 
    and idx2 = TblRename.find_order Opt.orders k2 in
    Misc.int_compare idx1 idx2

  let order =
    (fun k1 k2 ->
      try order_order k1 k2
      with Not_found ->
        try let idx1 = TblRename.find_order Opt.kinds  k1 in
        try let idx2 = TblRename.find_order Opt.kinds  k2 in
        Misc.int_compare idx1 idx2
        with Not_found -> -1          
        with Not_found ->
          try ignore (TblRename.find_order kinds  k2) ; 1
          with Not_found -> str_compare k1 k2)

(* Transpose : list of columns -> list of rows *)

let get_width m = List.length m

let transpose_in col1 m =
  let w = get_width m in
  if w > 0 then match  m with
  | [] -> []
  | c::_ ->
      let h = Array.length c in
      if h = 0 then []
      else
        let e = c.(0) in
        let rec loop k nrow =
        if nrow < 0 then k
        else begin
          let r = Array.make w e in
          Misc.iteri
            (fun i c -> r.(i) <- c.(nrow))
            m ;
          loop ((col1.(nrow),r)::k) (nrow-1)
        end in
        loop [] (h-1)
  else []

let transpose_out m =
  let h = List.length m in
  match m with
  | (c1,r)::_ ->
      let w = Array.length r in
      let e = r.(0) in
      let col1 = Array.make h c1 in
      let mout = Array.make_matrix w h e in
      Misc.iteri
        (fun k (c1,r) ->
          col1.(k) <- c1 ;
          for i = 0 to Array.length r-1 do
            mout.(i).(k) <- r.(i)
          done)
        m ;
      col1,Array.to_list mout
  | [] -> assert false

  
(* Sorting rows proper *)
  let sort_with_col2 col1 m = match m with
  | [] -> (* Empty matrix do not transpose *)
      Array.sort order col1 ;
      col1,[]
  | _ ->
      let m = transpose_in col1 m in
      let m = List.sort (fun (k1,_) (k2,_) -> order k1 k2) m in
      transpose_out m

  let sort_matrix_by_col1 col1 ?col2:col2 m =
(* Add col2 *)
    let m = match col2 with
    | None -> m
    | Some col2 -> col2::m in
    let col1,m = sort_with_col2 col1 m in
(* Extract col2 *)
    match col2,m with
    | Some _,col2::m -> col1,Some col2,m
    | None,m -> col1,None,m
    | _,_ -> assert false


  let erase_empty_with_col2 col1 m = match m with
    | [] -> col1,m
    | _::_ ->
        let h = Array.length col1 in
        let col_out = ExtArray.create () in
        let m_out = List.rev_map (fun _ -> ExtArray.create ()) m in
        for k = 0 to h-1 do
          if
            List.exists
              (fun c -> match c.(k) with
              | []|[(""|"---")] -> false
              | _ -> true)
              m
          then begin
            ExtArray.add col_out (col1.(k)) ;
            List.iter2
              (fun m_out c -> ExtArray.add m_out c.(k))
              m_out m
          end
        done ;
        ExtArray.to_array col_out,
        List.map ExtArray.to_array m_out

  let erase_empty_rows col1 ?col2:col2 m =
(* Add col2 *)
    let m = match col2 with
    | None -> m
    | Some col2 -> col2::m in
    let col1,m = erase_empty_with_col2 col1 m in
(* Extract col2 *)
    match col2,m with
    | Some _,col2::m -> col1,Some col2,m
    | None,m -> col1,None,m
    | _,_ -> assert false

(* zyva *)
  (* Totally innefficient, well... *)
  let add_last xs x = Array.append xs [| x |]

  let dump legend _horiz row1 rown col1 ?col2:col2 m =
    let col1 = Array.map (fun k -> k.Key.name) col1 in
    let col1,col2,m = match rown with
    | [] -> col1,col2,m
    | _  ->
        add_last col1 "",
        (match col2 with None -> None | Some c -> Some (add_last c [])),
        List.map2 (fun col c -> add_last col [c]) m rown in
    let col1,col2,m = sort_matrix_by_col1 col1 ?col2:col2 m in    
    let col1,col2,m =
      if show_empty_rows then col1,col2,m        
      else erase_empty_rows col1 ?col2:col2 m in
    let w1 = max_array String.length col1
    and w2 = match col2 with
    | None -> 0
    | Some col2 -> max_array (max_list String.length) col2 in
    let row1_expanded = 
      List.flatten 
        (List.map 
	   (fun (k,s) -> 
	     let smode = 
	       match mode with
	       | Txt -> s
	       | LaTeX|HeVeA ->
		   if k = 1 then sprintf "\\multicolumn{1}{|c|}{%s}" s 
		   else sprintf "\\multicolumn{%i}{|l|}{%s}" k s 
               |HeVeANew -> 
		   if k = 1 then sprintf "\\multicolumn{1}{|c}{%s}" s 
		   else sprintf "\\multicolumn{%i}{l}{%s}" k s 
	     in 
	     complete_list "kill" k [smode]) row1) in
    let ws =
      match m with
      | [] ->
          List.map String.length row1_expanded
      | _ ->
          List.map2
            max
            (List.map (max_array (max_list String.length)) m)
            (List.map String.length row1_expanded) in
    let rec do_rec i width c1 c2 cs previous_first_char (*linecount*) =
      let x1 = extract_col i c1 
      and xc2 = extract_col_opt i c2
      and xs = extract_cols i cs in
      let x2 = Misc.app_opt (fun x -> x) xc2 in
      if not (Misc.is_none x1) then begin
        hline '-' width ;
        let this_first_char = 
          match x1 with 
          | Some s -> (try s.[0] with Invalid_argument _ -> ' ')
          | None -> ' ' in
        if this_first_char <> previous_first_char then 
          (match mode with
          | Txt|HeVeA|HeVeANew -> ()
          | LaTeX -> hline '-' width);
        dump_row
          (Misc.proj_opt "" x1) w1
          (Misc.proj_opt [] (Misc.proj_opt None x2)) w2
          (List.map (Misc.proj_opt []) xs) ws ;
(*        if (linecount+1) mod 5 = 0 then *)
(*           match mode with *)
(*           | Txt -> ()  *)
(*           | LaTeX -> fprintf chan "\\hline\n"); *)
        do_rec (i+1) width c1 c2 cs this_first_char (* (linecount+1) *)
      end in

    let do_header () = 
      match mode with
      | Txt -> fprintf chan "*%s*\n" legend
      | LaTeX ->
	  fprintf chan "\\newcommand{\\%stable}{\n" legend ;
	  fprintf chan "\\begin{longtable}" ;
          begin match col2 with
          | None -> fprintf chan "{|l||" 
          | Some _ -> fprintf chan "{|l|l||"
          end ;
	  List.iter (fun _ -> fprintf chan "r|") row1_expanded;
	  fprintf chan "}\n\\hline\n"
      | HeVeA|HeVeANew ->
          let is_new = match mode with
          | HeVeANew -> true
          | HeVeA -> false
          | _ -> assert false in
	  fprintf chan "\\begin{tabular}" ;
          begin if is_new then  match col2 with
          | None -> fprintf chan "{l|" 
          | Some _ -> fprintf chan "{l|l|"
          else match col2 with
          | None -> fprintf chan "{|l||" 
          | Some _ -> fprintf chan "{|l|l||"
          end ;
          let rs = List.map (fun _ -> "r") row1_expanded in
          fprintf chan "%s"
            (if is_new then String.concat "|" rs
            else
              let rs = List.map (sprintf "%s|") rs in
              String.concat "" rs) ;
	  begin if is_new then fprintf chan "}\n"
          else fprintf chan "}\n\\hline\n" end

    in

    let do_footer () = 
      match mode with 
      | Txt -> ()
      | LaTeX ->
          fprintf chan "\\hline\n\\end{longtable}}\n"
      | HeVeA ->
          fprintf chan "\\hline\n\\end{tabular}\n"
      | HeVeANew ->
          fprintf chan "\\end{tabular}\n"

    in
    
    do_header ();
    dump_row "" w1 ["Kind"] w2 (List.map (fun x -> [x]) row1_expanded) ws ;
    let width =
      List.fold_left (+) (1+w1+w2+1+List.length row1) ws in
    begin match mode with
    | HeVeANew -> ()
    | Txt|LaTeX|HeVeA -> hline '-' width
    end ;
    begin match mode with 
    | Txt|HeVeA|HeVeANew -> ()
    | LaTeX -> fprintf chan "\\endhead\n"
    end ;

    do_rec 0 width col1 col2 m ' ' (*0*) ;
    do_footer ()
end
