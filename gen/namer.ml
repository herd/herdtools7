(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2011 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

(* Normalised names for cycles *)
open Printf

module type S = sig
  type edge
  val mk_name : string -> edge list -> string
end

module Make
    (A:Fence.S)
    (E:Edge.S with
     type dp = A.dp and type fence=A.fence and type atom = A.atom) : S with type edge = E.edge = struct

       type edge = E.edge

       open Code
       open E

       let pp_com c = String.lowercase (pp_com c)
       let edge_name = function
         | Po (Same,_,_) -> Some "pos"
         | Po (Diff,_,_) -> Some "po"
         | Fenced (f,_,_,_) -> Some (String.lowercase (A.pp_fence f))
         | Dp (dp,_,_) -> Some (String.lowercase (A.pp_dp dp))
         | Rf Int -> Some "rfi"
         | Ws Int -> Some "wsi"
         | Fr Int -> Some "fri"
         | Rf Ext -> Some "rfe"
         | Ws Ext -> Some "wse"
         | Fr Ext -> Some "fre"
         | Rmw -> Some "rmw"
         | Detour e ->
             Some (sprintf "det%s" (String.lowercase (pp_extr e)))
         | DetourWs e ->
             Some (sprintf "det%sw" (String.lowercase (pp_extr e)))
         | Store -> Some "sto"
         | Leave c -> Some ("["^pp_com c)
         | Back c -> Some (pp_com c^"]")
         | _ -> None

       let plain  = String.lowercase (Code.plain)

       let atom_name = function
         | None ->  plain
         | Some a -> String.lowercase (A.pp_atom a)

       let atoms_name a1 a2 = match a1,a2 with
       | None,None -> ""
       | _ -> sprintf "%s%s" (atom_name a1) (atom_name a2)

       let one_name e = match edge_name e.edge with
       | Some n ->
           Some (sprintf "%s%s" n (atoms_name e.a1 e.a2))
       | None -> None


       let all_same = function
         | x::xs ->
             let rec do_rec = function
               | y::ys -> if x = y then do_rec ys else None
               | [] -> Some x in
             do_rec xs
         | [] -> None

       let rec count_a = function
         | {edge=(Rf Ext|Fr Ext|Ws Ext); a2=Some a}::
           ({edge=(Rf Ext|Fr Ext|Ws Ext);a1=Some _}::_ as es) ->
             A.pp_atom a::count_a es
         | {edge=(Rf Ext|Fr Ext|Ws Ext); a2=None}::
           ({edge=(Rf Ext|Fr Ext|Ws Ext);a1=None}::_ as es) ->
             Code.plain::count_a es
         | _::es -> count_a es
         | [] -> []

       let init_a = function
         | {edge=(Rf Ext|Fr Ext|Ws Ext);a1=Some a}::_ as es ->
             begin match Misc.last es with
             | {edge=(Rf Ext|Fr Ext|Ws Ext);a2=Some _} ->
                 [A.pp_atom a]
             | _ -> []
             end
         | {edge=(Rf Ext|Fr Ext|Ws Ext);a1=None}::_ as es ->
             begin match Misc.last es with
             | {edge=(Rf Ext|Fr Ext|Ws Ext);a2=None} -> [Code.plain]
             | _ -> []
             end
         | _ -> []

       let isolated_writes es =
         let x =  init_a es @ count_a es in
         let x =
           if List.for_all (fun s -> s = Code.plain) x then []
           else  x in
         String.concat "" x

(* New naming convention with '-' inbetween consecutive int edges *)
       let add_list xs xss = match xs with
       | [] -> xss
       | _  -> xs::xss

       let rec do_po_list d es = match  es with
       | [] -> [],[]
       | e::es ->
           let d = match e.E.edge with
           | E.Leave _ -> d+1
           | E.Back _ -> d-1
           | _ -> d in
           let xs,xss = do_po_list d es in
           match E.get_full_ie e with
           | E.IE Ext when d <= 0 -> [],add_list xs xss
           | E.IE _|E.LeaveBack -> (e::xs),xss

       let po_list = do_po_list 0

       let new_namer es =
         let xs,xss = po_list es in
         let xss = add_list xs xss in
         let xs =
           List.map
             (fun es ->
               String.concat "-"
                 (List.map
                    (fun e -> match one_name e with
                    | Some s -> s
                    | None -> Warn.fatal "Namer failure")
                    es))
             xss in
         xs


       let mk_name base es =
         let name =
           let xs = new_namer es in
           let ys = match isolated_writes es with
           | "" -> []
           | s -> [s] in
           let xs = match all_same xs,xs with
           | Some "po",_ -> ys
           | Some "pos",[_] -> ys
           | Some _x,[_] -> xs@ys
           | Some x,_::_::_ -> (x ^ "s")::ys
           | None, _ -> xs@ys
           | Some _,[] -> assert false in
           String.concat "+" (base::xs) in
         name
     end
