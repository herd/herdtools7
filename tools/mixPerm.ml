(*********************************************************************)
(*                        Offence                                    *)
(*                                                                   *)
(* Jade Alglave, Luc Maranget, INRIA Paris-Rocquencourt, France.     *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)

module Make(O:MixOption.S)(A:Arch.S) : sig
  val perm : Name.t -> A.test -> A.test
end =
  struct
    open ConstrGen

    let perm_location p loc = match loc with
    | A.Location_reg (i,r) -> A.Location_reg (p.(i),r)
    | A.Location_global _ -> loc

    let perm_state_atom p (loc,v) = perm_location p loc,v

    let perm_state p = List.map (perm_state_atom p)

    let perm_locations p = perm_state p

    let perm_atom p a = match a with
    | LV (loc,v) -> LV (perm_location p loc,v)
    | LL (l1,l2) -> LL (perm_location p l1,perm_location p l2)

    let perm_constr p = ConstrGen.map_constr (perm_atom p)

    let perm_prog p prog =
      let n = Array.length p in
      let t = Array.make n (-1,[]) in
      List.iter
        (fun (i,code) -> let idx = p.(i) in t.(idx) <- idx,code)
        prog ;
      Array.to_list t

      
    open MiscParser

(* Ramdom permutation *)
    let perm_t n =
      let t = Array.init n (fun i -> i) in
      let n = Array.length t in
      for k=0 to n-2 do
        let j =
          if k = 0 then 1 else k+Random.int (n-k) in
        let x = t.(j) in
        t.(j) <- t.(k) ;
        t.(k) <- x
      done ;
      t
(* Given permutation *)
    let perm_list xs n =
      let t = Array.init n (fun k -> k) in
      let rec do_rec k = function
        | [] -> ()
        | x::xs ->
            if k >= n || x >= n then Warn.fatal "bad permutation" ;
            t.(k) <- x ;
            do_rec (k+1) xs in
      do_rec 0 xs ;
      t

    open MixOption
    open Permut

    let mk_perm = match O.permut with
    | Random -> perm_t
    | Permut xs -> perm_list xs

    let perm _doc t =
      let n = List.length t.prog in
      let p = mk_perm n in
      { t with
        init = perm_state p t.init;
        locations = perm_locations p t.locations;
        condition = perm_constr p t.condition;
        prog = perm_prog p t.prog; }
  end
