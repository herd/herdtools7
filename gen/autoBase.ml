(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

open Printf


(* Description of a test subdirectory *)
type base_st =
    {
     id : string ;      (* directory name *)
     num : int ;        (* rank in base sequence *)
     ntests : int ;     (* number of (diy) tests generated *)
     next_log : int ;   (* number of run logs present *)
   }

let alloc_log dir base =
  let log =
    Filename.concat dir
      (sprintf "%s.%02i"base.id base.next_log) in
  log,{ base with next_log = base.next_log+1 ; }

module Make(A:AutoArch.S) =
  struct
    open A

    module K = struct
      type t =
          {
           cur : R.Set.t ;
           rel : R.Set.t ; saf : R.Set.t ;
         }
      let compare a b = 
        begin match R.Set.compare a.cur b.cur with
        | 0 ->
            begin match R.Set.compare a.rel b.rel with
            | 0 -> R.Set.compare a.saf b.saf
            | r -> r
            end
        | r -> r
        end

      let pp chan k =
        fprintf chan
          "C=%a, R=%a, S=%a"
          R.pp_set k.cur
          R.pp_set k.rel
          R.pp_set k.saf
    end

    module Key = struct
      type t = { phase : AutoPhase.t ; key : K.t }

      let compare a b = 
        match Pervasives.compare a.phase b.phase with
        | 0 -> K.compare a.key b.key
        | r -> r

      let pp chan k =
        fprintf chan "P=%s %a" (AutoPhase.pp k.phase) K.pp k.key
    end


    module BaseMap = Map.Make(Key)


(* All test subdirectory, in a map indexed
   by diy significant agruments *)
    type t =
        {
         next_base : int ;
         base_map : base_st BaseMap.t ;
       }

    let empty = { next_base = 0 ; base_map = BaseMap.empty ; }

    let do_mk_base n =
      if n < 26 then
        sprintf "%c" (Char.chr (n + Char.code 'A'))
      else
        sprintf "Z%02i" n

    let look key bases =
      try  BaseMap.find key bases.base_map,bases
      with Not_found ->
        let new_base =
          { id = do_mk_base bases.next_base ;
            num = bases.next_base ; 
            ntests = -1 ;
            next_log = 0 ; } in
        let bases =
          { next_base = bases.next_base+1 ;
            base_map = BaseMap.add key new_base bases.base_map ; } in
        new_base,bases


    let change key base bases =
      { bases with base_map =  BaseMap.add key base bases.base_map ; }

  end
