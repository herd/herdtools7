(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Litmus tests *)

type ('prog,'nice_prog,'start,'state,'constr, 'loc, 'locset) t =
    {
     arch : Archs.t ; 
     name : Name.t ;
     info : MiscParser.info ;
     program : 'prog ;
     nice_prog : 'nice_prog ;
     start_points : 'start ;
     init_state : 'state ;
     cond : 'constr ;
     flocs : 'loc list ;
     observed : 'locset ;
     extra_data : MiscParser.extra_data ;
   }

val simple_name :
    ('prog,'nice_prog,'start,'state,'constr, 'loc, 'locset) t -> string
val readable_name :
    ('prog,'nice_prog,'start,'state,'constr, 'loc, 'locset) t -> string
val very_readable_name :
    ('prog,'nice_prog,'start,'state,'constr, 'loc, 'locset) t -> string
val basename :
    ('prog,'nice_prog,'start,'state,'constr, 'loc, 'locset) t -> string


module Make(A:Arch.S) : sig

  type result =
      (A.program,
       A.nice_prog,
       A.start_points,
       A.state,
       A.constr, 
       A.location,
       A.LocSet.t
      ) t
       
  val build : Name.t -> A.pseudo MiscParser.t -> result

  val find_our_constraint : result -> A.constr

  (* needed to interpret bell *)
  val empty_test : result

end
