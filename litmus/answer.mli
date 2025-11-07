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

(* Answer of a test compilation or run *)


type hash = { filename : string ;  hash : string ; }
type hash_env = hash StringMap.t

type completed = (* Answer for completed test *)
  { arch : Archs.t ; (* Arch of test *)
    doc : Name.t   ; (* Name of test *)
    src : string   ; (* Name of emitted source file *)
    fullhash : hash ; nprocs : int ; (* hash and numbre of threads *)
    flags: Flags.t ;
  }

type answer =
  | Completed of completed
  | Interrupted of exn (* Error *)
  | Absent (* Test not compile for some reason under user control *)
