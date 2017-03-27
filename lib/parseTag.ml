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
module type Opt = sig
  type t
  val tags : string list
  val parse : string -> t option
  val pp : t -> string
end

module Make (O:Opt) =
  struct

    open Printf

    let argfun opt f =
      (fun tag ->
        if not (f tag) then 
          raise
            (Arg.Bad
               (sprintf "%s is a bad tag for %s, allowed tag are %s"
                  tag opt (String.concat "," O.tags))))

    let complete opt msg spec d =
      opt,spec,
      match d with
      | Some d ->
          sprintf
            "<%s> %s, default %s"
            (String.concat "|" O.tags) msg
            (O.pp d)
      | None ->
          sprintf "<%s> %s" (String.concat "|" O.tags) msg

    let parse_withfun opt f msg d =
      complete opt msg
        (Arg.String
           (argfun opt
              (fun tag ->
                match O.parse tag with
                | Some o -> f o ; true
                | None -> false)))
        d
    let parse opt r msg =
      parse_withfun opt (fun o -> r := o) msg (Some !r)

    let parse_opt opt r msg =
      parse_withfun opt (fun o -> r := Some o) msg None

    let parse_fun opt f msg = parse_withfun opt f msg None
  end

