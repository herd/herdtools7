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

module type OptS = sig
  include Opt
  val compare : t -> t -> int
  val setnow : t -> bool
end

module MakeS (O:OptS)
    = struct

      let  taglist = String.concat "," O.tags

      let do_parse_tag_set opt f =
        let spec tag =
          let es = Misc.split_comma tag in
          let es =
            List.map
              (fun tag -> match O.parse tag with
              | Some tag -> tag
              | None ->
                  raise
                    (Arg.Bad
                       (Printf.sprintf "tag %s for %s is wrong, allowed tags are %s"  tag opt taglist)))
              es in
          List.iter f es in
        spec

      let add_tag add tag =
        if not (O.setnow tag) then begin
          let old = !add in
          add := (fun t -> O.compare t tag = 0 || old t)
        end

      let parse_tag_set opt add =  do_parse_tag_set opt (add_tag add)

      let parse opt add msg =
        let spec = do_parse_tag_set opt (add_tag add) in
        opt,Arg.String spec,
        Printf.sprintf "<tags> where tags in {%s}, %s" taglist msg
    end
