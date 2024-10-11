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
  (** If true, tag performs a hidden action and is forgotten *)

  val reducetag : t -> t list
  (** Tag may perform a hidden action and may be changed *)
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
          let tags = O.reducetag tag in
          List.iter
            (fun tag ->
              let old = !add in
              add := (fun t -> O.compare t tag = 0 || old t))
            tags
        end

      let parse_tag_set opt add =  do_parse_tag_set opt (add_tag add)

      let parse opt add msg =
        let spec = do_parse_tag_set opt (add_tag add) in
        opt,Arg.String spec,
        Printf.sprintf "<tags> where tags in {%s}, %s" taglist msg
    end

module type SArg = sig
  include Opt

  val compare : t -> t -> int

  val set_fault_handling :  Fault.Handling.t ref -> t -> bool
  val set_mte_precision : Precision.t ref -> t -> bool
  val set_mte_store_only : bool ref -> t -> bool
  val set_sve_length : int ref -> t -> t option
  val set_sme_length : int ref -> t -> t option
  val check_tag : t -> t list
end

module type RefsArg = sig
  val fault_handling : Fault.Handling.t ref
  val mte_precision : Precision.t ref
  val mte_store_only : bool ref
  val sve_vector_length : int ref
  val sme_vector_length : int ref
end

module MakeOptS =
  functor (Opt:SArg) -> functor (Refs:RefsArg) ->
  struct
    include Opt

    let setnow t =
      set_fault_handling Refs.fault_handling t ||
      set_mte_precision Refs.mte_precision t ||
      set_mte_store_only Refs.mte_store_only t

    let check_lengths t =
      let (>>=) o f = match o with
      | Some _ -> o
      | None -> f t in
      set_sve_length Refs.sve_vector_length t
      >>= fun t -> set_sme_length Refs.sme_vector_length t

    let reducetag tag =
      match check_lengths tag with
      | Some tag -> [tag]
      | None -> check_tag tag

  end
