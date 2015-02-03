(*********************************************************************)
(*                          Litmus                                   *)
(*                                                                   *)
(*        Luc Maranget, INRIA Paris-Rocquencourt, France.            *)
(*        Susmit Sarkar, University of Cambridge, UK.                *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)
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

  end

