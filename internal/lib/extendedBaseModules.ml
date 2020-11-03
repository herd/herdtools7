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

(** Extending built-in / base modules, either to port future features into
 *  earlier versions of OCaml, or to add new functionality. *)

module Int = struct
  type t = int

  let compare (x : int) (y : int) = compare x y

  let to_ocaml_string i = Printf.sprintf "%i" i
end

module String = struct
  include String

  let to_ocaml_string s = Printf.sprintf "%S" s
end

module List = struct
  include List

  let compare cf xs ys =
    let rec find_comparison cs =
      match cs with
      | [] -> 0
      | c :: cs' -> if c <> 0 then c else (find_comparison cs')
    in
    let compared_length = Int.compare (List.length xs) (List.length ys) in
    if compared_length = 0 then begin
      List.combine xs ys
      |> List.map (fun (x, y) -> cf x y)
      |> find_comparison
    end else
      compared_length

  let to_ocaml_string f xs =
    Printf.sprintf "[%s]" (String.concat "; " (List.map f xs))
end
