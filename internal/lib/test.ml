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

(** Unit-testing utilities. *)

let run_test (name, test) =
  try
    test () ;
    true
  with
  | Failure msg ->
      Printf.printf "Failed: %s: %s\n" name msg ;
      false
  | e ->
      Printf.printf "Failed %s: raised exception\n" name ;
      raise e

let run tests =
  let results = List.map run_test tests in
  let failed r = not r in
  if List.exists failed results then
    exit 1


(* Pretty-printing for failure messages. *)

let pp_list pp_x xs = Printf.sprintf "[%s]" (String.concat "; " (List.map pp_x xs))

let pp_int_list xs = pp_list (Printf.sprintf "%i") xs

let pp_string_list xs = pp_list (Printf.sprintf "%S") xs


(* Comparisons. *)

let int_compare (x:int) (y:int) = compare x y

let rec find_comparison cs =
  match cs with
  | [] -> 0
  | c :: cs' -> if c <> 0 then c else (find_comparison cs')

let list_compare c xs ys =
  let compared_length = int_compare (List.length xs) (List.length ys) in
  if compared_length = 0 then begin
    List.combine xs ys
    |> List.map (fun (x, y) -> c x y)
    |> find_comparison
  end else
    compared_length

let string_list_compare xs ys =
  list_compare String.compare xs ys

let int_list_compare xs ys =
  list_compare int_compare xs ys
