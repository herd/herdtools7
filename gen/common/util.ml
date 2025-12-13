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

open Printf


type spec = string * Arg.spec * string

let parse_tag opt set tags msg =
  opt,
  Arg.String
    (fun tag -> match set tag with
    | false ->
        raise
          (Arg.Bad
             (sprintf "bad tags for %s, allowed tag are %s"
                opt (String.concat "," tags)))
    | true -> ()),
  sprintf "<%s> %s" (String.concat "|" tags) msg

let parse_tags opt set_one all_tags msg =
  opt,
  Arg.String
    (fun tags ->
      let tags = Misc.split_comma tags in
      List.iter
        (fun tag -> match set_one tag with
        | false ->
            raise
              (Arg.Bad
                 (sprintf "bad tags for %s, allowed tag are %s"
                    opt (String.concat "," all_tags)))
        | true -> ())
      tags),
   sprintf "<%s> %s" (String.concat "|" all_tags) msg

let arch_opt arch =
  let d = !arch in
   parse_tag
    "-arch"
    (fun tag -> match Archs.parse tag with
    | None -> false
    | Some a -> arch := a ; true)
    Archs.tags (sprintf "specify architecture, default %s" (Archs.pp d))

let parse_cmdline options get_cmd_arg =
  Arg.parse options
    get_cmd_arg
    (sprintf "Usage %s [options] [arg]*\noptions are:" Sys.argv.(0))

module List = struct
  let concat_map : ('a -> 'b list) -> 'a list -> 'b list =
    fun f l -> List.concat (List.map f l)

  let uniq ~eq l =
    let rec uniq eq acc l =
      match l with
      | [] -> List.rev acc
      | x :: xs when List.exists (eq x) xs -> uniq eq acc xs
      | x :: xs -> uniq eq (x :: acc) xs
    in
    uniq eq [] l

  module Infix = struct
    let (let*) = fun x f -> concat_map f x
  end

  let rec sequence : 'a list list -> 'a list list =
    let open Infix in
    function [] -> [[]]
      | x :: xs ->
        let* x' = x in
        let* xs' = sequence xs in
        [ x' :: xs' ]

  let rec fold_left_opt (f : 'acc -> 'a -> 'acc option) (v : 'acc) : 'a list -> 'acc option =
    function
      | [] -> Some v
      | x :: xs -> Option.bind (f v x) (fun acc -> fold_left_opt f acc xs)
end

module Option = struct
  let rec choice_fn : (unit -> 'a option) list -> 'a option = function
    | [] -> None
    | f :: l -> let x = f () in if Option.is_some x then x else choice_fn l

  let guard b = if b then Some () else None

  module Infix = struct
    let (let*) = Option.bind
  end
end
