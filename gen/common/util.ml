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

module type Monad = sig
  type 'a t
  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module List = struct
  let concat_map : ('a -> 'b list) -> 'a list -> 'b list =
    fun f l -> List.concat (List.map f l)

  let pure x = [ x ]
  let bind x f = concat_map f x

  module Infix = struct
    let (>>=) = fun x f -> concat_map f x
    let (let*) = (>>=)
  end

  module Traversal (M: Monad) = struct
    let rec fold_left (f : 'acc -> 'a -> 'acc M.t) (v : 'acc) : 'a list -> 'acc M.t =
      let open M.Infix in
      function
        | [] -> M.pure v
        | x :: xs ->
          let* acc = f v x in
          fold_left f acc xs
  end
end

module Option = struct
  let rec choice : 'a option list -> 'a option = function
    | [] -> None
    | None :: l -> choice l
    | Some x :: _ -> Some x

  let rec choice_fn : (unit -> 'a option) list -> 'a option = function
    | [] -> None
    | f :: l -> let x = f () in if Option.is_some x then x else choice_fn l

  let pure x = Some x
  let bind = Option.bind

  let guard b = if b then Some () else None

  module Infix = struct
    let (>>=) = Option.bind
    let (let*) = Option.bind
  end
end
