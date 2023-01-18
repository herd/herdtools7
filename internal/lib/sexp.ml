(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* jade alglave, university college london, uk.                             *)
(* luc marange_inclusivet, inria paris-rocquencourt, france.                *)
(*                                                                          *)
(* copyright 2010-present institut national de recherche en informatique et *)
(* en automatique and the authors. all rights reserved.                     *)
(*                                                                          *)
(* this software is governed by the cecill-b license under french law and   *)
(* abiding by the rules of distribution of free software. you can use,      *)
(* modify and/ or redistribute the software under the terms of the cecill-b *)
(* license as circulated by cea, cnrs and inria at the following url        *)
(* "http://www.cecill.info". we also give a copy in license.txt.            *)
(****************************************************************************)

(** An S-expression type, parser, and Dune-specific parser. *)

(* NOTE: Because this is used by scripts run with the OCaml top-level, it MUST
 * NOT depend on any code from outside this file. *)

exception ParseError of string

type t =
  | Atom of string
  | List of t list

let compare x y =
  let rec compare_sexp x y =
    match x, y with
    | Atom x, Atom y -> String.compare x y
    | Atom _, List _ -> -1
    | List _, Atom _ -> 1
    | List xs, List ys -> compare_sexp_list xs ys
  and compare_sexp_list xs ys =
    match xs, ys with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
        match compare_sexp x y with
        | 0 -> compare_sexp_list xs ys
        | n -> n
  in
  compare_sexp x y

let rec to_string sexp =
  match sexp with
  | Atom s -> s
  | List sexps ->
      let subexps = List.map to_string sexps in
      Printf.sprintf "(%s)" (String.concat " " subexps)


(* Parser. *)

(* Strean is deprecated, write our simple stream module *)

module MyStream = struct
  type t =
    {chan:in_channel;
     mutable count: int;
     mutable nxt: char option;
     mutable eof:bool; }

  let of_channel chan = { chan; count=0; nxt=None; eof=false; }

  let read_next s =
    if s.eof then None
    else
      let r =
        try
          let r = Some (input_char s.chan) in
          s.count <- s.count+1 ;
          r
        with
        | End_of_file ->
            s.eof <- true ;
            close_in_noerr s.chan ;
            None
        | e ->
            close_in_noerr s.chan ;
            raise e in
      s.nxt <- r;
      r

  let peek s = match s.nxt with
    | Some _ as nxt -> nxt
    | None -> read_next s

  let junk s = s.nxt <- None

  let count s = s.count

end

let from_dune_channel chan =
  (* This parser is written by hand because this module is used by a script run
   * with the OCaml top-level, and so cannot have any compilation steps, and so
   * cannot use ocamllex/etc. *)

  let printable c =
    match c with
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' | '-' | '+' | ':' | '.' | '/' | '*' | '\\'  | '%' | '{' | '}' -> true
    | _ -> false
  in
  let stream = MyStream.of_channel chan in
  let junk () = MyStream.junk stream in
  let peek () = MyStream.peek stream in

  let unexpected_character c =
    ParseError (Printf.sprintf "Unexpected character %c at char %i" c (MyStream. count stream))
  in

  let rec whitespace () =
    match peek () with
    | Some ' ' | Some '\t' | Some '\n' | Some '\r' -> junk () ; whitespace ()
    | _ -> ()
  in
  let rec comment () =
    match peek () with
    | Some '\n' -> junk ()
    | Some _ -> junk () ; comment ()
    | None -> ()
  in
  let atom () =
    let buf = Buffer.create 16 in
    let rec atom' () =
      match peek () with
      | Some c when printable c -> junk () ; Buffer.add_char buf c ; atom' ()
      | _ -> Atom (Buffer.contents buf)
    in
    atom' ()
  in
  let rec list (acc : t list) =
    whitespace () ;
    match peek () with
    | None -> raise (ParseError "Unexpected end of input")
    | Some ';' -> comment () ; list acc
    | Some '(' -> junk () ; list ((list []) :: acc)
    | Some ')' -> junk () ; List (List.rev acc)
    | Some c when printable c -> list ((atom ()) :: acc)
    | Some c ->
        raise (unexpected_character c)
  in
  let rec dune_file (acc : t list) =
    whitespace () ;
    match peek () with
    | None -> List (List.rev acc)
    | Some ';' -> comment () ; dune_file acc
    | Some '(' -> junk () ; dune_file ((list []) :: acc)
    | Some c when printable c -> dune_file ((atom ()) :: acc)
    | Some c ->
        raise (unexpected_character c)
  in
  dune_file []

let of_dune_file path =
  let ch = open_in path in
  let dune =
    try
      from_dune_channel ch
    with e -> close_in ch ; raise e
  in
  close_in ch ; dune
