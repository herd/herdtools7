(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
  * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
  * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

open Format

(** Type of Lisp objects *)
type obj =
  | Num of (Q.t * Q.t)  (** Complex rational *)
  | String of string
  | Symbol of (string * string)  (** Package, symbol *)
  | Char of char
  | Cons of (obj * obj)
  | Comment of (string * obj)
      (** Fake new object type to allow us to add single-line comments *)

let nil = Symbol ("COMMON-LISP", "NIL")
let t = Symbol ("COMMON-LISP", "T")
let quote = Symbol ("COMMON-LISP", "QUOTE")
let zero = Num (Q.zero, Q.zero)
let one = Num (Q.one, Q.zero)

let rec equal x y =
  match (x, y) with
  | Num (rx, ix), Num (ry, iy) -> Q.( = ) rx ry && Q.( = ) ix iy
  | String sx, String sy -> String.equal sx sy
  | Symbol (px, nx), Symbol (py, ny) -> String.equal nx ny && String.equal px py
  | Char cx, Char cy -> Char.equal cx cy
  | Cons (carx, cdrx), Cons (cary, cdry) -> equal carx cary && equal cdrx cdry
  | Comment (sx, ox), Comment (sy, oy) -> String.equal sx sy && equal ox oy
  | _ -> false

module type PrinterConf = sig
  val defaultpkg : string
  val downcase : bool
end

module MakePrinter (Conf : PrinterConf) = struct
  let defaultpkg = Conf.defaultpkg
  let downcase = Conf.downcase

  let symbol_part_str x =
    (* to get this actually right we'd need much better analysis of whether the symbol needs escaping.
       For now we don't expect to see bad characters in symbols much, but we might need mixed case *)
    if x = String.uppercase_ascii x then
      if downcase then String.lowercase_ascii x else x
    else "|" ^ x ^ "|"

  (* In strings we need to escape both quotes and backslashes, so that the Lisp reader will get the right string.
     We need four backslashes to represent each one backslash in both the regex input and output. *)
  let escape_backslashes_and_quotes s =
    let repl_char ch newstr str =
      String.concat newstr (String.split_on_char ch str)
    in
    repl_char '"' "\\\"" (repl_char '\\' "\\\\" s)

  let rec pp_obj f x =
    match x with
    | Num (r, i) ->
        if Q.( = ) i Q.zero then Q.pp_print f r
        else fprintf f "#C(@[<hov 0>%a@ %a@]" Q.pp_print r Q.pp_print i
    | String s -> fprintf f "\"%s\"" (escape_backslashes_and_quotes s)
    | Symbol (pkg, name) ->
        (* BOZO neither packages nor escaping are handled correctly *)
        if String.equal pkg "KEYWORD" then
          fprintf f ":%s" (symbol_part_str name)
        else if String.equal pkg defaultpkg || String.equal pkg "COMMON-LISP"
        then fprintf f "%s" (symbol_part_str name)
        else fprintf f "%s::%s" (symbol_part_str pkg) (symbol_part_str name)
    | Char c -> fprintf f "#\\%s" (Char.escaped c)
    | Cons (car, cdr) ->
        if equal car quote then
          match cdr with
          | Cons (cadr, cddr) ->
              if equal cddr nil then fprintf f "'%a" pp_obj cadr
              else pp_obj_as_list f x
          | _ -> pp_obj_as_list f x
        else pp_obj_as_list f x
    | Comment (str, obj) ->
        fprintf f "@[<v>%a@]%a" pp_comment_lines
          (String.split_on_char '\n' str)
          pp_obj obj

  and pp_comment_lines f lines =
    match lines with
    | line :: rest -> fprintf f ";; %s@;%a" line pp_comment_lines rest
    | _ -> fprintf f ""

  and pp_obj_as_list f x = fprintf f "(@[<hov 0>%a@])" pp_list x

  and pp_list f x =
    match x with
    | Cons (car, cdr) ->
        if equal cdr nil then pp_obj f car
        else fprintf f "%a@ %a" pp_obj car pp_list cdr
    | _ -> fprintf f ".@ %a" pp_obj x
end

let print_obj ?(pkg = "ACL2") f x =
  let module C = struct
    let defaultpkg = pkg
    let downcase = true
  end in
  let module P = MakePrinter (C) in
  P.pp_obj f x

let _test () =
  let foo = Symbol ("ACL2", "FOO") in
  let bar = Symbol ("ACL2", "BAR") in

  let rec make_sexpr n =
    if n <= 0 then
      match Random.int 8 with
      | 0 -> bar
      | 1 -> t
      | 2 -> zero
      | 3 -> one
      | 4 -> foo
      | _ -> nil
    else
      match Random.int 6 with
      | 0 ->
          Comment
            ( "hello this is a single-line comment",
              make_sexpr (n - Random.int (n + 4)) )
      | 1 ->
          Comment
            ( "hello this is a multi\n-line comment",
              make_sexpr (n - Random.int (n + 4)) )
      | _ ->
          Cons
            ( make_sexpr (n - Random.int (n + 4)),
              make_sexpr (n - Random.int (n + 4)) )
  in
  let obj = make_sexpr 100 in
  let _ = print_obj Format.std_formatter obj in
  let obj = make_sexpr 20 in
  let _ = print_obj Format.std_formatter obj in
  obj

let of_list x = List.fold_right (fun fst rst -> Cons (fst, rst)) x nil
let of_list_map f x = of_list (List.map f x)

let sym_alist pkg x =
  List.map (fun (k, v) -> Cons (Symbol (pkg, k), v)) x |> of_list

let of_option f = function Some x -> f x | None -> nil
let of_q x = Num (x, Q.zero)
let of_int x = of_q (Q.of_int x)
let of_bigint x = of_q (Q.of_bigint x)
let of_bool x = if x then t else nil
let key str = Symbol ("KEYWORD", str)
let of_str x = String x
