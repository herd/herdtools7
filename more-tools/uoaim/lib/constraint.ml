(****************************************************************************)
(*                           The Diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

module PreConstraint = struct
  type connective = And | Or

  type chain = {
    connective : connective;
    names : string list;
    source : Eff.t;
    target : Eff.t;
  }

  type t = Other of string | Negated of t list | Chain of chain

  include (
    struct
      module A = Angstrom
      open ParserUtil

      let eff : Eff.t A.t =
        let* n = A.char 'E' *> number in
        A.return (Eff.make n)

      let chain_word = word_except [ "and"; "or"; "from"; "to" ]

      (* Parse 'chain' constraints of the form:

         "there exists a chain of REL_1 (or|and) REL_2 ... from En to Em"

       which give rise to cat constraints of the form `(REL_1 & REL_2 & ..)+`.
     *)
      let chain : t list A.t =
        let chain_p separator conn =
          let* ws =
            A.sep_by1 (string_token separator) (A.many1 (token chain_word))
          in
          let* _ = token (A.string "from") in
          (* signals the end of the chain *)
          A.return (conn, ws)
        in
        let* _ = words_ci "there exists a chain of" *> A.commit in
        let* connective, ws = chain_p "and" And <|> chain_p "or" Or in
        let* source = token eff in
        let* _ = string_token "to" in
        let* target = token eff in
        let names = List.map Util.String.from_words ws in
        A.return [ Chain { connective; names; source; target } ]

      (* let contrast : t list A.t = *)
      (*   let* e = token eff in *)
      (*   let* ws = A.many_till (token (word_except ["but"])) (token (A.string "but")) in *)
      (*   let* ws' = A.many1 (token word) in *)
      (*   let left_str = Util.String.from_words ws in *)
      (*   let right_str = Util.String.from_words ws' in *)
      (*   let right_str = Format.sprintf "%s is %s" (Eff.to_string e) right_str in *)
      (*   A.return [Other left_str; Other right_str ] *)

      (* Parse the set phrase:

           "En appears in program order between Em and Ek"

         desugaring it into the equivalent conjunction of two constraints:

           "Em appears in program order before En"
           "En appears in program order before Ek"
     *)
      let po_triple : t list A.t =
        let* e1 = token eff in
        let* _ = words "appears in program order between" in
        let* e2 = token eff in
        let* _ = token (A.string "and") in
        let* e3 = token eff in
        let fst_str =
          Format.sprintf "%s appears in program order before %s"
            (Eff.to_string e2) (Eff.to_string e1)
        in
        let snd_str =
          Format.sprintf "%s appears in program order before %s"
            (Eff.to_string e1) (Eff.to_string e3)
        in
        A.return [ Other fst_str; Other snd_str ]

      let other_constr : t list A.t =
        let* ws = A.many1 (token word) in
        A.return [ Other (Util.String.from_words ws) ]

      let clause : t list A.t = A.choice [ chain; po_triple; other_constr ]

      (* Parse subclauses of the form

          "(given that ... , and ... , and ...)"

       desugaring them into a conjunction of all its components.
     *)
      let sub_clauses : t list A.t =
        let* _ = lparen *> words "given that" in
        let* cls = A.sep_by1 (comma *> token (A.string "and")) clause in
        let* _ = rparen in
        A.return (List.concat cls)

      (* Parse negated phrases of the form "it is not the case that ..." *)
      let negation_prefix =
        A.option false (words_ci "it is not the case that" *> A.return true)

      let parser : t list A.t =
        let* is_negated = negation_prefix in
        let* c = clause in
        let* opt = A.option [] sub_clauses in
        let* _ = A.end_of_input in
        let result = c @ opt in
        let result = if is_negated then [ Negated result ] else result in
        A.return result

      let parse str : t list = parse_string parser str
    end :
      sig
        val parse : string -> t list
      end)
end

type rel = Eff.t * Cat.rel_exp * Eff.t
type t = Set of Eff.t * Cat.set_exp | Rel of rel

let negate : t -> t = function
  | Set (e, exp) -> Set (e, Cat.SetExp.negate exp)
  | Rel (x, exp, y) -> Rel (x, Cat.RelExp.negate exp, y)

let pp fmt =
  let open Format in
  function
  | Set (e, s) -> fprintf fmt "Set (%a, %a)" Eff.pp e Cat.SetExp.pp s
  | Rel (x, exp, y) ->
      fprintf fmt "Rel (%a, %a, %a)" Eff.pp x Cat.RelExp.pp exp Eff.pp y

let dict_relations =
  let mk_relation (name, body) = (name, body, Subst.is_reverse body) in
  let extra_relations = [ ("rf", "{1}Reads-from{0}") ] in
  Dict.relations @ extra_relations |> List.map mk_relation

(* This module is concerned with interpreting a raw string as an
   intersection of cat identifiers, according to the mapping from cat
   identifiers to prose defined in herd/libdir/catdefinitions.tex.
   In other words, it a reversal of such mapping. *)
module CatDefinitions (P : NameParser.S) = struct
  let subst_effs body (effs : Eff.t list) =
    let args = Array.of_list (List.map Eff.to_string effs) in
    Subst.subst body args

  let subst_relation ~is_inverse body (fst : Eff.t) (snd : Eff.t) =
    if is_inverse then subst_effs body [ snd; fst ]
    else subst_effs body [ fst; snd ]

  let interpret_rel_name str : string option =
    let str = Util.String.remove_spaces str in
    let score (_, name) = Util.distance str name in
    let is_better ~than d = d < than in
    let (nm, _), best_d = Util.find_best ~score ~is_better Dict.names in
    if best_d > 2 then None
    else
      let names = P.parse_names nm in
      match names with [ nm ] -> Some nm | _ -> None

  let interpret_rel_exp fst_arg snd_arg : string -> Cat.rel_exp option =
   fun str ->
    let str = Util.String.remove_spaces str in
    let score (_, body, is_inverse) =
      let s = subst_relation ~is_inverse body fst_arg snd_arg in
      Util.distance str s
    in
    let is_better ~than d = d < than in
    let (name, _, is_inverse), best_d =
      Util.find_best ~score ~is_better dict_relations
    in
    if best_d > 2 then begin
      (* Format.eprintf "Best approximation (%d) for %S: `%s`\n" best_d str name; *)
      None
    end
    else
      let names = match P.parse_names name with [] -> [ "???" ] | l -> l in
      let exp = Cat.RelExp.inter (List.map Cat.RelExp.of_ident names) in
      let exp = if is_inverse then Cat.RelExp.invert exp else exp in
      Some exp

  let interpret_miaou_rel fst_arg snd_arg str : t option =
    interpret_rel_exp fst_arg snd_arg str
    |> Option.map (fun exp ->
        match Cat.RelExp.identifiers exp with
        | [ (("ADDR" | "DATA") as nm) ] -> Set (fst_arg, Cat.SetExp.of_ident nm)
        | _ -> Rel (fst_arg, exp, snd_arg))

  let interpret_miaou_set arg : string -> t option =
   fun str ->
    let str = String.(concat "" (split_on_char ' ' str)) in
    let score (_, body) = Util.distance str (subst_effs body [ arg ]) in
    let is_better ~than d = d < than in
    let (name, _), best_d = Util.find_best ~score ~is_better Dict.sets in
    let exp =
      if best_d > 2 then begin
        (* Format.eprintf "Best approx[%d]: `%s`\n" best_d name; *)
        None
      end
      else
        let names = match P.parse_names name with [] -> [ "???" ] | l -> l in
        Some
          (Cat.SetExp.inter (List.map (fun x -> Cat.SetExp.of_ident x) names))
    in
    Option.map (fun exp -> Set (arg, exp)) exp
end

module MakeInterpreter (P : NameParser.S) : sig
  val interpret_rel_exp : Eff.t -> Eff.t -> string -> Cat.rel_exp option
  val interpret_string : string -> t list
  val interpret_structure : string Structure.t -> t Structure.t
end = struct
  module CatDefs = CatDefinitions (P)

  let interpret_rel_exp = CatDefs.interpret_rel_exp

  let interpret_cat_definition str =
    let args = Eff.detect str in
    match args with
    | [ x ] -> (
        match CatDefs.interpret_miaou_set x str with
        | Some c -> c
        | None -> Set (x, Cat.SetExp.of_ident "???"))
    | [ x; y ] -> (
        match CatDefs.interpret_miaou_rel x y str with
        | Some c -> c
        | None -> Rel (x, Cat.RelExp.of_ident "???", y))
    | _ ->
        let msg = Format.sprintf "Failed to interpret cat definition" in
        raise (Util.Interpret_error { msg; context = str })

  let interpret_chain (ch : PreConstraint.chain) =
    let open PreConstraint in
    let exps =
      ch.names
      |> List.map (fun s ->
          let name =
            Option.value (CatDefs.interpret_rel_name s) ~default:"???"
          in
          Cat.RelExp.of_ident name)
    in
    let exp =
      match ch.connective with
      | And -> Cat.RelExp.inter exps
      | Or -> Cat.RelExp.union exps
    in
    let exp = Cat.RelExp.plus exp in
    Rel (ch.source, exp, ch.target)

  let rec interpret_pre_constr : PreConstraint.t -> t list =
    let open PreConstraint in
    function
    | Other s -> [ interpret_cat_definition s ]
    | Negated c -> (
        match Misc.List.concat_map interpret_pre_constr c with
        | [ c ] -> [ negate c ]
        | _ -> [])
    | Chain ch -> [ interpret_chain ch ]

  let interpret_string str =
    let pre_c = PreConstraint.parse str in
    Misc.List.concat_map interpret_pre_constr pre_c

  let interpret_structure : string Structure.t -> t Structure.t =
    Structure.map_constr (fun s ->
        match interpret_string s with
        | [ c ] -> Structure.constr c
        | cs -> Structure.And (List.map Structure.constr cs))
end
