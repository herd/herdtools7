(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2025 Arm Limited and/or its affiliates                         *)
(* <open-source-office@arm.com>                                             *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

let src = Logs.Src.create "normalization"

module Log = (val Logs.src_log src : Logs.LOG)

type normalization_error =
  | Function_not_supported of AST.exp
  | Exp_not_supported of AST.exp
  | Empty_union
  | Unknown_identifier of string
  | Not_a_fence of AST.exp
  | Recursion_not_supported of string

exception NormalizationError of normalization_error

let pp_norm_err : normalization_error -> string =
 fun err ->
  let open Format in
  match err with
  | Function_not_supported e ->
      asprintf "Function_not_supported: %a" AstUtils.pp_exp e
  | Exp_not_supported e -> asprintf "Exp_not_supported: %a" AstUtils.pp_exp e
  | Empty_union -> sprintf "Empty_union"
  | Unknown_identifier s -> sprintf "Unknown_identifier: %s" s
  | Not_a_fence e -> asprintf "Not_a_fence: %a" AstUtils.pp_exp e
  | Recursion_not_supported v -> asprintf "Recursion_not_supported: %s" v

type binding = { body : AST.exp; is_recursive : bool }

module type S = sig
  type fence
  type rel_nf
  type set_nf

  val empty_set : set_nf
  val empty_rel : rel_nf

  (* Operations on sets and relations *)
  val rel_union_l : rel_nf list -> rel_nf
  val set_union_l : set_nf list -> set_nf
  val rel_seq_l : rel_nf list -> rel_nf
  val rel_inter_l : rel_nf list -> rel_nf option
  val set_inter_l : set_nf list -> set_nf
  val inv : rel_nf -> rel_nf (* Relation inversion *)
  val to_id : set_nf -> rel_nf (* Construct identity/reflexive relation *)
  val set_diff : set_nf -> set_nf -> set_nf (* Set difference *)
  val set_comp : set_nf -> set_nf (* Set complement *)

  (* These functions determines the interpretation of set-typed and
     relation-typed identifiers.
     They can be used to define the meaning of builtin cat symbols, or to
     override the interpretation of user-defined bindings. *)
  val parse_set_id : string -> set_nf option
  val parse_rel_id : string -> rel_nf option

  (* Check whether the given set expression represents a fence effect *)
  val find_fence : set_nf -> fence option

  (* Builtin cat functions *)
  val domain : rel_nf -> set_nf
  val range : rel_nf -> set_nf
  val fencerel : fence -> rel_nf

  (* Pretty-printing *)
  val pp_set_nf : Format.formatter -> set_nf -> unit
  val pp_rel_nf : Format.formatter -> rel_nf -> unit
end

type config = { variants : string list; unroll_depth : int }

module Make (NF : S) : sig
  type nf_map

  (* Compute normal forms for the provided bindings.
     Results are stored in a map of type [nf_map] which can be accessed
     via [find_opt].
   *)
  val normalize_bindings : config:config -> (string * binding) list -> nf_map

  (* Find a normalized binding within a [nf_map], by name.

     Successful results are given as a list of normal forms and source AST expression,
     such that if the requested binding is, in the source AST, of the form

         let b = exp1 | exp2 | ... | expn

     then the result of [find_opt "b" m] is a list

         [ (nf1, exp1); (nf2, exp2); ...; (nfn; expn) ]

     where nf_i is the normal form of exp_i for all i.
   *)
  val find_opt : string -> nf_map -> (NF.rel_nf * AST.exp) list option
end = struct
  open NF

  let unroll (n : int) (e : rel_nf) : rel_nf =
    rel_union_l
      (List.init n (fun i -> rel_seq_l (List.init (n - i) (fun _ -> e))))

  (* A normal form is either a normal set or normal relation. *)
  type nf = (set_nf, rel_nf) Either.t

  (* Environments store the result of normalizing previous let bindings.

     Normalizations are wrapped in Lazy.t so that they are only performed if
     downstream callers actually need them.
     Among other things, this makes it easier to only report errors for
     bindings that are relevant to the user's request.

     As normalization can potentially fail, environment values are also wrapped
     in `optional` to signal whether a normalization succeeded or failed.
  *)
  type env = nf option Lazy.t StringMap.t

  let find_env (v : string) (env : env) : nf option =
    let open Util.Option.Infix in
    let* lazy_nf = StringMap.find_opt v env in
    Lazy.force_val lazy_nf

  let find_env_set v env = Option.bind (find_env v env) Either.find_left
  let find_env_rel v env = Option.bind (find_env v env) Either.find_right

  let rec normalize_set ~(config : config) ~(env : env) ~(name : string)
      ~(is_recursive : bool) : AST.exp -> set_nf =
    let open AST in
    let variants = config.variants in
    let rec go = function
      | Op (_, Union, expl) ->
          let nfs =
            List.filter_map
              (fun e -> try Some (go e) with NormalizationError _ -> None)
              expl
          in
          if List.length nfs = 0 then raise (NormalizationError Empty_union)
          else set_union_l nfs
      | Op (_, Inter, expl) -> set_inter_l (List.map go expl)
      | Op (_, Diff, [ e1; e2 ]) -> set_diff (go e1) (go e2)
      | Op1 (_, Comp, exp) ->
          let nf = go exp in
          let res = set_comp nf in
          res
      | App (_, fn, e) -> (
          match fn with
          | Var (_, v) when v = "domain" ->
              domain (normalize_rel ~config ~env ~name ~is_recursive e)
          | Var (_, v) when v = "range" ->
              range (normalize_rel ~config ~env ~name ~is_recursive e)
          | _ -> raise (NormalizationError (Function_not_supported e)))
      | If (_, VariantCond a, exp, exp2) ->
          if AstUtils.eval_variant_cond ~variants a then go exp else go exp2
      | Try (_, e, e2) -> ( try go e with NormalizationError _ -> go e2)
      | Var (_, var) when var = name && is_recursive ->
          raise (NormalizationError (Recursion_not_supported var))
      | Var (_, var) -> (
          let bound_nf =
            if Option.is_some (parse_rel_id var) then None
            else
              Util.Option.choice_fn
                [ (fun _ -> parse_set_id var); (fun _ -> find_env_set var env) ]
          in
          match bound_nf with
          | Some nf -> nf
          | None -> raise (NormalizationError (Unknown_identifier var)))
      | e -> raise (NormalizationError (Exp_not_supported e))
    in
    go

  and normalize_rel ~(config : config) ~(env : env) ~(name : string)
      ~(is_recursive : bool) (e : AST.exp) : rel_nf =
    let open AST in
    let variants = config.variants in
    let rec go e =
      match e with
      | Op (_, Union, expl) ->
          let nfs =
            List.filter_map
              (fun e ->
                try Some (go e)
                with NormalizationError err ->
                  Log.debug (fun m ->
                      m "Skipping union branch: %s" (pp_norm_err err));
                  None)
              expl
          in
          if List.length nfs = 0 then raise (NormalizationError Empty_union)
          else rel_union_l nfs
      | Op (_, Seq, expl) -> rel_seq_l (List.map go expl)
      | Op (_, Inter, expl) -> (
          let nfs = List.map go expl in
          match rel_inter_l nfs with
          | Some nf -> nf
          | None -> raise (NormalizationError (Exp_not_supported e)))
      | Op (_, Diff, _) -> raise (NormalizationError (Exp_not_supported e))
      | Op1 (_, ToId, exp) ->
          to_id (normalize_set ~config ~env ~name ~is_recursive exp)
      | Op1 (_, Inv, exp) -> inv (go exp)
      | Op1 (_, Comp, _) -> raise (NormalizationError (Exp_not_supported e))
      | Op1 (_, Plus, exp) -> unroll config.unroll_depth (go exp)
      | Op1 (_, Star, exp) ->
          let unrolled = unroll config.unroll_depth (go exp) in
          rel_union_l [ unrolled; empty_rel ]
      | Op1 (_, Opt, exp) -> rel_union_l [ go exp; empty_rel ]
      | Konst (_, Empty _) -> empty_rel
      | App (_, fexp, exp) -> (
          match fexp with
          | Var (_, "fencerel") -> (
              let nf = normalize_set ~config ~env ~name ~is_recursive exp in
              match find_fence nf with
              | Some fence -> fencerel fence
              | None -> raise (NormalizationError (Not_a_fence exp)))
          | _ -> raise (NormalizationError (Function_not_supported e)))
      | If (_, VariantCond a, exp, exp2) ->
          if AstUtils.eval_variant_cond ~variants a then go exp else go exp2
      | Try (_, e, e2) -> ( try go e with NormalizationError _ -> go e2)
      | Var (_, var) when var = name && is_recursive ->
          raise (NormalizationError (Recursion_not_supported var))
      | Var (_, var) -> (
          let bound_nf =
            Util.Option.choice_fn
              [ (fun () -> parse_rel_id var); (fun () -> find_env_rel var env) ]
          in
          match bound_nf with
          | Some nf -> nf
          | None -> raise (NormalizationError (Unknown_identifier var)))
      | _ -> raise (NormalizationError (Exp_not_supported e))
    in
    go e

  (* Normalize the given AST expression.
     Since AST.exp does not carry type information, and normalization operates
     differently for sets vs relations, we first attempt to normalize the
     expression as a set, then attempt it as a relation on failure.

     May fail by throwing NormalizationError if both attempts fail.
  *)
  let normalize_binding ~(config : config) ~(env : env) ~(name : string)
      (b : binding) : nf =
    Log.info (fun m -> m "Processing let binding `%s`" name);
    let e = b.body in
    let is_recursive = b.is_recursive in
    try Either.Left (normalize_set ~config ~env ~name ~is_recursive e)
    with NormalizationError _ ->
      Either.Right (normalize_rel ~config ~env ~name ~is_recursive e)

  type nf_map = (rel_nf * AST.exp) list StringMap.t

  let find_opt = StringMap.find_opt

  (* Assumes that the order of let-definitions in `bindings` is that of
     the source cat file. This is important to properly scope bindings in
     the presence of shadowing. *)
  let normalize_bindings ~(config : config) (bindings : (string * binding) list)
      : nf_map =
    let _, nf_bindings_rev =
      List.fold_left
        (fun (env, l) (name, b) ->
          let do_normalize () =
            try Some (normalize_binding ~config ~env ~name b)
            with NormalizationError err ->
              Log.warn (fun m ->
                  m "Skipping let binding `%s`: %s" name (pp_norm_err err));
              None
          in
          let new_env = StringMap.add name (Lazy.from_fun do_normalize) env in
          let item = (name, b, env) in
          (new_env, item :: l))
        (StringMap.empty, []) bindings
    in
    let nf_bindings = List.rev nf_bindings_rev in
    nf_bindings
    |> List.map (fun (name, b, env) ->
        let nfs =
          match parse_rel_id name with
          | Some nf -> [ (nf, b.body) ]
          | None ->
              let expl =
                match b.body with
                | AST.Op (_, AST.Union, expl) -> expl
                | e -> [ e ]
              in
              List.filter_map
                (fun e ->
                  try
                    let nf =
                      normalize_rel ~config ~env ~name
                        ~is_recursive:b.is_recursive e
                    in
                    Some (nf, e)
                  with NormalizationError err ->
                    Log.debug (fun m ->
                        m "Skipping `%s` component `%a`: %s" name
                          AstUtils.pp_exp e (pp_norm_err err));
                    None)
                expl
        in
        (name, nfs))
    |> StringMap.from_bindings
end
