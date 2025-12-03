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
      asprintf "Function_not_supported: %a" Ast_utils.pp_exp e
  | Exp_not_supported e -> asprintf "Exp_not_supported: %a" Ast_utils.pp_exp e
  | Empty_union -> sprintf "Empty_union"
  | Unknown_identifier s -> sprintf "Unknown_identifier: %s" s
  | Not_a_fence e -> asprintf "Not_a_fence: %a" Ast_utils.pp_exp e
  | Recursion_not_supported v -> asprintf "Recursion_not_supported: %s" v

type binding = { body : AST.exp; is_recursive : bool }

module type S = sig
  type fence
  type rel_nf
  type set_nf

  val empty_set : set_nf
  val empty_rel : rel_nf
  val domain : rel_nf -> set_nf
  val range : rel_nf -> set_nf
  val find_fence : set_nf -> fence option
  val fencerel : fence -> rel_nf
  val rel_union_l : rel_nf list -> rel_nf
  val set_union_l : set_nf list -> set_nf
  val rel_seq_l : rel_nf list -> rel_nf
  val rel_inter_l : rel_nf list -> rel_nf option
  val set_inter_l : set_nf list -> set_nf
  val inv : rel_nf -> rel_nf
  val to_id : set_nf -> rel_nf
  val set_diff : set_nf -> set_nf -> set_nf
  val set_comp : set_nf -> set_nf
  val parse_set_id : string -> set_nf option
  val parse_rel_id : string -> rel_nf option
  val pp_set_nf : Format.formatter -> set_nf -> unit
  val pp_rel_nf : Format.formatter -> rel_nf -> unit
end

module Make (NormalForms : S) = struct
  open NormalForms

  let unroll (n : int) (e : rel_nf) : rel_nf =
    rel_union_l
      (List.init n (fun i -> rel_seq_l (List.init (n - i) (fun _ -> e))))

  (* A normal form is either a normal set or normal relation. *)
  type nf = (set_nf, rel_nf) Either.t

  (* Environments store the result of normalizing previous let bindings.

     Normalizations are wrapped in Lazy.t so that they are only performed if
     and when needed.
     If also ensures that we only report errors for bindings that are relevant
     to the user's request.

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

  type config = { conditions : string list; unroll_depth : int }

  let rec normalize_set ~(config : config) ~(env : env) ~(name : string)
      ~(is_recursive : bool) : AST.exp -> set_nf =
    let open AST in
    let conditions = config.conditions in
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
          if Ast_utils.eval_variant_cond ~conditions a then go exp else go exp2
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
    let conditions = config.conditions in
    let rec go e =
      match e with
      | Op (_, Union, expl) ->
          let nfs =
            List.filter_map
              (fun e ->
                try Some (go e)
                with NormalizationError err ->
                  Log.debug (fun m ->
                      m "Skipping union branch: %s@." (pp_norm_err err));
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
          if Ast_utils.eval_variant_cond ~conditions a then go exp else go exp2
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
    Log.info (fun m -> m "Processing let binding `%s`@." name);
    let e = b.body in
    let is_recursive = b.is_recursive in
    try Either.Left (normalize_set ~config ~env ~name ~is_recursive e)
    with NormalizationError _ ->
      Either.Right (normalize_rel ~config ~env ~name ~is_recursive e)

  type nf_map = (rel_nf * AST.exp) list StringMap.t

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
                  m "Skipping let binding `%s`: %s@." name (pp_norm_err err));
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
                        m "Skipping `%s` component `%a`: %s@." name
                          Ast_utils.pp_exp e (pp_norm_err err));
                    None)
                expl
        in
        (name, nfs))
    |> StringMap.from_bindings
end
