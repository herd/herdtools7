type normalization_error =
  | Function_not_supported of AST.exp
  | Exp_not_supported of AST.exp
  | Empty_union
  | Unknown_identifier of string
  | Not_a_fence of AST.exp

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
  val rel_diff_opt : rel_nf -> rel_nf -> rel_nf option
  val set_diff : set_nf -> set_nf -> set_nf
  val rel_comp_opt : rel_nf -> rel_nf option
  val set_comp : set_nf -> set_nf
  val pp_set_nf : Format.formatter -> set_nf -> unit
  val pp_rel_nf : Format.formatter -> rel_nf -> unit
end

module Make (NormalForms : S) (Log : Logger.S) = struct
  open NormalForms

  let unroll (n : int) (e : rel_nf) : rel_nf =
    rel_union_l
      (List.init n (fun i -> rel_seq_l (List.init (n - i) (fun _ -> e))))

  (* Environments store the result of normalizing previous let bindings.

     Normalizations are wrapped in Lazy.t so that they are only performed if
     and when needed.
     If also ensures that we only report errors for bindings that are relevant
     to the user's request.

     As normalization can potentially fail, environment values are also wrapped
     in `optional` to signal whether a normalization succeeded or failed.
  *)
  type env = (set_nf, rel_nf) Either.t option Lazy.t StringMap.t

  let find_env_set (v : string) (env : env) : set_nf option =
    let open Util.Option.Infix in
    let* lazy_nf = StringMap.find_opt v env in
    let* nf = Lazy.force_val lazy_nf in
    Either.find_left nf

  let find_env_rel (v : string) (env : env) : rel_nf option =
    let open Util.Option.Infix in
    let* lazy_nf = StringMap.find_opt v env in
    let* nf = Lazy.force_val lazy_nf in
    Either.find_right nf

  type config = {
    conditions : string list;
    unroll_depth : int;
    set_var : string -> set_nf option;
    rel_var : string -> rel_nf option;
  }

  let rec normalize_set ~(config : config) ~(env : env) : AST.exp -> set_nf =
    let open AST in
    let conditions = config.conditions in
    let set_var = config.set_var in
    let rel_var = config.rel_var in
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
              domain (normalize_rel ~config ~env e)
          | Var (_, v) when v = "range" -> range (normalize_rel ~config ~env e)
          | _ -> raise (NormalizationError (Function_not_supported e)))
      | If (_, VariantCond a, exp, exp2) ->
          if Ast_utils.eval_variant_cond ~conditions a then go exp else go exp2
      | Try (_, e, e2) -> ( try go e with NormalizationError _ -> go e2)
      | Var (_, var) -> (
          let bound_nf =
            if Option.is_some (rel_var var) then None
            else
              Util.Option.choice_fn
                [ (fun _ -> set_var var); (fun _ -> find_env_set var env) ]
          in
          match bound_nf with
          | Some nf -> nf
          | None -> raise (NormalizationError (Unknown_identifier var)))
      | e -> raise (NormalizationError (Exp_not_supported e))
    in
    go

  and normalize_rel ~(config : config) ~(env : env) (e : AST.exp) : rel_nf =
    let open AST in
    let conditions = config.conditions in
    let rel_var = config.rel_var in
    let rec go e =
      match e with
      | Op (_, Union, expl) ->
          let nfs =
            List.filter_map
              (fun e ->
                try Some (go e)
                with NormalizationError err ->
                  Log.eprintv 1 "Skipping union branch: %s@." (pp_norm_err err);
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
      | Op (_, Diff, [ e1; e2 ]) -> (
          match rel_diff_opt (go e1) (go e2) with
          | Some result -> result
          | None -> raise (NormalizationError (Exp_not_supported e)))
      | Op1 (_, ToId, exp) -> to_id (normalize_set ~config ~env exp)
      | Op1 (_, Inv, exp) -> inv (go exp)
      | Op1 (_, Comp, exp) -> (
          match rel_comp_opt (go exp) with
          | Some result -> result
          | None -> raise (NormalizationError (Exp_not_supported e)))
      | Op1 (_, Plus, exp) -> unroll config.unroll_depth (go exp)
      | Op1 (_, Star, exp) ->
          let unrolled = unroll config.unroll_depth (go exp) in
          rel_union_l [ unrolled; empty_rel ]
      | Op1 (_, Opt, exp) -> rel_union_l [ go exp; empty_rel ]
      | Konst (_, Empty _) -> empty_rel
      | App (_, fexp, exp) -> (
          match fexp with
          | Var (_, "fencerel") -> (
              let nf = normalize_set ~config ~env exp in
              match find_fence nf with
              | Some fence -> fencerel fence
              | None -> raise (NormalizationError (Not_a_fence exp)))
          | _ -> raise (NormalizationError (Function_not_supported e)))
      | If (_, VariantCond a, exp, exp2) ->
          if Ast_utils.eval_variant_cond ~conditions a then go exp else go exp2
      | Try (_, e, e2) -> ( try go e with NormalizationError _ -> go e2)
      | Var (_, var) -> (
          let bound_nf =
            Util.Option.choice_fn
              [ (fun () -> rel_var var); (fun () -> find_env_rel var env) ]
          in
          match bound_nf with
          | Some nf -> nf
          | None -> raise (NormalizationError (Unknown_identifier var)))
      | _ -> raise (NormalizationError (Exp_not_supported e))
    in
    go e

  (* Normalize the given AST expression.
     May fail by throwing NormalizationError.
  *)
  (* TODO: properly handle recursive bindings *)
  let normalize ~(config : config) ~(env : env) (e : AST.exp) :
      (set_nf, rel_nf) Either.t =
    try Either.Left (normalize_set ~config ~env e)
    with NormalizationError _ -> Either.Right (normalize_rel ~config ~env e)

  type nf_map = string -> (rel_nf * AST.exp option) list

  (* Assumes that the order of let-definitions in `bindings` is that of
     the source cat file. This is important to properly scope bindings in
     the presence of shadowing. *)
  let normalize_bindings ~(config : config) (bindings : (string * AST.exp) list)
      : nf_map =
   fun var ->
    let bindings_in_scope_rev =
      bindings |> List.rev |> Util.List.drop_while (fun (v, _) -> not (v = var))
    in
    match bindings_in_scope_rev with
    | [] ->
        Log.eprintv 0 "Requested let binding `%s` not found in cat file.@." var;
        []
    | (_, e) :: env_rev -> begin
        match config.rel_var var with
        | Some nf -> [ (nf, None) ]
        | None ->
            let env =
              List.fold_left
                (fun (env : env) (v, e) ->
                  let f =
                   fun () ->
                    Log.eprintv 1 "Normalizing let binding `%s`.@." v;
                    try Some (normalize ~config ~env e)
                    with NormalizationError err ->
                      Log.eprintv 1 "Skipping let binding `%s`: %s@." v
                        (pp_norm_err err);
                      None
                  in
                  StringMap.add v (Lazy.from_fun f) env)
                StringMap.empty (List.rev env_rev)
            in
            begin match e with
            | AST.Op (_, AST.Union, expl) ->
                let nfs =
                  List.filter_map
                    (fun e ->
                      try
                        let nf = normalize_rel ~config ~env e in
                        Some (nf, Some e)
                      with NormalizationError err ->
                        Log.eprintv 1 "Skipping union branch: %s@."
                          (pp_norm_err err);
                        None)
                    expl
                in
                nfs
            | _ -> (
                try [ (normalize_rel ~config ~env e, None) ]
                with NormalizationError err ->
                  Log.eprintv 1 "Skipping let binding `%s`: %s@." var
                    (pp_norm_err err);
                  [])
            end
      end
end
