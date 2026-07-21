open AST
open ASTUtils
open StaticEnv

(** [unconstrained_integer_ty ~loc] is an unconstrained integer type carrying
    the source position of [loc]. *)
let unconstrained_integer_ty ~loc = T_Int UnConstrained |> add_pos_from loc

(** [bool_ty ~loc] is a Boolean type carrying the source position of [loc]. *)
let bool_ty ~loc = T_Bool |> add_pos_from loc

(** [map_untyped_desc node f] applies [f] to the descriptor of a node that
    cannot carry a type annotation. *)
let map_untyped_desc (node : ('a, no_type_annotation) t_annotated) f =
  { node with desc = f node.desc }

(** [map_typed_desc node ty_opt f] applies [f] to the descriptor of [node] and
    replaces its type annotation with [ty_opt]. *)
let map_typed_desc (node : ('a, ty) t_annotated) ty_opt f =
  { node with desc = f node.desc; ty_opt }

(** [existing_annotation_or_fallback ~existing ~fallback] returns [existing]
    when it is present, and [fallback] otherwise. *)
let[@inline] existing_annotation_or_fallback ~existing ~fallback =
  match existing with Some _ -> existing | None -> fallback

(** [source_position_opt node] returns the printable source position of [node],
    or [None] when [node] has a dummy position. *)
let source_position_opt node =
  if is_dummy_pos node then None else Some (PP.pp_pos_str node)

(** [require_annotation kind pp node annotation] returns the type carried by
    [annotation], or reports a typechecker invariant violation if it is absent.
    [kind] names the kind of annotated construct, [pp] prints [node], and [node]
    supplies both the construct and its source position for the diagnostic. *)
let require_annotation kind pp node = function
  | Some t -> t
  | None ->
      let position =
        match source_position_opt node with
        | None -> "with no source position"
        | Some position -> "at " ^ position
      in
      Format.kasprintf failwith
        "@[<v>Typechecker invariant violated: missing type annotation on %s \
         %s:@,\
         %a@]"
        kind position pp node

(** [complete_expr env ?fallback_ty e] uses [env] to recursively fill missing
    type annotations in [e]. If [e] has no annotation, [fallback_ty] takes
    precedence over a type inferred from the expression's form. Existing
    annotations are preserved.

    The traversal also completes expressions embedded in type annotations. It
    supplies a fallback type to a child only when the surrounding construct
    supplies that type: for example, Boolean conditions, integer indices, tuple
    and record components, and array elements. For comparisons, it can also use
    one operand's existing annotation as the fallback type of the other. Other
    children are completed without a fallback type.

    Ordinary expressions are expected to retain the result annotation assigned
    by the typechecker. They are nevertheless traversed because synthesized
    operands or expressions embedded in their annotations may still contain
    holes. *)
let rec complete_expr env ?fallback_ty e =
  let annotation = complete_expr_annotation env ?fallback_ty e in
  map_typed_desc e annotation @@ complete_expr_desc env annotation

(** [complete_expr_annotation env ?fallback_ty e] uses [env] to complete the
    existing annotation on [e]. If it is absent, [fallback_ty] takes precedence
    over a type inferred from [e]'s descriptor. The descriptor is not traversed.
*)
and complete_expr_annotation env ?fallback_ty e =
  (* Select the supplied annotation before completing it, so an unused fallback
     is not traversed and descriptor inference remains lazy. *)
  let supplied_annotation =
    existing_annotation_or_fallback ~existing:e.ty_opt ~fallback:fallback_ty
  in
  match supplied_annotation with
  | Some t -> Some (complete_ty env t)
  | None -> infer_expr_annotation env e

(** [infer_expr_annotation env e] uses [env] and the descriptor of [e] to infer
    only [e]'s result annotation. It does not traverse child expressions. *)
and infer_expr_annotation env e =
  match e.desc with
  | E_Literal (L_Int _) -> Some (unconstrained_integer_ty ~loc:e)
  | E_Literal (L_Bool _) -> Some (bool_ty ~loc:e)
  | E_Literal (L_Real _) -> Some (T_Real |> add_pos_from e)
  | E_Literal (L_String _) -> Some (T_String |> add_pos_from e)
  | E_Literal (L_BitVector bv) ->
      let width =
        expr_of_int (Bitvector.length bv) |> complete_integer_expr env
      in
      Some (T_Bits (width, []) |> add_pos_from e)
  | E_Binop
      ((`BAND | `BEQ | `BOR | `EQ | `GE | `GT | `IMPL | `LE | `LT | `NE), _, _)
    ->
      Some (bool_ty ~loc:e)
  (* These ordinary expressions are expected to retain their result type from
     typechecking. Their children are still completed below. *)
  | E_Literal (L_Label _)
  | E_Var _ | E_Call _ | E_Slice _ | E_Cond _ | E_GetArray _ | E_GetField _
  | E_GetFields _ | E_GetCollectionFields _ | E_GetItem _ | E_Pattern _
  | E_Binop _ | E_Unop _ ->
      None
  (* Synthesized structured base values obtain their result type from their
     surrounding declaration or structured value. *)
  | E_Tuple _ | E_Array _ -> None
  | E_ATC (_, t) | E_Record (t, _) | E_Arbitrary t -> Some (complete_ty env t)

(** [complete_expr_desc env annotation desc] uses [env] to recursively complete
    the child expressions and embedded types in [desc], using [annotation] for
    structural fallback-type propagation. *)
and complete_expr_desc env annotation =
  let unfolded_annotation =
    match annotation with
    | None -> None
    (* Unfold a named type so fallback-type propagation can inspect its outer
       integer, tuple, or array structure. This is only an inspection view; the
       original annotation above remains unchanged. *)
    | Some t -> Some (Types.make_anonymous env t)
  in
  let is_integer =
    match unfolded_annotation with
    | Some { desc = T_Int _; _ } -> true
    | _ -> false
  in
  (* The cases below pass [~fallback_ty] when the parent fixes a child's type, for
     example a Boolean condition or an integer index. Otherwise the child is
     traversed without supplying a type: its existing annotation is preserved,
     or its type is inferred only from the child's own expression form. *)
  function
  | (E_Literal _ | E_Var _ | E_GetCollectionFields _) as desc ->
      (* These descriptors contain no child expressions or embedded types, so the
         descriptor phase has nothing to traverse. The annotation phase preserves
         an existing or fallback type; when it cannot infer one, a hole remains. *)
      desc
  | E_ATC (operand, t) ->
      (* The annotation is the conversion's result type, which need not be the
         operand's source type, so it is not propagated to [operand]. *)
      E_ATC (complete_expr env operand, complete_ty env t)
  | E_Binop (op, e1, e2) ->
      (* [fallback_operand_ty e] returns the type fixed for operand [e] by [op]
         and the completed result, when such a type exists. *)
      let fallback_operand_ty e =
        if is_integer then
          (* Missing integer operands here are primarily due to normalization. *)
          Some (unconstrained_integer_ty ~loc:e)
        else
          match op with
          | `BAND | `BEQ | `BOR | `IMPL -> Some (bool_ty ~loc:e)
          | _ -> None
      in
      let e1 = complete_expr env ?fallback_ty:(fallback_operand_ty e1) e1
      and e2 = complete_expr env ?fallback_ty:(fallback_operand_ty e2) e2 in
      let e1, e2 =
        match op with
        (* A comparison does not fix its operand type. If independent
           completion inferred one operand's type, use it for the other. *)
        | `EQ | `NE | `GE | `GT | `LE | `LT -> (
            match (e1.ty_opt, e2.ty_opt) with
            | None, Some t -> (complete_expr env ~fallback_ty:t e1, e2)
            | Some t, None -> (e1, complete_expr env ~fallback_ty:t e2)
            | _ -> (e1, e2))
        | _ -> (e1, e2)
      in
      E_Binop (op, e1, e2)
  | E_Unop (op, operand) ->
      (* Integer negation changes constraints, so propagate the unconstrained
         integer type rather than the unary expression's result annotation. *)
      let operand =
        if is_integer then complete_integer_expr env operand
        else complete_expr env operand
      in
      E_Unop (op, operand)
  | E_Call call -> E_Call (complete_call env call)
  | E_Slice (base, slices) ->
      E_Slice (complete_expr env base, List.map (complete_slice env) slices)
  | E_Cond (cond, if_true, if_false) ->
      E_Cond
        ( complete_expr env ~fallback_ty:(bool_ty ~loc:cond) cond,
          complete_expr env ?fallback_ty:annotation if_true,
          complete_expr env ?fallback_ty:annotation if_false )
  | E_GetArray (base, index) ->
      E_GetArray (complete_expr env base, complete_integer_expr env index)
  | E_GetField (base, field) -> E_GetField (complete_expr env base, field)
  | E_GetFields (base, fields) -> E_GetFields (complete_expr env base, fields)
  | E_GetItem (base, item) -> E_GetItem (complete_expr env base, item)
  | E_Record (record_ty, fields) ->
      let record_ty = complete_ty env record_ty in
      let field_types =
        match (Types.make_anonymous env record_ty).desc with
        | T_Record fields | T_Exception fields | T_Collection fields -> fields
        | _ -> []
      in
      (* [complete_field (name, value)] preserves field [name] and completes
         [value] using the corresponding type from the record structure. *)
      let complete_field (name, value) =
        ( name,
          complete_expr env ?fallback_ty:(List.assoc_opt name field_types) value
        )
      in
      E_Record (record_ty, List.map complete_field fields)
  | E_Tuple items ->
      let fallback_item_tys =
        match unfolded_annotation with
        | Some { desc = T_Tuple tys; _ } -> tys
        | _ -> []
      in
      let items =
        if List.compare_lengths items fallback_item_tys = 0 then
          List.map2
            (fun t e -> complete_expr env ~fallback_ty:t e)
            fallback_item_tys items
        else
          (* Without a matching tuple structure, there is no component type that
           can be propagated safely, so complete each item independently. *)
          List.map (complete_expr env) items
      in
      E_Tuple items
  | E_Array { length; value } ->
      let length_ty, element_ty =
        match unfolded_annotation with
        | Some { desc = T_Array (type_length, element_ty); _ } ->
            (type_length.ty_opt, Some element_ty)
        | _ -> (None, None)
      in
      let length =
        match length_ty with
        | Some t -> complete_expr env ~fallback_ty:t length
        | None -> complete_integer_expr env length
      in
      E_Array
        { length; value = complete_expr env ?fallback_ty:element_ty value }
  | E_Arbitrary t -> E_Arbitrary (complete_ty env t)
  | E_Pattern (value, matcher) ->
      let value = complete_expr env value in
      let matcher =
        complete_pattern_matcher env ?fallback_ty:value.ty_opt matcher
      in
      E_Pattern (value, matcher)

(** [complete_integer_expr env e] uses [env] to complete [e], using
    unconstrained integer as its fallback type when its annotation is missing.
*)
and complete_integer_expr env e =
  complete_expr env ~fallback_ty:(unconstrained_integer_ty ~loc:e) e

(** [complete_typechecked_expr env e] uses [env] to complete the contents of
    expression [e] after checking that ordinary typechecking annotated its
    result. *)
and complete_typechecked_expr env e =
  let _ = require_annotation "expression" PP.pp_expr e e.ty_opt in
  complete_expr env e

(** [complete_call env call] uses [env] to complete the actual parameters and
    arguments of [call]. *)
and complete_call env call =
  {
    call with
    (* Explicit actual parameters have already passed through [annotate_expr]
       and retain those annotations. Omitted standard-library parameters are
       inserted later and may still have [ty_opt = None]. We traverse both
       uniformly: [complete_integer_expr] preserves the former and completes
       the latter as unconstrained integers. *)
    params = List.map (complete_integer_expr env) call.params;
    (* Unlike omitted parameters, every argument has passed through
       [annotate_expr] and must retain its result annotation. *)
    args = List.map (complete_typechecked_expr env) call.args;
  }

(** [complete_slice env slice] uses [env] to complete the integer expressions in
    [slice]. *)
and complete_slice env = function
  | Slice_Single e -> Slice_Single (complete_integer_expr env e)
  | Slice_Range (e1, e2) ->
      Slice_Range (complete_integer_expr env e1, complete_integer_expr env e2)
  | Slice_Length (e1, e2) ->
      Slice_Length (complete_integer_expr env e1, complete_integer_expr env e2)
  | Slice_Star (e1, e2) ->
      Slice_Star (complete_integer_expr env e1, complete_integer_expr env e2)

(** [complete_pattern env ?fallback_ty p] uses [env] to complete expressions in
    pattern [p], propagating [fallback_ty] when available. *)
and complete_pattern env ?fallback_ty p =
  map_untyped_desc p @@ function
  | Pattern_All | Pattern_Mask _ -> p.desc
  | Pattern_Geq e -> Pattern_Geq (complete_expr env ?fallback_ty e)
  | Pattern_Leq e -> Pattern_Leq (complete_expr env ?fallback_ty e)
  | Pattern_Single e -> Pattern_Single (complete_expr env ?fallback_ty e)
  | Pattern_Range (e1, e2) ->
      Pattern_Range
        (complete_expr env ?fallback_ty e1, complete_expr env ?fallback_ty e2)

(** [complete_pattern_matcher env ?fallback_ty matcher] uses [env] to complete
    every pattern in [matcher], propagating [fallback_ty] when available. *)
and complete_pattern_matcher env ?fallback_ty (patterns, kind) =
  (List.map (complete_pattern env ?fallback_ty) patterns, kind)

(** [complete_constraint env c] uses [env] to recursively complete the integer
    expressions in constraint [c]. *)
and complete_constraint env = function
  | Constraint_Exact e -> Constraint_Exact (complete_integer_expr env e)
  | Constraint_Range (e1, e2) ->
      Constraint_Range
        (complete_integer_expr env e1, complete_integer_expr env e2)

(** [complete_bitfield env bitfield] uses [env] to complete slices and nested
    types in [bitfield]. *)
and complete_bitfield env = function
  | BitField_Simple (name, slices) ->
      BitField_Simple (name, List.map (complete_slice env) slices)
  | BitField_Nested (name, slices, bitfields) ->
      BitField_Nested
        ( name,
          List.map (complete_slice env) slices,
          List.map (complete_bitfield env) bitfields )
  | BitField_Type (name, slices, field_ty) ->
      BitField_Type
        (name, List.map (complete_slice env) slices, complete_ty env field_ty)

(** [complete_ty env t] uses [env] to complete expressions embedded in type [t],
    recursively including constraints, widths, array lengths, and bitfield
    slices. *)
and complete_ty env t =
  map_untyped_desc t @@ function
  | T_Int (WellConstrained (constraints, precision)) ->
      T_Int
        (WellConstrained
           (List.map (complete_constraint env) constraints, precision))
  | T_Bits (width, bitfields) ->
      T_Bits
        ( complete_integer_expr env width,
          List.map (complete_bitfield env) bitfields )
  | T_Tuple tys -> T_Tuple (List.map (complete_ty env) tys)
  | T_Array (length, element_ty) ->
      T_Array (complete_integer_expr env length, complete_ty env element_ty)
  | T_Record fields ->
      T_Record (List.map (fun (name, t) -> (name, complete_ty env t)) fields)
  | T_Exception fields ->
      T_Exception (List.map (fun (name, t) -> (name, complete_ty env t)) fields)
  | T_Collection fields ->
      T_Collection
        (List.map (fun (name, t) -> (name, complete_ty env t)) fields)
  | T_Int (UnConstrained | PendingConstrained | Parameterized _)
  | T_Real | T_String | T_Bool | T_Enum _ | T_Named _ ->
      t.desc

(** [complete_lexpr env le] uses [env] to complete annotations within assignable
    expression [le], requiring every assignable-expression node to retain the
    result annotation assigned by ordinary typechecking. *)
let rec complete_lexpr env le =
  let annotation =
    require_annotation "assignable expression" PP.pp_lexpr le le.ty_opt
    |> complete_ty env |> Option.some
  in
  map_typed_desc le annotation @@ function
  | (LE_Discard | LE_Var _ | LE_SetCollectionFields _) as desc ->
      (* These descriptors contain no child expressions or embedded types. *)
      desc
  | LE_Slice (base, slices) ->
      LE_Slice (complete_lexpr env base, List.map (complete_slice env) slices)
  | LE_SetArray (base, index) ->
      LE_SetArray (complete_lexpr env base, complete_integer_expr env index)
  | LE_SetField (base, field) -> LE_SetField (complete_lexpr env base, field)
  | LE_SetFields (base, fields, positions) ->
      LE_SetFields (complete_lexpr env base, fields, positions)
  | LE_Destructuring les ->
      (* [annotate_lexpr_ty] assigns each component its actual LHS type. Complete
         each annotation independently instead of deriving it from the
         enclosing tuple annotation. *)
      LE_Destructuring (List.map (complete_lexpr env) les)

(** [complete_stmt env s] uses [env] to complete annotations by recursing into
    expressions and types in statement [s]. *)
let rec complete_stmt env s =
  map_untyped_desc s @@ function
  | S_Pass | S_Unreachable -> s.desc
  | S_Seq (s1, s2) -> S_Seq (complete_stmt env s1, complete_stmt env s2)
  | S_Decl (ldk, ldi, ty_opt, expr_opt) ->
      let ty_opt = Option.map (complete_ty env) ty_opt in
      let expr_opt =
        Option.map (fun e -> complete_expr env ?fallback_ty:ty_opt e) expr_opt
      in
      S_Decl (ldk, ldi, ty_opt, expr_opt)
  | S_Assign (le, e) ->
      (* [annotate_lexpr] assigns every LHS node its actual type, which may be
         less specific than the RHS type. Check both annotations independently
         instead of propagating the RHS type to the LHS. *)
      S_Assign (complete_lexpr env le, complete_typechecked_expr env e)
  | S_Call call -> S_Call (complete_call env call)
  | S_Return expr_opt ->
      S_Return (Option.map (complete_typechecked_expr env) expr_opt)
  | S_Cond (cond, if_true, if_false) ->
      S_Cond
        ( complete_typechecked_expr env cond,
          complete_stmt env if_true,
          complete_stmt env if_false )
  | S_Assert e -> S_Assert (complete_typechecked_expr env e)
  | S_For ({ start_e; end_e; body; limit; dir = _ } as loop) ->
      S_For
        {
          loop with
          start_e = complete_typechecked_expr env start_e;
          end_e = complete_typechecked_expr env end_e;
          body = complete_stmt env body;
          (* Limits pass through normalization after typechecking and may
             therefore require an unconstrained integer fallback. *)
          limit = Option.map (complete_integer_expr env) limit;
        }
  | S_While (cond, limit, body) ->
      S_While
        ( complete_typechecked_expr env cond,
          Option.map (complete_integer_expr env) limit,
          complete_stmt env body )
  | S_Repeat (body, cond, limit) ->
      S_Repeat
        ( complete_stmt env body,
          complete_typechecked_expr env cond,
          Option.map (complete_integer_expr env) limit )
  | S_Throw (e, throw_ty) ->
      (* [annotate_expr] supplies both the expression annotation and
         [throw_ty], so both must already be present. *)
      let throw_ty =
        require_annotation "throw statement" PP.pp_stmt s throw_ty
        |> complete_ty env
      in
      S_Throw (complete_typechecked_expr env e, Some throw_ty)
  | S_Try (body, catchers, otherwise) ->
      (* [complete_catcher (name, catcher_ty, body)] preserves [name] and
         completes [catcher_ty] and [body]. *)
      let complete_catcher (name, catcher_ty, body) =
        (name, complete_ty env catcher_ty, complete_stmt env body)
      in
      S_Try
        ( complete_stmt env body,
          List.map complete_catcher catchers,
          Option.map (complete_stmt env) otherwise )
  | S_Print ({ args; _ } as print) ->
      S_Print
        { print with args = List.map (complete_typechecked_expr env) args }
  | S_Pragma _ -> assert false

(** [complete_decl env d] uses [env] to complete annotations in declaration [d].
*)
let complete_decl env d =
  map_untyped_desc d @@ function
  | D_Func f ->
      let parameters =
        List.map
          (fun (name, ty_opt) -> (name, Option.map (complete_ty env) ty_opt))
          f.parameters
      in
      let args =
        List.map (fun (name, ty_opt) -> (name, complete_ty env ty_opt)) f.args
      in
      let return_type = Option.map (complete_ty env) f.return_type in
      let body =
        match f.body with
        | SB_Primitive _ as body -> body
        | SB_ASL body -> SB_ASL (complete_stmt env body)
      in
      let recurse_limit =
        Option.map (complete_integer_expr env) f.recurse_limit
      in
      D_Func { f with parameters; args; return_type; body; recurse_limit }
  | D_GlobalStorage gsd ->
      let ty = Option.map (complete_ty env) gsd.ty in
      let initial_value =
        Option.map (complete_expr env ?fallback_ty:ty) gsd.initial_value
      in
      D_GlobalStorage { gsd with ty; initial_value }
  | D_TypeDecl (name, t) -> D_TypeDecl (name, complete_ty env t)
  | D_Pragma _ -> assert false

(** [complete_annotations genv ast] fills missing type annotations in the
    typechecked AST [ast], using [genv] to resolve named types. *)
let complete_annotations genv ast =
  let env = with_empty_local genv in
  List.map (complete_decl env) ast

(** Private validation of the invariant established by completion. *)
module Validation = struct
  type missing_type_annotation = {
    kind : string;
    construct : string;
    position : string option;
    contexts : string list;
  }

  exception Missing_type_annotation of missing_type_annotation

  (** [validate_within context f] runs [f] and adds [context] to any missing
      annotation it reports. *)
  let validate_within context f =
    try f ()
    with Missing_type_annotation missing ->
      raise
        (Missing_type_annotation
           { missing with contexts = context :: missing.contexts })

  (** [validate_missing kind pp node] reports a missing annotation on [node],
      using [kind] to name the construct and [pp] to print it. *)
  let validate_missing kind pp node =
    let position = source_position_opt node in
    raise
      (Missing_type_annotation
         {
           kind;
           construct = Format.asprintf "%a" pp node;
           position;
           contexts = [];
         })

  (** [validate_missing_constraint ~loc constraint_ expression] reports a
      missing annotation on [expression] within [constraint_], using [loc] when
      [expression] has no source position. *)
  let validate_missing_constraint ~loc constraint_ expression =
    let position =
      Option.value (source_position_opt expression) ~default:(PP.pp_pos_str loc)
    in
    raise
      (Missing_type_annotation
         {
           kind = "integer constraint";
           construct = Format.asprintf "%a" PP.pp_int_constraint constraint_;
           position = Some position;
           contexts = [];
         })

  (** [validate_pp_contexts f contexts] prints [contexts], the path to a missing
      annotation, to formatter [f]. *)
  let validate_pp_contexts f = function
    | [] -> ()
    | contexts ->
        (* [validate_pp_context f context] prints path step [context] to
           formatter [f]. *)
        let validate_pp_context f context = Format.fprintf f "@,- %s" context in
        Format.fprintf f "@,@[<v>Validation path:%a@]"
          (Format.pp_print_list validate_pp_context)
          contexts

  (** [validate_expr e] checks the annotation on expression [e] and recursively
      validates its embedded types and child expressions. *)
  let rec validate_expr e =
    (match e.ty_opt with
    | None -> validate_missing "expression" PP.pp_expr e
    | Some t -> validate_ty t);
    (* [validate_child role child] validates [child] while recording [role] in
       the parent expression. *)
    let validate_child role child =
      let context =
        match source_position_opt e with
        | None -> Format.asprintf "%s of expression `%a`" role PP.pp_expr e
        | Some position ->
            Format.asprintf "%s of expression `%a` at %s" role PP.pp_expr e
              position
      in
      validate_within context (fun () -> validate_expr child)
    in
    match e.desc with
    | E_Literal _ | E_Var _ | E_GetCollectionFields _ -> ()
    | E_ATC (e, t) ->
        validate_child "operand" e;
        validate_ty t
    | E_Binop (_, e1, e2) ->
        validate_child "left operand" e1;
        validate_child "right operand" e2
    | E_GetArray (e1, e2) ->
        validate_child "array" e1;
        validate_child "index" e2
    | E_Unop (_, e) | E_GetField (e, _) | E_GetFields (e, _) | E_GetItem (e, _)
      ->
        validate_child "operand" e
    | E_Pattern (e, (patterns, _)) ->
        validate_child "matched expression" e;
        List.iter validate_pattern patterns
    | E_Call call -> validate_call call
    | E_Slice (e, slices) ->
        validate_child "sliced expression" e;
        List.iter validate_slice slices
    | E_Cond (e1, e2, e3) ->
        validate_child "condition" e1;
        validate_child "then branch" e2;
        validate_child "else branch" e3
    | E_Record (t, fields) ->
        validate_ty t;
        List.iter
          (fun (name, field) -> validate_child ("field " ^ name) field)
          fields
    | E_Tuple es ->
        List.iteri
          (fun i item -> validate_child (Format.sprintf "tuple item %d" i) item)
          es
    | E_Array { length; value } ->
        validate_child "array length" length;
        validate_child "array value" value
    | E_Arbitrary t -> validate_ty t

  (** [validate_call call] validates every actual parameter and argument in
      [call]. *)
  and validate_call call =
    List.iter validate_expr call.params;
    List.iter validate_expr call.args

  (** [validate_slice slice] validates every expression in [slice]. *)
  and validate_slice = function
    | Slice_Single e -> validate_expr e
    | Slice_Range (e1, e2) | Slice_Length (e1, e2) | Slice_Star (e1, e2) ->
        validate_expr e1;
        validate_expr e2

  (** [validate_pattern p] validates every expression in pattern [p]. *)
  and validate_pattern p =
    match p.desc with
    | Pattern_All | Pattern_Mask _ -> ()
    | Pattern_Geq e | Pattern_Leq e | Pattern_Single e -> validate_expr e
    | Pattern_Range (e1, e2) ->
        validate_expr e1;
        validate_expr e2

  (** [validate_constraint ~loc constraint_] validates the integer expressions
      in [constraint_], using [loc] to contextualize missing source positions.
  *)
  and validate_constraint ~loc constraint_ =
    (* [validate_constraint_expr e] validates [e] with [constraint_] and [loc]
       in the diagnostic path. *)
    let validate_constraint_expr e =
      validate_within
        (Format.asprintf "integer constraint `%a` at %s" PP.pp_int_constraint
           constraint_ (PP.pp_pos_str loc))
        (fun () -> validate_expr e)
    in
    match constraint_ with
    | Constraint_Exact e ->
        if Option.is_none e.ty_opt then
          validate_missing_constraint ~loc constraint_ e;
        validate_constraint_expr e
    | Constraint_Range (e1, e2) ->
        if Option.is_none e1.ty_opt then
          validate_missing_constraint ~loc constraint_ e1;
        if Option.is_none e2.ty_opt then
          validate_missing_constraint ~loc constraint_ e2;
        validate_constraint_expr e1;
        validate_constraint_expr e2

  (** [validate_bitfield bitfield] validates the slices and nested types in
      [bitfield]. *)
  and validate_bitfield = function
    | BitField_Simple (_, slices) -> List.iter validate_slice slices
    | BitField_Nested (_, slices, bitfields) ->
        List.iter validate_slice slices;
        List.iter validate_bitfield bitfields
    | BitField_Type (_, slices, t) ->
        List.iter validate_slice slices;
        validate_ty t

  (** [validate_ty t] recursively validates expressions embedded in type [t]. *)
  and validate_ty t =
    match t.desc with
    | T_Int (WellConstrained (constraints, _)) ->
        List.iter (validate_constraint ~loc:t) constraints
    | T_Bits (width, bitfields) ->
        validate_expr width;
        List.iter validate_bitfield bitfields
    | T_Tuple tys -> List.iter validate_ty tys
    | T_Array (length, element_ty) ->
        validate_expr length;
        validate_ty element_ty
    | T_Record fields | T_Exception fields | T_Collection fields ->
        List.iter (fun (_, t) -> validate_ty t) fields
    | T_Int (UnConstrained | PendingConstrained | Parameterized _)
    | T_Real | T_String | T_Bool | T_Enum _ | T_Named _ ->
        ()

  (** [validate_lexpr le] checks assignable expression [le] and its child
      assignable expressions. *)
  and validate_lexpr le =
    (match le.ty_opt with
    | None -> validate_missing "assignable expression" PP.pp_lexpr le
    | Some t -> validate_ty t);
    match le.desc with
    | LE_Discard | LE_Var _ | LE_SetCollectionFields _ -> ()
    | LE_Slice (le, slices) ->
        validate_lexpr le;
        List.iter validate_slice slices
    | LE_SetArray (le, index) ->
        validate_lexpr le;
        validate_expr index
    | LE_SetField (le, _) | LE_SetFields (le, _, _) -> validate_lexpr le
    | LE_Destructuring les -> List.iter validate_lexpr les

  (** [validate_stmt s] recursively validates annotations throughout statement
      [s]. *)
  and validate_stmt s =
    match s.desc with
    | S_Pass | S_Unreachable -> ()
    | S_Seq (s1, s2) ->
        validate_stmt s1;
        validate_stmt s2
    | S_Decl (_, _, ty_opt, expr_opt) ->
        Option.iter validate_ty ty_opt;
        Option.iter validate_expr expr_opt
    | S_Assign (le, e) ->
        validate_lexpr le;
        validate_expr e
    | S_Call call -> validate_call call
    | S_Return expr_opt -> Option.iter validate_expr expr_opt
    | S_Cond (e, s1, s2) ->
        validate_expr e;
        validate_stmt s1;
        validate_stmt s2
    | S_Assert e -> validate_expr e
    | S_For { start_e; end_e; body; limit; _ } ->
        validate_expr start_e;
        validate_expr end_e;
        Option.iter validate_expr limit;
        validate_stmt body
    | S_While (e, limit, body) ->
        validate_expr e;
        Option.iter validate_expr limit;
        validate_stmt body
    | S_Repeat (body, e, limit) ->
        validate_stmt body;
        validate_expr e;
        Option.iter validate_expr limit
    | S_Throw (e, ty_opt) ->
        validate_expr e;
        Option.iter validate_ty ty_opt
    | S_Try (body, catchers, otherwise) ->
        validate_stmt body;
        List.iter
          (fun (_, catcher_ty, catcher_stmt) ->
            validate_ty catcher_ty;
            validate_stmt catcher_stmt)
          catchers;
        Option.iter validate_stmt otherwise
    | S_Print { args; _ } | S_Pragma (_, args) -> List.iter validate_expr args

  (** [validate_decl_contents d] validates every annotated construct in
      declaration [d]. *)
  and validate_decl_contents d =
    match d.desc with
    | D_Func f ->
        List.iter
          (fun (_, ty_opt) -> Option.iter validate_ty ty_opt)
          f.parameters;
        List.iter (fun (_, t) -> validate_ty t) f.args;
        (match f.body with SB_ASL s -> validate_stmt s | SB_Primitive _ -> ());
        Option.iter validate_ty f.return_type;
        Option.iter validate_expr f.recurse_limit
    | D_GlobalStorage { ty = ty_opt; initial_value; _ } ->
        Option.iter validate_ty ty_opt;
        Option.iter validate_expr initial_value
    | D_TypeDecl (_, t) -> validate_ty t
    | D_Pragma (_, args) -> List.iter validate_expr args

  (** [validate_decl d] validates declaration [d] and reports a contextual
      diagnostic when an annotation is missing. *)
  let validate_decl d =
    try validate_decl_contents d with
    | Missing_type_annotation
        { kind; construct; position = Some position; contexts } ->
        Format.kasprintf failwith
          "@[<v>Missing type annotation on %s at %s:@,%s%a@]" kind position
          construct validate_pp_contexts contexts
    | Missing_type_annotation { kind; construct; position = None; contexts } ->
        Format.kasprintf failwith
          "@[<v>Missing type annotation on %s with no source position:@,\
           %s%a@,\
           @,\
           While validating the declaration at %s:@,\
           @[<v 2>%a@]@]"
          kind construct validate_pp_contexts contexts (PP.pp_pos_str d) PP.pp_t
          [ d ]

  (** [validate_annotations ast] validates every declaration in AST [ast]. *)
  let validate_annotations ast = List.iter validate_decl ast
end

(** [complete ~validate genv ast] uses global environment [genv] to complete
    annotations in [ast] and, when [validate] is true, checks that no holes
    remain. *)
let complete ?(validate = false) genv ast =
  let ast = complete_annotations genv ast in
  if validate then Validation.validate_annotations ast;
  ast
