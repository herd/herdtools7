open AST
open ASTUtils
open StaticEnv

let should_log = ref false

(*-------------------- Helpers --------------------*)

let set l =
  let rec set_aux acc l =
    match l with
    | [] -> List.rev acc
    | x :: xs ->
        if List.mem x acc then set_aux acc xs else set_aux (x :: acc) xs
  in
  set_aux [] l

let difference l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1
let intersection l1 l2 = List.filter (fun x -> List.mem x l2) l1
let subset l1 l2 = List.for_all (fun x -> List.mem x l2) l1
let equal l1 l2 = subset l1 l2 && subset l2 l1
let pp_sep f () = Format.fprintf f ", "

let func_sig_to_string f =
  let params = List.map fst f.parameters in
  let parameters =
    if list_is_empty params then ""
    else Format.asprintf "{%s}" (String.concat ", " params)
  in
  let arguments =
    String.concat ", "
      (List.map (Format.asprintf "%a" PP.pp_typed_identifier) f.args)
  in
  let return =
    match f.return_type with
    | None -> ""
    | Some ty -> Format.asprintf " => %a" PP.pp_ty ty
  in
  Format.asprintf "%s %s(%s)%s" f.name parameters arguments return

let with_file name f =
  let oc =
    open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_text ] 0o666 name
  in
  f oc;
  flush oc;
  close_out oc

(*-------------------- Calculating required parameter declarations --------------------*)

let rec parameters_of_expr ~env e =
  match e.desc with
  | E_Var x -> if is_undefined x env then [ x ] else []
  | E_Binop (_, e1, e2) ->
      parameters_of_expr ~env e1 @ parameters_of_expr ~env e2
  | E_Unop (_, e) -> parameters_of_expr ~env e
  | E_Literal _ -> []
  | _ ->
      Format.eprintf "@[parameters_of_expr:@.%a@.%a@]@." PP.pp_pos e PP.pp_expr
        e;
      failwith "Unsupported"

let parameters_of_constraint ~env c =
  match c with
  | Constraint_Exact e -> parameters_of_expr ~env e
  | Constraint_Range (e1, e2) ->
      parameters_of_expr ~env e1 @ parameters_of_expr ~env e2

let rec parameters_of_ty ~env ~consider_int ty =
  match ty.desc with
  | T_Bits (e, _) -> parameters_of_expr ~env e
  | T_Tuple tys -> list_concat_map (parameters_of_ty ~consider_int ~env) tys
  | T_Int (WellConstrained cs) ->
      if consider_int then list_concat_map (parameters_of_constraint ~env) cs
      else []
  | T_Int UnConstrained | T_Real | T_String | T_Bool | T_Array _ | T_Named _ ->
      []
  | _ ->
      Format.eprintf "@[parameters_of_expr:@.%a@.%a@]@." PP.pp_pos ty PP.pp_ty
        ty;
      failwith "Unsupported"

let types_in_func_sig func_sig =
  let types_in_args = List.map snd func_sig.args in
  let return_type =
    match func_sig.return_type with None -> [] | Some ty -> [ ty ]
  in
  return_type @ types_in_args

let required_declarations ~env func_sig =
  let types = types_in_func_sig func_sig in
  let parameters =
    list_concat_map (parameters_of_ty ~consider_int:true ~env) types
  in
  set parameters

(*-------------------- Parameter declarations which need updating --------------------*)

let parameter_declarations = ref (IMap.empty : ISet.t IMap.t)

let add_parameter_declaration ~loc func_sig ~inferred ~declared =
  let string_opt =
    let missing = difference inferred declared in
    let has_missing = not (list_is_empty missing) in
    let misordered = inferred <> declared in
    if not (has_missing || misordered) then None
    else
      let pp_string_list =
        Format.pp_print_list ~pp_sep Format.pp_print_string
      in
      let pp_missing f missing =
        if has_missing then
          Format.fprintf f "MISSING: %a@." pp_string_list missing
      in
      let pp_misordered f (inferred, declared) =
        if (not has_missing) && misordered then
          Format.fprintf f "MISORDERED: expected {%a}, actual {%a}@."
            pp_string_list inferred pp_string_list declared
      in
      Some
        (Format.asprintf "%a@.%s@.%a%a@." PP.pp_pos loc
           (func_sig_to_string func_sig)
           pp_missing missing pp_misordered (inferred, declared))
  in
  match string_opt with
  | None -> ()
  | Some str ->
      let updater = function
        | None -> Some (ISet.singleton str)
        | Some s -> Some (ISet.add str s)
      in
      parameter_declarations :=
        IMap.update func_sig.name updater !parameter_declarations

let log_parameter_declarations ~loc ~env (func_sig : func) type_checker_inferred
    =
  if
    !should_log
    && (not (Builder.is_stdlib_name func_sig.name))
    && not (func_sig.body = SB_Primitive)
  then
    let inferred = required_declarations ~env func_sig in
    let _ = assert (equal type_checker_inferred inferred) in
    let declared = List.map fst func_sig.parameters in
    add_parameter_declaration ~loc func_sig ~inferred ~declared

let print_parameter_declarations () =
  if !should_log && not (IMap.is_empty !parameter_declarations) then
    with_file "declarations.txt" (fun oc ->
        IMap.iter
          (fun name s ->
            Printf.fprintf oc "*** Function %s: %i declarations ***\n" name
              (ISet.cardinal s);
            ISet.iter
              (fun str ->
                Printf.fprintf oc "%s" (Format.asprintf "@[<v 4>%s@]" str))
              s;
            Printf.fprintf oc "\n")
          !parameter_declarations)

(*-------------------- Parameter instantiations which need manual updating --------------------*)

let parameter_instantiations = ref (IMap.empty : (int * ISet.t) IMap.t)

let add_parameter_instantiation name func =
  let updater = function
    | None -> Some (1, ISet.singleton (func_sig_to_string func))
    | Some (i, s) -> Some (i + 1, ISet.add (func_sig_to_string func) s)
  in
  parameter_instantiations := IMap.update name updater !parameter_instantiations

let can_infer_parameters func_sig =
  match (func_sig.parameters, func_sig.args) with
  | [ (n, _) ], [ (_, { desc = T_Bits ({ desc = E_Var n' }, _) }) ] ->
      (Builder.is_stdlib_name func_sig.name || func_sig.body = SB_Primitive)
      && String.equal n n'
  | _ -> false

let log_parameter_instantiations ~env name func eqs =
  if
    !should_log
    && (not (list_is_empty eqs)) (* there are instantiations *)
    && not (can_infer_parameters func)
    (* which are not inferred *)
  then
    let env = { env with local = empty_local } in
    let return_params =
      match func.return_type with
      | None -> ISet.empty
      | Some ty ->
          parameters_of_ty ~env ~consider_int:false ty
          |> ISet.of_list
          |> ISet.filter (fun name -> List.mem_assoc name func.parameters)
    in
    if ISet.is_empty return_params (* and are not return parameters *) then
      let parameters =
        List.fold_left
          (fun acc (x, _) -> if List.mem x acc then acc else x :: acc)
          [] eqs
      in
      let new_parameters =
        (* and are not already in argument list *)
        List.filter (fun x -> not (List.mem_assoc x func.args)) parameters
      in
      if not (list_is_empty new_parameters) then
        add_parameter_instantiation name func

let print_parameter_instantiations ?(verbose = false) () =
  if !should_log && not (IMap.is_empty !parameter_instantiations) then
    let sorted =
      !parameter_instantiations |> IMap.bindings
      |> List.sort (fun (_, (n1, _)) (_, (n2, _)) -> n2 - n1)
    in
    let total = List.fold_left (fun acc (_, (i, _)) -> acc + i) 0 sorted in
    with_file "instantiations.txt" (fun oc ->
        Printf.fprintf oc "Total call sites: %i\n\n" total;
        List.iter
          (fun (n, (i, s)) ->
            Printf.fprintf oc "%6i %s\n" i n;
            if verbose then (
              ISet.iter (Printf.fprintf oc "         %s\n") s;
              Printf.fprintf oc "\n"))
          sorted)
