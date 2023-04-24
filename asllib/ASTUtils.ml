open AST
module ISet = Set.Make (String)

module IMap : sig
  include Map.S with type key = identifier

  val of_list : (key * 'a) list -> 'a t
end = struct
  include Map.Make (String)

  let of_list li =
    List.fold_left (fun acc (key, value) -> add key value acc) empty li
end

let dummy_pos = Lexing.dummy_pos
let annotated desc pos_start pos_end = { desc; pos_start; pos_end }
let add_dummy_pos desc = annotated desc dummy_pos dummy_pos
let dummy_annotated = add_dummy_pos ()
let to_pos pos = { pos with desc = () }

let add_pos_from_st pos desc =
  if pos.desc == desc then pos else { pos with desc }

let with_pos_from_st pos { desc; _ } = add_pos_from_st pos desc
let map_desc_st f thing = f thing |> add_pos_from_st thing
let add_pos_from pos desc = { pos with desc }
let with_pos_from pos { desc; _ } = add_pos_from pos desc
let map_desc f thing = f thing |> add_pos_from thing

let map2_desc f thing1 thing2 =
  {
    desc = f thing1 thing2;
    pos_start = thing1.pos_start;
    pos_end = thing2.pos_end;
  }

let s_pass = add_dummy_pos S_Pass
let s_then = map2_desc (fun s1 s2 -> S_Then (s1, s2))

let stmt_from_list : stmt list -> stmt =
  let is_not_s_pass = function { desc = S_Pass; _ } -> false | _ -> true in
  let rec one_step acc = function
    | [] -> List.rev acc
    | [ x ] -> List.rev (x :: acc)
    | s1 :: s2 :: t -> one_step (s_then s1 s2 :: acc) t
  in
  let rec aux = function
    | [] -> add_dummy_pos S_Pass
    | [ x ] -> x
    | l -> aux @@ one_step [] l
  in
  fun l -> List.filter is_not_s_pass l |> aux

let mask_from_set_bits_positions size pos =
  let buf = Bytes.make size '0' in
  let set i = Bytes.set buf i '1' in
  let () = List.iter set pos in
  Bytes.to_string buf

let inv_mask =
  let one_char = function '0' -> '1' | '1' -> '0' | c -> c in
  String.map one_char

let slices_to_positions as_int =
  let one_slice (start, length) =
    let start = as_int start and length = as_int length in
    (* Reversed interval *)
    List.init length (( - ) (start + length - 1))
  in
  fun positions -> List.map one_slice positions |> List.flatten

let used_identifiers, used_identifiers_stmt =
  let rec use_e acc e =
    match e.desc with
    | E_Literal _ -> acc
    | E_Typed (e, _) -> use_e acc e
    | E_Var x -> ISet.add x acc
    | E_Binop (_op, e1, e2) -> use_e (use_e acc e2) e1
    | E_Unop (_op, e) -> use_e acc e
    | E_Call (x, args, named_args) ->
        let acc = ISet.add x acc in
        let acc = List.fold_left use_field acc named_args in
        List.fold_left use_e acc args
    | E_Slice (e, args) ->
        let acc = use_e acc e in
        List.fold_left use_slice acc args
    | E_Cond (e1, e2, e3) -> use_e (use_e (use_e acc e1) e3) e2
    | E_GetField (e, _, _ta) -> use_e acc e
    | E_GetFields (e, _, _ta) -> use_e acc e
    | E_Record (_ty, li, _ta) -> List.fold_left use_field acc li
    | E_Concat es -> List.fold_left use_e acc es
    | E_Tuple es -> List.fold_left use_e acc es
    | E_Unknown _ -> acc
    | E_Pattern (e, _p) -> use_e acc e
  and use_field acc (_, e) = use_e acc e
  and use_slice acc = function
    | Slice_Single e -> use_e acc e
    | Slice_Length (e1, e2) | Slice_Range (e1, e2) -> use_e (use_e acc e1) e2
  and use_s acc s =
    match s.desc with
    | S_Pass | S_Return None -> acc
    | S_Then (s1, s2) -> use_s (use_s acc s1) s2
    | S_Assert e | S_Return (Some e) -> use_e acc e
    | S_Assign (le, e) -> use_le (use_e acc e) le
    | S_Call (x, args, named_args) ->
        let acc = ISet.add x acc in
        let acc = List.fold_left use_field acc named_args in
        List.fold_left use_e acc args
    | S_Cond (e, s1, s2) -> use_s (use_s (use_e acc e) s2) s1
    | S_Case (e, cases) -> List.fold_left use_case (use_e acc e) cases
    | S_TypeDecl _ -> acc
  and use_case acc { desc = _p, stmt; _ } = use_s acc stmt
  and use_le acc _le = acc
  and use_decl acc = function
    | D_Func { body; _ } -> use_s acc body
    | D_GlobalConst (_name, _ty, e) -> use_e acc e
    | _ -> acc
  in
  (List.fold_left use_decl ISet.empty, use_s ISet.empty)

let canonical_fields li =
  let compare (x, _) (y, _) = String.compare x y in
  List.sort compare li

let literal v = E_Literal v |> add_dummy_pos
let var_ x = E_Var x |> add_dummy_pos
let binop op = map2_desc (fun e1 e2 -> E_Binop (op, e1, e2))

let expr_of_lexpr : lexpr -> expr =
  let rec aux le =
    match le.desc with
    | LE_Var x -> E_Var x
    | LE_Typed (le, t) -> E_Typed (map_desc aux le, t)
    | LE_Slice (le, args) -> E_Slice (map_desc aux le, args)
    | LE_SetField (le, x, ta) -> E_GetField (map_desc aux le, x, ta)
    | LE_SetFields (le, x, ta) -> E_GetFields (map_desc aux le, x, ta)
    | LE_Ignore -> E_Var "-"
    | LE_TupleUnpack les -> E_Tuple (List.map (map_desc aux) les)
  in
  map_desc aux

let fresh_var =
  let i = ref 0 in
  fun s ->
    let () = incr i in
    s ^ "-" ^ string_of_int !i

let rec big_union = function
  | [] -> literal (V_Bool true)
  | [ e ] -> e
  | h :: t -> binop BOR h (big_union t)

let case_to_conds : stmt -> stmt =
  let rec cases_to_cond x = function
    | [] -> s_pass
    | case :: t -> map_desc (one_case x t) case
  and one_case x t case =
    let p, s = case.desc in
    S_Cond (E_Pattern (var_ x, p) |> add_pos_from case, s, cases_to_cond x t)
  in
  map_desc @@ fun s ->
  match s.desc with
  | S_Case ({ desc = E_Var y; _ }, cases) -> (cases_to_cond y cases).desc
  | S_Case (e, cases) ->
      let x = fresh_var "case" in
      let assign =
        let pos = e.pos_start in
        let le = annotated (LE_Var x) pos pos in
        annotated (S_Assign (le, e)) pos e.pos_end
      in
      S_Then (assign, cases_to_cond x cases)
  | _ -> raise (Invalid_argument "case_to_conds")

let slice_as_single = function
  | Slice_Single e -> e
  | _ -> raise @@ Invalid_argument "slice_as_single"

let getter_prefix = "getter-"
let setter_prefix = "setter-"
let setter_name = ( ^ ) setter_prefix
let getter_name = ( ^ ) getter_prefix

let num_args = function
  | 0 -> Fun.id
  | n -> fun name -> name ^ "-" ^ string_of_int n

let default_t_bits = T_Bits (BitWidth_Constrained [], None)

let patch ~src ~patches =
  (* Size considerations:
     - [src] is BIG.
     - [patches] is not that little. *)
  let identifier_of_decl = function
    | D_Func { name; _ }
    | D_GlobalConst (name, _, _)
    | D_TypeDecl (name, _)
    | D_Primitive { name; _ } ->
        name
  in
  let to_remove =
    patches |> List.to_seq |> Seq.map identifier_of_decl |> ISet.of_seq
  in
  let filter d = not (ISet.mem (identifier_of_decl d) to_remove) in
  src |> List.filter filter |> List.rev_append patches
