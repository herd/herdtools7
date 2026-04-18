type 'prim t =
  | One of 'prim
  | Opt of 'prim t
  | Seq of 'prim t list
  | Choice of 'prim t list

let rec normalise ast =
  let normalise_seq_items items =
    List.fold_right
      (fun item acc ->
        match normalise item with
        | Seq nested -> nested @ acc
        | item -> item :: acc)
      items [] in
  let normalise_choice_items items =
    List.fold_right
      (fun item acc ->
        match normalise item with
        | Choice nested -> nested @ acc
        | item -> item :: acc)
      items [] in
  match ast with
  | One _ -> ast
  | Opt opt -> Opt (normalise opt)
  | Seq ss -> Seq (normalise_seq_items ss)
  | Choice ss -> Choice (normalise_choice_items ss)

let rec bind ast func =
  let bind_func ast = bind ast func in
  match ast with
  | One s -> func s
  | Opt opt -> Opt (bind_func opt)
  | Seq ss -> Seq (List.map bind_func ss)
  | Choice ss -> Choice (List.map bind_func ss)

let rec pp pp_prim ast =
  let pp_with_prim = pp pp_prim in
  match ast with
  | One s -> Printf.sprintf "%s" (pp_prim s)
  | Opt opt -> Printf.sprintf "%s?" (pp_with_prim opt)
  | Seq ss ->
      Printf.sprintf "[%s]" (String.concat "," (List.map pp_with_prim ss))
  | Choice ss ->
      Printf.sprintf "[%s]" (String.concat "|" (List.map pp_with_prim ss))

let list_cross_product_map f lhs rhs =
  List.map ( fun l ->
    List.map ( fun r ->
      f l r
    ) rhs
  ) lhs
  |> List.flatten

let rec expand t =
  let result = match t with
  | One str -> [[str]]
  | Opt opt -> [] :: expand opt
  | Seq seq -> List.fold_left
    ( fun acc s ->
      expand s
      |> list_cross_product_map ( @ ) acc
    ) [[]] seq
  | Choice choice -> List.map expand choice |> List.flatten in
  result
