type ('pred,'prim) t =
  | One of 'prim
  | Opt of ('pred,'prim) t
  | Seq of ('pred,'prim) t list
  | Choice of ('pred,'prim) t list
  | Predicate of 'pred * ('pred,'prim) t

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
  | Predicate (pred,t) -> Predicate (pred, normalise t)

let rec map ~one ~predicate ast =
  let map_child ast = map ~one ~predicate ast in
  match ast with
  | One prim -> one prim
  | Opt opt -> Opt (map_child opt)
  | Seq seq -> Seq (List.map map_child seq)
  | Choice choice -> Choice (List.map map_child choice)
  | Predicate (pred,t) -> predicate pred t (fun () -> map_child t)

let bind ast func =
  map ~one:func
    ~predicate:(fun pred _ child -> Predicate (pred, child ()))
    ast

let rec pp pp_pred pp_prim ast =
  let ast = normalise ast in
  let pp_wrap = pp pp_pred pp_prim in
  match ast with
  | One s -> Printf.sprintf "%s" (pp_prim s)
  | Opt opt -> Printf.sprintf "%s?" (pp_wrap opt)
  | Seq ss ->
      Printf.sprintf "[%s]" (String.concat "," (List.map pp_wrap ss))
  | Choice ss ->
      Printf.sprintf "[%s]" (String.concat "|" (List.map pp_wrap ss))
  | Predicate (pred,t) ->
      Printf.sprintf "@%s(%s)" (pp_pred pred) (pp_wrap t)

let list_cross_product_map f lhs rhs =
  List.map ( fun l ->
    List.map ( fun r ->
      f l r
    ) rhs
  ) lhs
  |> List.flatten

let rec expand pred_func t =
  let expand_pred = expand pred_func in
  let result = match t with
  | One str -> [[str]]
  | Opt opt -> [] :: expand_pred opt
  | Seq seq -> List.fold_left
    ( fun acc s ->
      expand_pred s
      |> list_cross_product_map ( @ ) acc
    ) [[]] seq
  | Choice choice -> List.map expand_pred choice |> List.flatten
  | Predicate (pred,t) ->
      expand_pred t
      |> List.map (List.map (fun e -> pred_func pred e)) in
  result
