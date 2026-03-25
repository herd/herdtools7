type ('pred,'prim) t =
  | One of 'prim
  | Opt of ('pred,'prim) t
  | Multi of ('pred,'prim) t
  | Seq of ('pred,'prim) t list
  | Choice of ('pred,'prim) t list
  | Predicate of 'pred * ('pred,'prim) t

let rec bind func ast =
  let bind_func = bind func in
  let bind_pred pred = bind ( fun e -> Predicate(pred, func e) ) in
  match ast with
  | One s -> func s
  | Opt opt -> Opt (bind_func opt)
  | Seq ss -> Seq (List.map bind_func ss)
  | Choice ss -> Choice (List.map bind_func ss)
  | Multi ss -> Multi (bind_func ss)
  | Predicate (pred,t) -> bind_pred pred t

let rec pp pp_pred pp_prim ast =
  let pp_wrap = pp pp_pred pp_prim in
  match ast with
  | One s -> Printf.sprintf "%s" (pp_prim s)
  | Opt opt -> Printf.sprintf "(%s)?" (pp_wrap opt)
  | Seq ss ->
      Printf.sprintf "(%s)" (String.concat "," (List.map pp_wrap ss))
  | Choice ss ->
      Printf.sprintf "(%s)" (String.concat "|" (List.map pp_wrap ss))
  | Multi ss ->
      Printf.sprintf "[%s]" (pp_wrap ss)
  | Predicate (pred,t) ->
      Printf.sprintf "%s(%s)" (pp_pred pred) (pp_wrap t)

let list_cross_product_map f lhs rhs =
  List.map ( fun l ->
    List.map ( fun r ->
      f l r
    ) rhs
  ) lhs
  |> List.flatten

let rec to_list = function
  | One _ as one -> [ one ]
  | Opt t | Predicate (_,t) -> to_list t
  | Seq l | Choice l -> l
  | Multi multi -> [ multi ]

let rec flatten t =
  let result = match t with
  | One str -> [[str]]
  | Opt opt -> [] :: flatten opt
  | Multi multi -> flatten multi
  | Seq seq -> List.fold_left
    ( fun acc s ->
      flatten s
      |> list_cross_product_map ( @ ) acc
    ) [[]] seq
  | Choice choice -> List.map flatten choice |> List.flatten
  | Predicate _ -> Warn.fatal "Predicate must be resolved before flatten" in
  result

