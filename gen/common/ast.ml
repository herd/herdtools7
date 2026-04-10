type 'prim t =
  | One of 'prim
  | Opt of 'prim t
  | Multi of 'prim t
  | Seq of 'prim t list
  | Choice of 'prim t list

let rec bind func ast =
  let bind_func = bind func in
  match ast with
  | One s -> func s
  | Opt opt -> Opt (bind_func opt)
  | Seq ss -> Seq (List.map bind_func ss)
  | Choice ss -> Choice (List.map bind_func ss)
  | Multi ss -> Multi (bind_func ss)

let rec pp pp_prim ast =
  let pp_with_prim = pp pp_prim in
  match ast with
  | One s -> Printf.sprintf "%s" (pp_prim s)
  | Opt opt -> Printf.sprintf "(%s)?" (pp_with_prim opt)
  | Seq ss ->
      Printf.sprintf "(%s)" (String.concat "," (List.map pp_with_prim ss))
  | Choice ss ->
      Printf.sprintf "(%s)" (String.concat "|" (List.map pp_with_prim ss))
  | Multi ss ->
      Printf.sprintf "[%s]" (pp_with_prim ss)

let list_cross_product_map f lhs rhs =
  List.map ( fun l ->
    List.map ( fun r ->
      f l r
    ) rhs
  ) lhs
  |> List.flatten

let rec node = function
  | One _ as one -> [ one ]
  | Opt t -> node t
  | Seq l | Choice l -> l
  | Multi multi -> [ multi ]

let seq_as_choice = function
  | Seq seq as ast ->
      if List.exists (function Choice _ -> true | _ -> false) seq
      then ast else Choice seq
  | ast -> ast

let rec to_list t =
  let result = match t with
  | One str -> [[str]]
  | Opt opt -> [] :: to_list opt
  | Multi multi -> to_list multi
  | Seq seq -> List.fold_left
    ( fun acc s ->
      to_list s
      |> list_cross_product_map ( @ ) acc
    ) [[]] seq
  | Choice choice -> List.map to_list choice |> List.flatten in
  result
