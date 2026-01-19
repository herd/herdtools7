type t =
  | One of string
  | Multi of t
  | Seq of t list
  | Choice of t list

let rec pp = function
| One s -> Printf.sprintf "%s" s
| Seq ss ->
    Printf.sprintf "(%s)" (String.concat "," (List.map pp ss))
| Choice ss ->
    Printf.sprintf "(%s)" (String.concat "|" (List.map pp ss))
| Multi ss ->
    Printf.sprintf "[%s]" (pp ss)

let list_cross_product_map f lhs rhs =
  List.map ( fun l ->
    List.map ( fun r ->
      f l r
    ) rhs
  ) lhs
  |> List.flatten

let to_list = function
  | One _ as one -> [ one ]
  | Seq l | Choice l -> l
  | Multi multi -> [ multi ]

let rec flatten t =
  let result = match t with
  | One str -> [[str]]
  | Multi multi -> flatten multi
  | Seq seq -> List.fold_left
    ( fun acc s ->
      flatten s
      |> list_cross_product_map ( @ ) acc
    ) [[]] seq
  | Choice choice -> List.map flatten choice |> List.flatten in
  result

