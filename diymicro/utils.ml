(** Print something if verbose level is high enough (0:Verbose, 1:Debug) *)
let verbose_print level string =
  if level <= !Config.verbose then output_string stderr string

(** Removes element from the option monad, raises Not_found if appropriate *)
let unsome = function None -> raise Not_found | Some v -> v

let rec list_last = function
  | _ :: x :: q -> list_last (x :: q)
  | [x] -> x
  | _ -> Warn.fatal "Can't get last element of empty list"

(** Rotates list by one step: `l@[x]` becomes `x::l` *)
let list_rot1 l =
  let rec rotate1 acc = function
    | [] -> []
    | [e] -> e :: List.rev acc
    | e :: l -> rotate1 (e :: acc) l
  in
  rotate1 [] l

(** Cartesian product of 2 lists *)
let cartesian2 l1 l2 = List.concat_map (fun x -> List.map (fun y -> x, y) l2) l1

(** Cartesian product of 3 lists*)
let cartesian3 l1 l2 l3 =
  List.concat_map
    (fun x -> List.concat_map (fun y -> List.map (fun z -> x, y, z) l3) l2)
    l1
