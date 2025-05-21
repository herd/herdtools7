open Edge

type loc = Loc of int
type proc = Proc of int


exception FailedEmit of Edge.edge


let new_loc =
  let counter = ref 0 in
  let inner_new_loc () =
    let loc = Loc !counter in
    let () = incr counter in loc
  in inner_new_loc

let pp_location (Loc i) =
  if i < 3 then String.make 1 ((Char.code 'x')+i |> Char.chr)
  else "loc_"^(string_of_int (i-3))

let pp_proc (Proc i) = "P"^string_of_int i




(** Assign a direction to each node *)
 let assign_directions edges =
  let unify_dir (d1, d2) =
    (* TODO: we want to know which edges are used to pp a good error *)
    if d1 <> d2 then failwith ("Can't unify edges direction "^Edge.pp_direction d1^" "^Edge.pp_direction d2)
    else d1
  in
  let rec unify_rec prev = function
    | e::q -> let cur, next = edge_direction e in
              unify_dir (cur, prev)::unify_rec next q
    | [] -> []

  in let loop_direction = unify_dir ((List.hd edges |> edge_direction |> fst), (Utils.list_last edges |> edge_direction |> snd))
  in unify_rec loop_direction edges


(** Create a value for each node depending on some attribute of the edge *)
let emit_node_values is_different_value new_value edges =
  let first_val = new_value () in

  (* Input:
    - previous value
    - remaining edges

    Output:
    - values list
    - Option val: to propagate `first_val` from the back until we meet a "diff" edge *)
  let rec unify prev = function
    [e] -> begin match is_different_value e with
                | false -> [first_val], Some first_val
                | true ->  if first_val=prev then raise (FailedEmit e)
                    else [first_val], None
              end
    | e::q -> begin match is_different_value e with
                | false -> begin match unify prev q with
                  | l, None -> prev::l, None
                  | l, Some next -> next::l, Some next
                end
                | true -> let cur = new_value ()
                  in begin match unify cur q with
                    | l, None -> cur::l, None
                    | l, Some next -> if next=prev then raise (FailedEmit e)
                        else next::l, None
                  end
              end
    | [] -> [], None
  in unify first_val edges |> fst |> Utils.list_rot1


(** Assign a location to each node *)
let emit_locations edges =
  try emit_node_values (fun e -> edge_location e = Different) new_loc edges
  with FailedEmit e -> failwith ("Cannot get a changing location across "^Edge.pp_edge e)

(** Assign a proc. to each node *)
let emit_procs edges =
  let proc_count = ref 0 in
  let new_proc () =
    let p = Proc !proc_count
    in let () = incr proc_count
    in p
  in let is_external = function
    | Rf ie | Fr ie | Ws ie -> ie=External
    | _ -> false
  in try emit_node_values is_external new_proc edges
  with FailedEmit e -> failwith ("Cannot get a changing proc across "^Edge.pp_edge e)