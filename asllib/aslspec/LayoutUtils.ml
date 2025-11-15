(** A module defining utility functions for layouts. *)

open AST

let horizontal_if_unspecified layout list =
  match layout with
  | Horizontal _ | Vertical _ -> layout
  | _ -> Horizontal (List.map (fun _ -> Unspecified) list)

let vertical_if_unspecified layout list =
  match layout with
  | Horizontal _ | Vertical _ -> layout
  | _ -> Vertical (List.map (fun _ -> Unspecified) list)

let rec contains_vertical = function
  | Unspecified -> false
  | Horizontal layout_list -> List.exists contains_vertical layout_list
  | Vertical _ -> true

(** [apply_layout_to_list layout list] distributes the layouts listed in
    [layout] to the list of elements in [list]. If [layout] is [Unspecified],
    then all elements in [list] are assigned the [Unspecified] layout. For
    example:
    {[
      apply_layout_to_list
        (Horizontal [ Unspecified; Vertical [ Unspecified; Unspecified ] ])
        [ a; b ]
      = [ (a, Unspecified); (b, Vertical [ Unspecified; Unspecified ]) ]
    ]} *)
let apply_layout_to_list layout list =
  let args_layout =
    match layout with
    | Horizontal l | Vertical l -> l
    | Unspecified -> List.map (fun _ -> Unspecified) list
  in
  assert (List.compare_lengths list args_layout = 0);
  List.combine list args_layout
