(** A module defining utility functions for layouts. *)

open AST

let unspecified_for_elements elements = List.map (fun _ -> Unspecified) elements

let horizontal_if_unspecified layout elements =
  match layout with
  | Unspecified -> Horizontal (unspecified_for_elements elements)
  | Horizontal _ | Vertical _ -> layout

let vertical_if_unspecified layout elements =
  match layout with
  | Unspecified -> Vertical (unspecified_for_elements elements)
  | Horizontal _ | Vertical _ -> layout

let rec contains_vertical = function
  | Unspecified -> false
  | Horizontal layout_list -> List.exists contains_vertical layout_list
  | Vertical _ -> true

(** [apply_layout_to_list layout elements] distributes the layouts listed in
    [layout] to the list of elements in [elements]. If [layout] is
    [Unspecified], then all elements in [elements] are assigned the
    [Unspecified] layout. For example:
    {[
      apply_layout_to_list
        (Horizontal [ Unspecified; Vertical [ Unspecified; Unspecified ] ])
        [ a; b ]
      = [ (a, Unspecified); (b, Vertical [ Unspecified; Unspecified ]) ]
    ]} *)
let apply_layout_to_list layout elements =
  let args_layout =
    match layout with
    | Horizontal l | Vertical l -> l
    | Unspecified -> unspecified_for_elements elements
  in
  assert (List.compare_lengths elements args_layout = 0);
  List.combine elements args_layout
