module Iter = struct
  type 'a t = ('a -> unit) -> unit

  let find_map f seq =
    let exception ExitFind in
    let r = ref None in
    (try
       seq (fun x ->
           match f x with
           | None -> ()
           | Some _ as res ->
               r := res;
               raise_notrace ExitFind)
     with ExitFind -> ());
    !r

  let of_in_channel_lines (ch : in_channel) : string t =
   fun f ->
    let rec go () =
      try
        f (Stdlib.input_line ch);
        go ()
      with End_of_file -> ()
    in
    go ()
end

let verbalize_index (ix : int) : string =
  match ix with
  | 0 -> "first"
  | 1 -> "second"
  | 2 -> "third"
  | 3 -> "fourth"
  | 4 -> "fifth"
  | ix -> Format.sprintf "%dth" (ix + 1)

let uniq ~(eq : 'a -> 'a -> bool) (l : 'a list) : 'a list =
  let rec uniq eq acc l =
    match l with
    | [] -> List.rev acc
    | x :: xs when List.exists (eq x) xs -> uniq eq acc xs
    | x :: xs -> uniq eq (x :: acc) xs
  in
  uniq eq [] l

let pp_list pp_item =
  let pp_sep fmt () = Format.fprintf fmt "; " in
  Format.pp_print_list ~pp_sep pp_item
