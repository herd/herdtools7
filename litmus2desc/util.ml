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
