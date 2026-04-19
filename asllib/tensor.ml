(* type 'a t = { data : 'a array; dims : int array; strides : int array } *)
(** A tensor represented via a flat array with associated dimensions and
    strides. *** IMPORTANT: *** users must manually call [copy] when sharing a
    tensor to ensure that modifications to one tensor do not affect the other.
*)

module IntMap = Map.Make (Int)

type 'a t = { data : 'a IntMap.t; dims : int array; strides : int array }

let compute_strides dims =
  let n = Array.length dims in
  let strides = Array.make n 1 in
  for i = n - 2 downto 0 do
    strides.(i) <- strides.(i + 1) * dims.(i + 1)
  done;
  strides

(** [linear_index t coord] returns the linear index corresponding to the
    multi-dimensional coordinates [coord] in the tensor [t]. *)
let linear_index t coord =
  let n = Array.length t.dims in
  if Array.length coord <> n then invalid_arg "rank mismatch";
  let idx = ref 0 in
  for i = 0 to n - 1 do
    let c = coord.(i) in
    let d = t.dims.(i) in
    if c < 0 || c >= d then invalid_arg "index out of bounds";
    idx := !idx + (c * t.strides.(i))
  done;
  !idx

let dims t = Array.to_list t.dims

let create dims value =
  let total_size = List.fold_left ( * ) 1 dims in
  let dims_array = Array.of_list dims in
  (* Unfortunately, Seq.init is not available in OCaml 4.08. *)
  let seq_init n f =
    let rec seq_init_rec i () =
      if i >= n then Seq.Nil else Seq.Cons (f i, seq_init_rec (i + 1))
    in
    seq_init_rec 0
  in
  let bindings = seq_init total_size (fun i -> (i, value)) in
  {
    data = IntMap.of_seq bindings;
    dims = dims_array;
    strides = compute_strides dims_array;
  }

let get t coordinates =
  let coord_array = Array.of_list coordinates in
  let index = linear_index t coord_array in
  IntMap.find index t.data

let set t coordinates value =
  let coord_array = Array.of_list coordinates in
  let index = linear_index t coord_array in
  { t with data = IntMap.add index value t.data }

(* let create dims value =
  let dims_array = Array.of_list dims in
  {
    data = Array.make (Array.fold_left ( * ) 1 dims_array) value;
    dims = dims_array;
    strides = compute_strides dims_array;
  }

let copy t = { t with data = Array.copy t.data }

let get t coordinates =
  let coord_array = Array.of_list coordinates in
  let index = linear_index t coord_array in
  t.data.(index)

let set t coordinates value =
  let coord_array = Array.of_list coordinates in
  let index = linear_index t coord_array in
  t.data.(index) <- value *)
