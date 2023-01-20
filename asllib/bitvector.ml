(** BitVector type *)

(*
   Notations:

      - A type t is named (length, data) or bv
      - b is a bit
      - n = length / 8 : ~the length of data~
      - m = length mod 8 : the number of bits of the trailing character.

 *)

type t = int * string
(** A bitvector is given by its length in bits and its containing data.

    Note that the stored data is a string, but only for performance and
    immutability of values reasons. This should not be printed as is, and
    users should use the dedicated printing primitives.

    Invariant:
    For a value (length, data), we have
      String.length data = length / 8 + 1

*)

let length = fst
let data = snd
let code_0 = Char.code '0'
let last_char_mask m = (1 lsl m) - 1
let read_bit_raw i c = (Char.code c lsr i) land 1
let read_bit i c = Char.chr (read_bit_raw i c + code_0)
let set_bit_raw i dst b = dst land lnot (1 lsl i) lor (b lsl i)

let set_bit src_pos dst_pos src dst =
  read_bit_raw src_pos src |> set_bit_raw dst_pos dst |> Char.chr

let pp_data f data =
  let open Format in
  pp_print_string f "0x";
  String.iter (fun c -> fprintf f "%x" @@ Char.code c) data

let pp_t =
  let open Format in
  fun f (length, data) ->
    pp_print_char f '\'';
    pp_open_hbox f ();
    let n = length / 8 and m = length mod 8 in
    if m <> 0 then
      let c = String.get data n in
      for j = m - 1 downto 0 do
        read_bit j c |> Format.pp_print_char f
      done
    else ();
    for i = n - 1 downto 0 do
      let c = String.get data i in
      for j = 7 downto 0 do
        read_bit j c |> Format.pp_print_char f
      done
    done;
    pp_close_box f ();
    pp_print_char f '\''

let to_string (length, data) =
  let result = Buffer.create (length + 2) in
  let n = length / 8 and m = length mod 8 in
  Buffer.add_char result '\'';
  if m <> 0 then
    let c = String.get data n in
    for j = m - 1 downto 0 do
      read_bit j c |> Buffer.add_char result
    done
  else ();
  for i = n - 1 downto 0 do
    let c = String.get data i in
    for j = 7 downto 0 do
      read_bit j c |> Buffer.add_char result
    done
  done;
  Buffer.add_char result '\'';
  Buffer.contents result

let to_int (length, data) =
  let result = ref 0 in
  let n = length / 8 and m = length mod 8 in
  for i = 0 to n - 1 do
    let c = String.get data i |> Char.code in
    result := !result lor (c lsl (i * 8))
  done;
  if m != 0 then
    let c = String.get data n |> Char.code |> ( land ) (last_char_mask m) in
    result := !result lor (c lsl (n * 8))
  else ();
  !result

let to_int64 (length, data) =
  let result = ref Int64.zero in
  let n = length / 8 and m = length mod 8 in
  for i = 0 to n - 1 do
    let c = String.get data i |> Char.code in
    result := Int64.logor !result (c lsl (i * 8) |> Int64.of_int)
  done;
  if m != 0 then
    let c = String.get data n |> Char.code |> ( land ) (last_char_mask m) in
    result := Int64.logor !result (c lsl (n * 8) |> Int64.of_int)
  else ();
  !result

let of_string s =
  let result = Buffer.create ((String.length s / 8) + 1) in
  let lengthr = ref 0 in
  let last_char = ref 0 in
  for i = String.length s - 1 downto 0 do
    match String.get s i with
    | ('0' | '1') as b ->
        let length = !lengthr in
        let () = lengthr := length + 1 in
        let m = length mod 8 in
        let c = !last_char in
        let c = if b = '1' then c lor (1 lsl m) else c in
        if m = 7 then (
          Buffer.add_char result (Char.chr c);
          last_char := 0)
        else last_char := c
    | _ -> ()
  done;
  let length = !lengthr in
  let m = length mod 8 in
  if m <> 0 then
    let c = !last_char land last_char_mask m in
    Buffer.add_char result (Char.chr c)
  else ();
  (length, Buffer.contents result)

let of_int s =
  let length = Sys.int_size - 1 in
  let n = length / 8 and m = length mod 8 in
  let result = Bytes.create (if m = 0 then n else n + 1) in
  for i = 0 to n - 1 do
    let c = (s lsr (8 * i)) land 255 in
    Bytes.set result i (Char.chr c)
  done;
  if m <> 0 then
    let c = (s lsr (8 * n)) land 255 in
    Bytes.set result n (Char.chr c)
  else ();
  (length, Bytes.unsafe_to_string result)

let of_int64 s =
  let result = Bytes.create 8 in
  for i = 0 to 7 do
    Int64.shift_right_logical s (8 * i)
    |> Int64.logand 255L |> Int64.to_int |> Char.chr |> Bytes.set result i
  done;
  (64, Bytes.unsafe_to_string result)

let remask (length, data) =
  let n = length / 8 and m = length mod 8 in
  if m == 0 then (length, data)
  else
    let trailing_d = String.get data n |> Char.code in
    let masked_d = trailing_d land last_char_mask m in
    let new_c = Char.chr masked_d in
    let buf = Bytes.unsafe_of_string data in
    let () = Bytes.set buf n new_c in
    let data = Bytes.unsafe_to_string buf in
    (length, data)

let ensure_equal_length length1 length2 =
  if length1 = length2 then length1 else raise (Invalid_argument "bitwise_op")

let bitwise_op int_op (length1, data1) (length2, data2) =
  let length = ensure_equal_length length1 length2 in
  let n = length / 8 and m = length mod 8 in
  let result = Bytes.create length in
  for i = 0 to n - 1 do
    let d1 = String.get data1 i |> Char.code
    and d2 = String.get data2 i |> Char.code in
    let c = int_op d1 d2 |> Char.chr in
    Bytes.set result i c
  done;
  if m <> 0 then
    let mask = last_char_mask m in
    let d1 = String.get data1 n |> Char.code |> ( land ) mask
    and d2 = String.get data2 n |> Char.code |> ( land ) mask in
    let c = int_op d1 d2 |> ( land ) mask |> Char.chr in
    Bytes.set result n c
  else ();
  (length, Bytes.unsafe_to_string result)

let folded_char_op folder default (length1, data1) (length2, data2) =
  let length = ensure_equal_length length1 length2 in
  let n = length / 8 and m = length mod 8 in
  let state = ref default in
  for i = 0 to n - 1 do
    let d1 = String.get data1 i |> Char.code
    and d2 = String.get data2 i |> Char.code in
    state := folder !state d1 d2
  done;
  if m <> 0 then
    let mask = last_char_mask m in
    let d1 = String.get data1 n |> Char.code |> ( land ) mask
    and d2 = String.get data2 n |> Char.code |> ( land ) mask in
    state := folder !state d1 d2
  else ();
  !state

let lognot (length, data) =
  let bnot c = c |> Char.code |> lnot |> ( land ) 0xff |> Char.chr in
  let ndata = String.map bnot data in
  remask (length, ndata)

let logand = bitwise_op ( land )
let logor = bitwise_op ( lor )
let logxor bv1 bv2 = bitwise_op ( lxor ) bv1 bv2 |> remask

let equal =
  let folder prev d1 d2 = prev && d1 = d2 in
  fun (length1, data1) (length2, data2) ->
    length1 = length2
    && folded_char_op folder true (length1, data1) (length2, data2)

let compare bv1 bv2 =
  match Int.compare (length bv1) (length bv2) with
  | 0 ->
      let bv1 = remask bv1 and bv2 = remask bv2 in
      String.compare (data bv1) (data bv2)
  | i -> i

let extract_slice (_length, data) positions =
  let buf = Buffer.create (List.length positions) in
  let read_and_add pos =
    String.get data (pos / 8) |> read_bit (pos mod 8) |> Buffer.add_char buf
  in
  let () = List.iter read_and_add positions in
  let printed = Buffer.contents buf in
  of_string printed

let write_slice (length_dst, data_dst) (_length_src, data_src) positions =
  let result = Bytes.of_string data_dst in
  let read_and_edit pos_src pos_dst =
    let c_src = String.get data_src (pos_src / 8)
    and c_dst = Bytes.get result (pos_dst / 8) |> Char.code in
    let new_char_dst = set_bit (pos_src mod 8) (pos_dst mod 8) c_src c_dst in
    Bytes.set result (pos_dst / 8) new_char_dst
  in
  let () = List.iteri read_and_edit positions in
  (length_dst, Bytes.unsafe_to_string result)
