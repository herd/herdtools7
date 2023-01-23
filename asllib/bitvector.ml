(* BitVector type *)

(*
   Notations:

      - A type t is named (length, data) or bv
      - b is a bit
      - n = length / 8 : ~the length of data~
      - m = length mod 8 : the number of bits of the trailing character.

 *)

(* --------------------------------------------------------------------------

                              Main type definition

   --------------------------------------------------------------------------*)

type t = int * string
(** A bitvector is given by its length in bits and its containing data.

    Note that the stored data is a string, but only for performance and
    immutability of values reasons. This should not be printed as is, and
    users should use the dedicated printing primitives.

    Invariant:
    For a value (length, data), we have
      String.length data = length / 8 + 1

*)

(* Accessors. *)
let length = fst
let data = snd

(* --------------------------------------------------------------------------

                                    Helpers

   --------------------------------------------------------------------------*)

(* Constant. *)
let code_0 = Char.code '0'
let char_0 = Char.chr 0

(* Mask for the last character, given by [length mod 8]. *)
let last_char_mask m = (1 lsl m) - 1

(* [read_bit_raw i c] reads bit at index [i] in character [c]. *)
let read_bit_raw i c = (Char.code c lsr i) land 1

(* [read_bit i c] is the printable character representing bit [i] in
   character [c]. *)
let read_bit i c = Char.chr (read_bit_raw i c + code_0)

(* [set_bit_raw i dst b] sets the [i]-th bit of [dst] to [b]. *)
let set_bit_raw i dst b = dst land lnot (1 lsl i) lor (b lsl i)

(* [set_bit src_pos dst_pos src dst] sets the [dst_pos]-th bit of [pos] to the [src_pos]-th bit of [src]. *)
let set_bit src_pos dst_pos src dst =
  read_bit_raw src_pos src |> set_bit_raw dst_pos dst |> Char.chr

(* Debug printer. *)
let _pp_data f (length, data) =
  let open Format in
  pp_print_int f length;
  pp_print_char f 'x';
  String.iter (fun c -> fprintf f "%x" @@ Char.code c) data

let create_data_bytes length =
  let n = length / 8 and m = length mod 8 in
  Bytes.create (if m = 0 then n else n + 1)

(* --------------------------------------------------------------------------

                              Printers and conversions

   --------------------------------------------------------------------------*)

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
  let result = Bytes.make (if m = 0 then n else n + 1) char_0 in
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
  let result = create_data_bytes 64 in
  for i = 0 to 7 do
    Int64.shift_right_logical s (8 * i)
    |> Int64.logand 255L |> Int64.to_int |> Char.chr |> Bytes.set result i
  done;
  (64, Bytes.unsafe_to_string result)

(* --------------------------------------------------------------------------

                                    Operations

   --------------------------------------------------------------------------*)

(** [remask bv] ensures that the extra bits on the trailing character are '0'.
    It edits in place the string, so to use with prudence. *)
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

(* [bitwise_op Int.logand bv1 bv2] computes the bitwise and on bv1 and bv2. *)
let bitwise_op int_op (length1, data1) (length2, data2) =
  let length = ensure_equal_length length1 length2 in
  let n = length / 8 and m = length mod 8 in
  let result = create_data_bytes length in
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

let lognot (length, data) =
  let bnot c = c |> Char.code |> lnot |> ( land ) 0xff |> Char.chr in
  let ndata = String.map bnot data in
  remask (length, ndata)

let logand = bitwise_op ( land )
let logor = bitwise_op ( lor )
let logxor bv1 bv2 = bitwise_op ( lxor ) bv1 bv2 |> remask

let equal bv1 bv2 =
  length bv1 == length bv2
  && (* let bv1 = remask bv1 and bv2 = remask bv2 in *)
  String.equal (data bv1) (data bv2)

let compare bv1 bv2 =
  match Int.compare (length bv1) (length bv2) with
  | 0 ->
      (* let bv1 = remask bv1 and bv2 = remask bv2 in *)
      String.compare (data bv1) (data bv2)
  | i -> i

(* --------------------------------------------------------------------------

                                    Slices

   --------------------------------------------------------------------------*)

(* Write into dst at position pos_dst the bit of src at position pos_src. *)
let copy_bit dst src pos_src pos_dst =
  let c_src = String.get src (pos_src / 8)
  and c_dst = Bytes.get dst (pos_dst / 8) |> Char.code in
  let new_char_dst = set_bit (pos_src mod 8) (pos_dst mod 8) c_src c_dst in
  Bytes.set dst (pos_dst / 8) new_char_dst

let extract_slice (_length_src, data_src) positions =
  let length = List.length positions in
  let result = create_data_bytes length in
  (* Same effect than [List.rev positions], as we build those from the end. *)
  let copy_bit_here i pos = copy_bit result data_src pos (length - 1 - i) in
  let () = List.iteri copy_bit_here positions in
  remask (length, Bytes.unsafe_to_string result)

let write_slice (length_dst, data_dst) (length_src, data_src) positions =
  let () =
    if List.length positions != length_src then
      raise (Invalid_argument "Bitvector.write_slice")
  in
  let result = Bytes.of_string data_dst in
  (* Same effect than [List.rev positions], as we build those from the end. *)
  let copy_bit_here i pos = copy_bit result data_src (length_src - 1 - i) pos in
  let () = List.iteri copy_bit_here positions in
  remask (length_dst, Bytes.unsafe_to_string result)

let read_char_offset offset =
  let next_mask = last_char_mask offset in
  let offset' = 8 - offset in
  fun prec_c next_c ->
    let prec_bits = (Char.code prec_c lsr offset') land 0xff
    and next_bits = (Char.code next_c land next_mask) lsl offset in
    next_bits lor prec_bits |> Char.chr

let copy_into dst (length_src, data_src) offset =
  let length_dst = offset + length_src in
  let n_src = length_src / 8 and m_src = length_src mod 8 in
  let n_dst = length_dst / 8 and m_dst = length_dst mod 8 in
  let n_off = offset / 8 and m_off = offset mod 8 in
  let () =
    if m_off = 0 then
      Bytes.blit_string data_src 0 dst n_off (String.length data_src)
    else
      (* We have an offset of m_off on every char.

         First handle the last written char, by hand because no offset has to
         be applied to the read character. *)
      let prec_c = Bytes.get dst n_off and next_c = String.get data_src 0 in
      let prec_bits = Char.code prec_c land last_char_mask m_off
      and next_bits = (Char.code next_c lsl m_off) land 0xff in
      next_bits lor prec_bits |> Char.chr |> Bytes.set dst n_off;

      (* Next body *)
      for i = n_off + 1 to n_dst - 1 do
        (* 0 already handled, n2 - 1 handled after *)
        let i_src = i - n_off in
        let prec_c = String.get data_src i_src in
        let next_c = String.get data_src (i_src + 1) in
        Bytes.set dst i @@ read_char_offset m_off prec_c next_c
      done;

      (* Last written char *)
      if m_dst != 0 && n_dst > n_off then
        if m_dst > m_src then
          let prec_c = String.get data_src (n_src - 1)
          and next_c =
            if m_src != 0 then String.get data_src n_src else char_0
          in
          Bytes.set dst n_dst @@ read_char_offset m_off prec_c next_c
        else
          let prec_c = String.get data_src n_src and next_c = char_0 in
          Bytes.set dst n_dst @@ read_char_offset m_off prec_c next_c
  in
  length_dst

let concat bvs =
  let length = List.fold_left (fun acc bv -> acc + length bv) 0 bvs in
  let result = create_data_bytes length in
  let _ = List.fold_right (copy_into result) bvs 0 in
  (length, Bytes.unsafe_to_string result)
