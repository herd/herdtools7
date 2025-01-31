(******************************************************************************)
(*                                ASLRef                                      *)
(******************************************************************************)
(*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 *)
(******************************************************************************)
(* Disclaimer:                                                                *)
(* This material covers both ASLv0 (viz, the existing ASL pseudocode language *)
(* which appears in the Arm Architecture Reference Manual) and ASLv1, a new,  *)
(* experimental, and as yet unreleased version of ASL.                        *)
(* This material is work in progress, more precisely at pre-Alpha quality as  *)
(* per Arm’s quality standards.                                               *)
(* In particular, this means that it would be premature to base any           *)
(* production tool development on this material.                              *)
(* However, any feedback, question, query and feature request would be most   *)
(* welcome; those can be sent to Arm’s Architecture Formal Team Lead          *)
(* Jade Alglave <jade.alglave@arm.com>, or by raising issues or PRs to the    *)
(* herdtools7 github repository.                                              *)
(******************************************************************************)

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
let char_ff = Char.chr 0xff

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

let create_data_bytes length = Bytes.create ((length + 7) / 8)

(** [String.for_all] taken directly out of stdlib version 4.13 . *)
let string_for_all p s =
  let n = String.length s in
  let rec loop i =
    if i = n then true
    else if p (String.unsafe_get s i) then loop (succ i)
    else false
  in
  loop 0

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

(* --------------------------------------------------------------------------

                              Signs and extensions

   --------------------------------------------------------------------------*)

let sign_bit (length, data) =
  match length with
  | 0 -> 0
  | _ ->
      let n = length / 8 and m = length mod 8 in
      let data_pos = if m = 0 then n - 1 else n in
      let bit_index = (m + 7) mod 8 in
      String.get data data_pos |> read_bit_raw bit_index

let extend signed nbytes (length, data) =
  let to_length = 8 * nbytes in
  let data_length = String.length data in
  if length > to_length then (to_length, String.sub data 0 nbytes)
  else if length = to_length then (length, data)
  else
    let result =
      Bytes.extend (Bytes.unsafe_of_string data) 0 (nbytes - data_length)
    in
    let neg = signed && sign_bit (length, data) = 1 in
    let () =
      if neg then
        let n = length / 8 and m = length mod 8 in
        let sign_bit_pos = if m = 0 then n - 1 else n in
        String.get data sign_bit_pos
        |> Char.code
        |> Int.logor [| 0; 0xff; 0xfe; 0xfc; 0xf8; 0xf0; 0xe0; 0xc0 |].(m)
        |> Char.chr
        |> Bytes.set result sign_bit_pos
    in
    Bytes.fill result data_length (nbytes - data_length)
      (if neg then char_ff else char_0);
    (to_length, Bytes.unsafe_to_string result)

let zero_extend = extend false
let sign_extend = extend true

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

let hex_number =
  Array.get
    [|
      '0';
      '1';
      '2';
      '3';
      '4';
      '5';
      '6';
      '7';
      '8';
      '9';
      'a';
      'b';
      'c';
      'd';
      'e';
      'f';
    |]

let buffer_add_hex result y = y mod 16 |> hex_number |> Buffer.add_char result

let write_char_hex result c =
  let x = Char.code c in
  buffer_add_hex result (x lsr 4);
  buffer_add_hex result x

let to_string_hexa (length, data) =
  let result = Buffer.create (2 * (length + 2)) in
  let n = length / 8 and m = length mod 8 in
  Buffer.add_string result "0x";
  (if m <> 0 then
     let c = String.get data n in
     if m > 4 then write_char_hex result c
     else c |> Char.code |> buffer_add_hex result);
  for i = n - 1 downto 0 do
    String.get data i |> write_char_hex result
  done;
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

let to_int64_raw (length, data) =
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

let to_int64_signed bv = bv |> sign_extend 8 |> to_int64_raw

let to_int64_unsigned (length, data) =
  let _, data = zero_extend 8 (length, data) in
  let _, data = remask (63, data) in
  to_int64_raw (64, data)

let to_z_unsigned (_, data) = Z.of_bits data

let to_z_signed ((sz, _) as bv) =
  let sgn = sign_bit bv in
  let r = to_z_unsigned bv in
  if sgn = 0 then r else Z.sub r (Z.shift_left Z.one sz)

let z63 = Z.shift_left Z.one 63
let z64 = Z.shift_left Z.one 64

let printable bv =
  let z = to_z_signed bv in
  if Z.geq z z63 then Z.sub z z64 else z

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

let of_int64 s =
  let result = create_data_bytes 64 in
  for i = 0 to 7 do
    Int64.shift_right_logical s (8 * i)
    |> Int64.logand 255L |> Int64.to_int |> Char.chr |> Bytes.set result i
  done;
  (64, Bytes.unsafe_to_string result)

let of_int x = of_int64 (Int64.of_int x)

let of_z sz z =
  let n = (sz + 7) / 8 and m = sz mod 8 in
  let result = Bytes.make n char_0 in
  let rec do_rec msk i =
    if i >= 0 then (
      let c = Z.extract z (i * 8) 8 |> Z.to_int |> ( land ) msk |> Char.chr in
      Bytes.unsafe_set result i c;
      do_rec 0xFF (i - 1))
  in
  let msk = last_char_mask (if m = 0 then 8 else m) in
  do_rec msk (n - 1);
  (sz, Bytes.unsafe_to_string result)

let of_int_sized sz i = of_z sz (Z.of_int i)

(* --------------------------------------------------------------------------

                                    Operations

   --------------------------------------------------------------------------*)

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
  if false then Format.eprintf "@[%a =@ %a@]@." _pp_data bv1 _pp_data bv2;
  length bv1 == length bv2
  && (* let bv1 = remask bv1 and bv2 = remask bv2 in *)
  String.equal (data bv1) (data bv2)

let compare bv1 bv2 =
  match Int.compare (length bv1) (length bv2) with
  | 0 ->
      (* let bv1 = remask bv1 and bv2 = remask bv2 in *)
      String.compare (data bv1) (data bv2)
  | i -> i

let string_fold_left f init s =
  let state = ref init in
  for i = 0 to String.length s - 1 do
    state := f !state @@ String.get s i
  done;
  !state

let bitcount (_length, data) =
  (* Inspired by both
      https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan
      https://github.com/backtracking/bitv/blob/master/bitv.ml#L351
  *)
  let rec one_byte x = if x = 0 then 0 else 1 + one_byte (x land (x - 1)) in
  (* Here Filliatre caches the count in an array, but we are not yet there. *)
  let folder acc c = acc + one_byte (Char.code c) in
  string_fold_left folder 0 data

let log2 =
  let rec loop acc i = if i <= 0 then acc else loop (acc + 1) (i lsr 1) in
  loop 0

let highest_set_bit (_length, data) =
  let rec loop i =
    if i < 0 then 0
    else
      let c = String.get data i |> Char.code in
      if c != 0 then log2 c + (8 * i) else loop (i - 1)
  in
  loop (String.length data - 1)

let to_int_signed (length, data) =
  if length = 0 then 0
  else if sign_bit (length, data) = 1 then
    -to_int (lognot (length - 1, data)) - 1
  else to_int (length - 1, data)

(* --------------------------------------------------------------------------

                                    Slices

   --------------------------------------------------------------------------*)

(* Write into dst at position pos_dst the bit of src at position pos_src. *)
let copy_bit dst src pos_src pos_dst =
  let c_src = String.get src (pos_src / 8)
  and c_dst = Bytes.get dst (pos_dst / 8) |> Char.code in
  let new_char_dst = set_bit (pos_src mod 8) (pos_dst mod 8) c_src c_dst in
  Bytes.set dst (pos_dst / 8) new_char_dst

let prefix (len, s) dst_len =
  assert (dst_len <= len);
  let sz8 = (dst_len + 7) / 8 in
  let dst_s = String.sub s 0 sz8 in
  (dst_len, dst_s)

let extract_slice (_length_src, data_src) positions =
  try
    let length = List.length positions in
    let result = create_data_bytes length in
    (* Same effect than [List.rev positions], as we build those from the end. *)
    let copy_bit_here i pos = copy_bit result data_src pos (length - 1 - i) in
    let () = List.iteri copy_bit_here positions in
    remask (length, Bytes.unsafe_to_string result)
  with Invalid_argument msg ->
    raise (Invalid_argument (Printf.sprintf "extract_sliced (%s)" msg))

let write_slice (length_dst, data_dst) (length_src, data_src) positions =
  let min x y = if x <= y then x else y in
  let length_src = min (List.length positions) length_src in
  let result = Bytes.of_string data_dst in
  (* Same effect as [List.rev positions], since we build those from the end. *)
  let copy_bit_here i pos = copy_bit result data_src (length_src - 1 - i) pos in
  let () = List.iteri copy_bit_here positions in
  remask (length_dst, Bytes.unsafe_to_string result)

(* Retuns length of destination *)
let pp (l, bs) = Printf.sprintf "%s<%d>" (to_string (l, Bytes.to_string bs)) l

let pp_bytes n dst =
  let sz = (n + 1) * 8 in
  pp (sz, dst)

(* [mix_chars_start off low_c high_c] return a char,
   whose off lower order bits are from low_c and
   the 8-off higher order bits are the  8-off lower
   order bits of high_c *)
let mix_chars_start off low_c high_c =
  let low_bits = ((1 lsl off) - 1) land Char.code low_c
  and high_bits = Char.code high_c lsl off in
  low_bits lor high_bits |> ( land ) 0xff |> Char.chr

(* [mix_chars_body off low_c high_c] return a char,
   whose off lower order bits are  the off higher
   order bits of low_c and the 8-off higher order bits are the
   8-off lower order bits of high_c *)
let mix_chars_body off low_c high_c =
  let low_bits = Char.code low_c lsr (8 - off)
  and high_bits = Char.code high_c lsl off in
  low_bits lor high_bits |> ( land ) 0xff |> Char.chr

(* [mix_chars_final off c] return a char,
   whose sz lower order bits are the
   sz bits of c at possition off. *)
let mix_char_final off sz c =
  (Char.code c lsr off) land ((1 lsl sz) - 1) |> Char.chr

let copy_into dst ((length_src, data_src) as b) offset =
  let () =
    if false then
      Printf.eprintf "copy_into %s + %s, offset=%d\n%!" (to_string b)
        (pp (offset, dst))
        offset
  in
  let length_dst = offset + length_src in
  if length_src <= 0 then length_dst
  else
    let n_off = offset / 8 and m_off = offset mod 8 in
    let () =
      if m_off = 0 then
        Bytes.blit_string data_src 0 dst n_off (String.length data_src)
      else
        (*
         * First handle the  first written char:
         * The 8-m_off low order bits of src.[0] are
         *  copied into the 8-m_off high order_bits of
         *  dst.[n_off] *)
        let prec_c = Bytes.get dst n_off and next_c = String.get data_src 0 in
        mix_chars_start m_off prec_c next_c |> Bytes.set dst n_off;
        let () =
          if false then Printf.eprintf "Start: %s\n%!" (pp_bytes n_off dst)
        in
        (*
         * Now, loop.
         * At each step, 8-off bits are taken from
         * the higher order bits of some source char,
         * while the off higher order bits are taken from the
         * lower order bits of the next_char
         *)
        (* Number of bits still to write *)
        let rem_bits = length_src - (8 - m_off) in
        (* Useful string length *)
        let src_str_len = (length_src + 7) / 8 in
        let rec do_rec i_src i_dst rem_bits =
          if i_src + 1 >= src_str_len then (i_src, i_dst, rem_bits)
          else
            let prec_c = String.get data_src i_src in
            let next_c = String.get data_src (i_src + 1) in
            mix_chars_body m_off prec_c next_c |> Bytes.set dst i_dst;
            let () =
              if false then
                Printf.eprintf "Body  -> %s\n%!" (pp_bytes i_dst dst)
            in
            do_rec (i_src + 1) (i_dst + 1) (rem_bits - 8)
        in
        let i_src, i_dst, rem_bits = do_rec 0 (n_off + 1) rem_bits in
        let () =
          if false then
            Printf.eprintf "i_src=%d, i_dst=%d, rem_bits=%d\n%!" i_src i_dst
              rem_bits
        in
        if rem_bits > 0 then
          let c = String.get data_src i_src in
          mix_char_final (8 - m_off) rem_bits c |> Bytes.set dst i_dst
    in
    let () =
      if false then
        Printf.eprintf "copy_into %s + %s ->%s\n%!" (to_string b)
          (pp (offset, dst))
          (pp (length_dst, dst))
    in
    length_dst

let concat bvs =
  (if false then
     let pp = List.map to_string bvs in
     Printf.eprintf "Concat %s\n%!" (String.concat "," pp));
  let length = List.fold_left (fun acc bv -> acc + length bv) 0 bvs in
  let result = create_data_bytes length in
  let _ = List.fold_right (copy_into result) bvs 0 in
  (length, Bytes.unsafe_to_string result)

(* --------------------------------------------------------------------------

                              Small utils

   --------------------------------------------------------------------------*)

let zeros length =
  let n = length / 8 and m = length mod 8 in
  let data = String.make (if m = 0 then n else n + 1) char_0 in
  (length, data)

let ones length =
  let n = length / 8 and m = length mod 8 in
  let data = String.make (if m = 0 then n else n + 1) char_ff in
  remask (length, data)

let zero = zeros 1
let one = ones 1
let empty = (0, "")
let is_zero = equal zero

let is_zeros bv =
  let _length, data = remask bv in
  string_for_all (( = ) char_0) data

let is_one = equal one
let is_ones bv = length bv |> ones |> equal bv

type mask = {
  length : int;
  set : string;
  unset : string;
  specified : string;
  initial_string : string;
}

let mask_length mask = mask.length

let mask_of_string s =
  let length_set, set =
    String.map (function 'x' -> '0' | '0' -> '0' | '1' -> '1' | c -> c) s
    |> of_string
  and length_unset, unset =
    String.map (function 'x' -> '0' | '0' -> '1' | '1' -> '0' | c -> c) s
    |> of_string
  and length_specified, specified =
    String.map (function 'x' -> '0' | '0' -> '1' | '1' -> '1' | c -> c) s
    |> of_string
  in
  let () =
    if false then
      Format.eprintf "Parsing %s gave %a and %a@." s _pp_data (length_set, set)
        _pp_data (length_unset, unset)
  in
  if length_set != length_unset || length_set != length_specified then
    raise (Invalid_argument "Mask")
  else { length = length_set; set; unset; specified; initial_string = s }

let mask_of_bitvector ((length, data) as bv) =
  let set = data
  and _, unset = lognot bv
  and _, specified = ones length
  and initial_string = to_string bv in
  { length; set; unset; specified; initial_string }

let matches bv mask =
  if length bv != mask.length then raise (Invalid_argument "mask_matches");
  equal
    (mask.length, mask.specified)
    (logor
       (logand bv (mask.length, mask.set))
       (logand (lognot bv) (mask.length, mask.unset)))

let mask_to_string mask = mask.initial_string

let mask_to_canonical_string mask =
  let set = to_string (mask.length, mask.set)
  and unset = to_string (mask.length, mask.unset) in
  let result = Bytes.create (String.length set) in
  for i = 0 to String.length set - 1 do
    let b =
      match (String.get set i, String.get unset i) with
      | '0', '0' -> 'x'
      | '1', _ -> '1'
      | _, '1' -> '0'
      | c, _ -> c
    in
    Bytes.set result i b
  done;
  Bytes.unsafe_to_string result

let mask_set mask = (mask.length, mask.set)
let mask_unset mask = (mask.length, mask.unset)
let mask_specified mask = (mask.length, mask.specified)
