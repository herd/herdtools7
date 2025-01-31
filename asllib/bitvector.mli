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

(** This module provide an interface to ASL bitvector, and main operations on it. *)

type t
(** Represent a bitvector. *)

val length : t -> int
(** The length of the bitvector. *)

(* --------------------------------------------------------------------------*)
(** {2 Constructors} *)

val zero : t
(** A length 1 bitvector with a 0 bit inside. *)

val is_zero : t -> bool
(** zero predicate *)

val one : t
(** A length 1 bitvector with a 1 bit inside. *)

val is_one : t -> bool
(** one predicate *)

val empty : t
(** A length 0 bitvector. *)

val ones : int -> t
(** [ones n] is a bitvector of length [n] with every bit set. *)

val zeros : int -> t
(** [zeros n] is a bitvector of length [n] without any bit set. *)

val of_string : string -> t
(** [of_string s] interpretes [s] as a right-indexed representation of a
    bitvector. Characters others than '0' or '1' are ignored. The length of
    [of_string s] is equal to the number of such characters in [s]. *)

val of_int : int -> t
(** [of_int i] is the bitvector of length [Sys.int_size] (e.g. 63) that
    corresponds to [i] in little-endian, i.e. index 0 (for slicing operations
    corresponds to [i mod 2]. *)

val of_int_sized : int -> int -> t
(** [of_int n i] is the bitvector of length [n] that corresponds to [i] in
    little-endian, i.e. index 0 (for slicing operations corresponds to
    [i mod 2]. *)

val of_int64 : int64 -> t
(** [of_int i] is the bitvector of length 64 that corresponds to [i] in
    little-endian, i.e. index 0 (for slicing operations corresponds to
    [i mod 2]. *)

val of_z : int -> Z.t -> t
(** [of_int sz i] is the bitvector of length [sz] that corresponds to [i] in
    little-endian. *)

(* --------------------------------------------------------------------------*)
(** {2 Exports} *)

val pp_t : Format.formatter -> t -> unit
(** Print the bitvector, indexed from the right, as a serie of '0' and '1',
    delimited by apostrophes. Inside a horizontal box. *)

val to_string : t -> string
(** Returns a string representing the bitvector, indexed from the right and
    delimited by apostrophes. *)

val to_string_hexa : t -> string
(** Returns a string representing the bitvector in hexadecimal, indexed from
    the right and preceded by '0x'. *)

val to_int : t -> int
(** Returns an integer representing the bitvector, little-endian. Result
    unspecified if [length > Sys.int_size]. *)

val to_int_signed : t -> int
(** Returns a signed integer representing the bitvector. *)

val to_int64_unsigned : t -> int64
(** Returns an integer representing the bitvector, little-endian. Result
    unspecified if [length > 64]. *)

val to_int64_signed : t -> int64
(** Returns an integer representing the bitvector, little-endian. Result
    unspecified if [length > 64]. *)

val to_z_unsigned : t -> Z.t
val to_z_signed : t -> Z.t
val printable : t -> Z.t

(* --------------------------------------------------------------------------*)
(** {2 Operations on bitvectors} *)

val lognot : t -> t
(** Bitwise not operation.

    @raise Invalid_argument if lengths are different. *)

val logand : t -> t -> t
(** Bitwise and operation.

    @raise Invalid_argument if lengths are different. *)

val logor : t -> t -> t
(** Bitwise or operation.
    @raise Invalid_argument if lengths are different. *)

val logxor : t -> t -> t
(** Bitwise xor operation.
    @raise Invalid_argument if lengths are different. *)

val equal : t -> t -> bool
(** [equal b1 b2] is [true] if and only if [b1] and [b2] are bitwise equal. *)

val compare : t -> t -> int
(** The comparison function for bitvectors, with the same specification as
    [Stdlib.compare]. *)

val sign_extend : int -> t -> t
(** [sign_extend nbytes bv] returns a copy of bv of length [8*nbytes],
    left-padded with [bv]'s bit-sign. *)

val bitcount : t -> int
(** Returns the number of bits set to 1. *)

val highest_set_bit : t -> int
(** Returns the index of the highest set bit. *)

val prefix : t -> int -> t
(** [prefix src len] returns the prefix of size [len] of bitvector [src].
    Will crash if [len] is strictly more then the size of [src]. *)

val extract_slice : t -> int list -> t
(** [extract_slice src positions] returns a bitvector whose [i]-th bit is the
    bit of [src] whose index is the [i]-th element of [positions].
    @raise Invalid_argument if any index in positions is greater or equal to
    the length of [src]. *)

val write_slice : t -> t -> int list -> t
(** [write_slice dst src positions] is a copy of [dst] where each bit at index
    [i] in [src] has been written in [dst] at the index given by the [i]-th
    element of [positions].
    @raise Invalid_argument if [positions] has not the same length as [src],
    or any of the indexes in [positions] is greater than the length of [dst]. *)

val concat : t list -> t
(** [concat [bv2; bv1; bv0]] is the concatenation of [bv0], [bv1], and [bv2],
    in this order, i.e. if [bv0] is not empty, the following is true:

    {[
    equal (extract_slice (concat [bv1; bv0]) [ 0 ]) (extract_slice bv0 [ 0 ])
    ]}
 *)

val is_zeros : t -> bool
(** [is_zeros bv] is true if every bit of bv is unset. *)

val is_ones : t -> bool
(** [is_ones bv] is true if every bit of bv is set. *)

(* --------------------------------------------------------------------------*)
(**
   {2 Bitvector masks}

   Bitvector in ASL can be matched against masks, that have the same syntax
   than bitvectors, with an extra possible bit: ['x']. This bits indicates that
   the mask would match against any bit at this position.

   For example:
   {[
     assert ('01' IN {'01'}) == TRUE;
     assert ('01' IN {'0x'}) == TRUE;
     assert ('10' IN {'0x'}) == FALSE;
   ]}
*)

type mask
(** Internal representation of a mask. *)

val mask_length : mask -> int
(** Returns the length of bitvectors matched by this mask. *)

val mask_of_string : string -> mask
(** Build a mask from its ASL representation. *)

val mask_of_bitvector : t -> mask
(** Build a mask that matches a bitvector. *)

val matches : t -> mask -> bool
(** [matches mask bv] is true iff [bv] matches [mask]. *)

val mask_to_string : mask -> string
(** Returns an ASL string matching its value. *)

val mask_to_canonical_string : mask -> string
(** Returns a unique ASL string matching its value. *)

val mask_set : mask -> t
(** [mask_set m]'s set bits are those required set by [m]. *)

val mask_unset : mask -> t
(** [mask_unset m]'s set bits are those required unset by [m]. *)

val mask_specified : mask -> t
(** [mask_specified m]'s set bits are those require set or unset by [m]. *)
