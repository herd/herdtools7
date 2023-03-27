(** This module provide an interface to ASL bitvector, and main operations on it. *)

type t
(** Represent a bitvector. *)

val length : t -> int
(** The length of the bitvector. *)

val pp_t : Format.formatter -> t -> unit
(** Print the bitvector, indexed from the right, as a serie of '0' and '1',
    delimited by apostrophes. Inside a horizontal box. *)

val to_string : t -> string
(** Returns a string representing the bitvector, indexed from the right and
    delimited by apostrophes. *)

val to_int : t -> int
(** Returns an integer representing the bitvector, little-endian. Result
    unspecified if [length > Sys.int_size]. *)

val to_int_signed : t -> int
(** Returns a signed integer representing the bitvector. *)

val to_int64 : t -> int64
(** Returns an integer representing the bitvector, little-endian. Result
    unspecified if [length > 64]. *)

val of_string : string -> t
(** [of_string s] interpretes [s] as a right-indexed representation of a
    bitvector. Characters others than '0' or '1' are ignored. The length of
    [of_string s] is equal to the number of such characters in [s]. *)

val of_int : int -> t
(** [of_int i] is the bitvector of length [Sys.int_size] (e.g. 63) that
    corresponds to [i] in little-endian, i.e. index 0 (for slicing operations
    corresponds to [i mod 2]. *)

val of_int64 : int64 -> t
(** [of_int i] is the bitvector of length 64 that corresponds to [i] in
    little-endian, i.e. index 0 (for slicing operations corresponds to
    [i mod 2]. *)

val lognot : t -> t
(** Bitwise not operation.
    Raise [Invalid_argument "bitwise_op"] if lengths are different. *)

val logand : t -> t -> t
(** Bitwise and operation.
    Raise [Invalid_argument "bitwise_op"] if lengths are different. *)

val logor : t -> t -> t
(** Bitwise or operation.
    Raise [Invalid_argument "bitwise_op"] if lengths are different. *)

val logxor : t -> t -> t
(** Bitwise xor operation.
    Raise [Invalid_argument "bitwise_op"] if lengths are different. *)

val equal : t -> t -> bool
(** [equal b1 b2] is [true] if and only if [b1] and [b2] are bitwise equal. *)

val compare : t -> t -> int
(** The comparison function for bitvectors, with the same specification as
    [Stdlib.compare]. *)

val bitcount : t -> int
(** Returns the number of bits set to 1. *)

val extract_slice : t -> int list -> t
(** [extract_slice src positions] returns a bitvector whose [i]-th bit is the
    bit of [src] whose index is the [i]-th element of [positions].
    Raise [Invalid_argument] if any index in positions is greater or equal to
    the length of [src]. *)

val write_slice : t -> t -> int list -> t
(** [write_slice dst src positions] is a copy of [dst] where each bit at index
    [i] in [src] has been written in [dst] at the index given by the [i]-th
    element of [positions].
    Raise [Invalid_argument] if [positions] has not the same length as [src],
    or any of the indexes in [positions] is greater than the length of [dst]. *)

val concat : t list -> t
(** [concat [bv2; bv1; bv0]] is the concatenation of [bv0], [bv1], and [bv2],
    in this order, i.e. if [bv0] is not empty, the following is true:

        equal (extract_slice (concat [bv1; bv0]) [ 0 ]) (extract_slice bv0 [ 0 ])

 *)

val one : t
(** A length 1 bitvector with a 1 bit inside. *)

val zero : t
(** A length 1 bitvector with a 0 bit inside. *)

val ones : int -> t
(** [ones n] is a bitvector of length [n] with every bit set. *)

val zeros : int -> t
(** [zeros n] is a bitvector of length [n] without any bit set. *)
