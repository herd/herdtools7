(** A module for representing a tensors data type *)

type 'a t

val dims : 'a t -> int list
(** [dims tensor] returns the dimensions of [tensor]. *)

val create : int list -> 'a -> 'a t
(** [create dims value] creates a tensor with the given dimensions, initialized
    with [value]. This function assumes that all dimensions are positive. *)

(* val copy : 'a t -> 'a t *)
(** [copy tensor] creates a copy of [tensor]. *)

val get : 'a t -> int list -> 'a
(** [get coordinates tensor] returns the value at the given coordinates in the
    tensor. *)

val set : 'a t -> int list -> 'a -> 'a t
(** [set tensor coordinates value] sets [value] at the coordinates given by
    [coordinates] in the tensor to [value]. *)
