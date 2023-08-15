(** Pretty-printers for ASL ASTs. *)

open AST

(** {1 Utils} *)

type 'a printer = Format.formatter -> 'a -> unit
(** A general pretty-printer type. *)

(* Available from 4.12.0 *)
val pp_print_seq : ?pp_sep:unit printer -> 'a printer -> 'a Seq.t printer
(** Re-exported from stdlib 4.12, print q sequence from its elements. *)

val pp_pos : 'a annotated printer
(** Print a position. *)

(** {1 AST pretty-printers} *)

val pp_literal : literal printer
(** Print a literal from its components.*)

val pp_expr : expr printer
(** Pretty-print an expression. *)

val pp_ty : ty printer
(** Pretty-print a type. *)

val pp_typed_identifier : typed_identifier printer
(** Pretty-print a variable and its type. *)

val pp_lexpr : lexpr printer
(** Pretty-print the left-hand side of an assignment. *)

val pp_stmt : stmt printer
(** Pretty-print a statement. *)

val pp_t : 'p t printer
(** Print an AST from printer for a literal *)

val pp_version : [ `ASLv0 | `ASLv1 | `Any ] printer
(** Print the ASL version. *)

val pp_scope : scope printer
(** Print a scope. *)

(** {1 Pretty-print to strings} *)

val literal_to_string : literal -> string
(** Converts a literal into a string. *)

val binop_to_string : binop -> string
(** Writes a binop as an ASL operator. *)

val unop_to_string : unop -> string
(** Writes a unop as an ASL operator. *)

val ty_to_string : ty -> string
(** Converts a type into a string. *)

val t_to_string : 'p t -> string
(** [t_to_string v_to_string ast] is a string representing [ast] with literals
    printed with [v_to_string].*)
