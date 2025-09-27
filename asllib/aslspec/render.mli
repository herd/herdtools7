(** A module for rendering an ASL semantics-specification for inclusion in a
    LaTeX document. *)

val render : Spec.t -> Format.formatter -> unit
(** Renders the LaTeX macros for rendering every element in [spec] to the
    provided [formatter] as LaTeX. *)

val render_debug : Spec.t -> Format.formatter -> unit
(** Renders a stand-alone LaTeX file that invokes all of the macros generated
    for rendering every element in [spec] to the provided [formatter]. *)
