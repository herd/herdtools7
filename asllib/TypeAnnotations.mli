(** {b This module is not part of the ASL type system.}

    Type annotation completion is an optional implementation-level
    post-processing pass for consumers that require a fully annotated AST. It
    does not decide whether an ASL program is well typed: it runs only after
    ordinary typechecking has succeeded. Annotations inserted by this module are
    consumer-facing metadata and must not be interpreted as results of ASL
    typechecking.

    The pass conservatively fills type annotation holes. These are not
    necessarily the precise annotations that the ASL typechecker would infer or
    generate. Existing typechecker annotations are preserved.

    {b Explicit call parameter.} An explicit actual parameter is processed by
    ordinary expression typing before completion. For example:
    {[
      let value = UInt{2}('11');
    ]}
    The parameter expression [2] has the constrained integer type assigned by
    the typechecker. Completion preserves the annotation. More generally, an
    explicit parameter retains whichever constrained or parameterized integer
    type the typechecker assigned to it.

    {b Inserted call parameter.} In the following call, the typechecker may
    infer and insert the omitted width parameter [2] from the bitvector
    argument:
    {[
      let value = UInt('11');
    ]}
    If that inserted expression has [ty_opt = None], completion assigns it
    [T_Int UnConstrained]. This is not a claim that the typechecker inferred the
    type [integer]; it is only the consumer-facing completion described above.

    {b Structured base value.} A declaration without an initializer causes the
    typechecker to synthesize a base value expression, for example:
    {[
      var registers : array [[31]] of bits(64);
    ]}
    Completion propagates the declared array type through the synthesized
    structure: the array length uses the annotation on the declared length when
    available, falling back to an unconstrained integer, and the repeated value
    is completed as [bits(64)]. *)

(** {1 Completion} *)

val complete : ?validate:bool -> StaticEnv.global -> AST.t -> AST.t
(** [complete ~validate genv ast] conservatively fills missing type annotations
    in the already typechecked AST [ast], using global environment [genv] to
    resolve named types. Existing annotations are preserved. When [validate] is
    true, the completed AST is checked for any remaining annotation holes. *)
