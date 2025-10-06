type operator = Assign | Equal | Iff | List | Size | Union
(* | NotEqual
    | And
    | Or
    | Not
    | Choice
    | ASTLabel *)

type application_lhs =
  | Appl_PrefixOperator of operator
  | Appl_InfixOperator of operator
  | Appl_Expr of expr

(** A term that can be used to form a judgement. *)
and expr =
  | Var of string
  | Tuple of expr list
  | Application of { lhs : application_lhs; args : expr list }
  | Field of { record : expr; field : string }
  | ListIndex of { list : expr; index : string }
  | Record of { name : expr; fields : (string * expr) list }

type judgment_form =
  | Expr of expr  (** a Boolean-valued expression *)
  | Output of expr  (** The output configuration of a conclusion judgement. *)
  | Transition of { lhs : expr; rhs : expr }
      (** A transition from the [lhs] configuration to the [rhs] configuration.
      *)
  | Indexed of { index : string; list : string; body : judgment_form }

type judgement = { form : judgment_form; att : Attributes.t }
(** A judgement represents either a premise or the the output configuration of
    the conclusion. *)

(** A tree of judgments. *)
type t =
  | Judgement of judgement  (** A leaf judgment. *)
  | Case of { name : string; elements : t list }
