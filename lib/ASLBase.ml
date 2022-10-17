(* Base definitions for ASL.

   This file is divided into two parts:
       - the ASL AST which uses ASL notations
       - the Base module, which implements [ArchBase.S], and uses herd notations
*)

(* Part One: ASL definitions.

   Note: this is only a small subset of ASL.
*)
type identifier = string
type value = int
type unop = unit Op.op1
type binop = Op.op

type expr =
  | ELiteral of value  (** A direct value *)
  | EVar of identifier  (** A variable *)
  | EUnop of unop * expr  (** [- e] *)
  | EBinop of expr * binop * expr  (** [e1 + e2] *)
  | EGet of expr * expr  (** [e1[e2]], but only for arrays *)

type lexpr = LEVar of identifier | LESet of lexpr * expr

type stmt =
  | SPass
  | SExit
  | SThen of stmt * stmt
  | SAssign of lexpr * expr
  | SCond of expr * stmt * stmt
  | SCall of identifier * expr list
  | SReturn

let stmt_from_list = function
  | [] -> SPass
  | h :: t -> List.fold_left (fun s1 s2 -> SThen (s1, s2)) h t

(* Formatting functions *)
(* We use Format here because I know how to indent with it. *)
let pp_print_unop f o =
  Format.pp_print_string f
  @@ Op.pp_op1 false (fun _b _aop -> Warn.fatal "Not yet implemented") o

let pp_print_binop f o = Format.pp_print_string f @@ Op.pp_op o
let pp_print_value f v = Format.pp_print_int f v

let rec pp_print_expr f e =
  let open Format in
  match e with
  | EUnop (o, e) -> fprintf f "@[%a %a@]" pp_print_unop o pp_print_expr e
  | EBinop (e1, o, e2) ->
      fprintf f "@[<2>%a@ %a %a@]" pp_print_expr e1 pp_print_binop o
        pp_print_expr e2
  | EVar x -> pp_print_string f x
  | ELiteral v -> pp_print_value f v
  | EGet (e1, e2) ->
      fprintf f "@[<2>%a[@,%a@;<0 -2>]@]" pp_print_expr e1 pp_print_expr e2

let rec pp_print_lexpr f e =
  let open Format in
  match e with
  | LEVar x -> pp_print_string f x
  | LESet (le, e) ->
      fprintf f "@[<2>%a[@,%a@;<0 -2>]@]" pp_print_lexpr le pp_print_expr e

let rec pp_print_stmt f s =
  let open Format in
  match s with
  | SPass -> pp_print_string f "Pass"
  | SExit -> pp_print_string f "Exit"
  | SReturn -> pp_print_string f "return"
  | SAssign (le, e) ->
      fprintf f "@[<3>%a =@ %a@]" pp_print_lexpr le pp_print_expr e
  | SThen (s1, s2) ->
      fprintf f "@[<v 0>%a ;@;%a@]" pp_print_stmt s1 pp_print_stmt s2
  | SCond (e, s1, s2) ->
      fprintf f "@[<3>@[<h>if@ %a@ then@]@ %a@ else@ %a@]" pp_print_expr e
        pp_print_stmt s1 pp_print_stmt s2
  | SCall (x, args) ->
      fprintf f "@[<3>%s(%a)@]" x
        (pp_print_list ~pp_sep:(fun f () -> fprintf f ",@ ") pp_print_expr)
        args

let pp_expr e = Format.asprintf "%a" pp_print_expr e
let pp_lexpr le = Format.asprintf "%a" pp_print_lexpr le
let pp_stmt s = Format.asprintf "%a" pp_print_stmt s
let main_asl_proc = (0, None, MiscParser.Main)

(* Part Two. ASL definitions inside herd. *)
let arch = Archs.asl

let base_type =
  CType.Base "int" (* This is strange, and maybe to be modified. *)

let endian = Endian.Little (* I think this is true but I really don't know. *)

(* In ASL-Spec, this is called an identifier *)
type reg = identifier

let parse_reg s = Some s
let pp_reg x = x
let reg_compare = String.compare

(* Symbolic registers. For now, everything is a symbolic register. *)
let symb_reg_name r = Some r
let symb_reg r = r
let type_reg _r = base_type
(* End of symbolic registers section. *)

(* Barriers. None yet in ASL. *)
type barrier = unit

let pp_barrier _barrier = Warn.fatal "No barriers in ASL yet"
let barrier_compare _b1 _b2 = Warn.fatal "No barriers in ASL yet"

(* Instructions. In ASL, this is called statements. *)
type instruction = stmt
type parsedInstruction = stmt

let dump_instruction = Format.asprintf "%a" pp_print_stmt
let dump_instruction_hash = dump_instruction
let pp_instruction _mode = dump_instruction

(* Taken from CBase.ml . I don't know if this is correct, or if it makes sense here. *)
let allowed_for_symb =
  List.map (fun x -> "r" ^ string_of_int x) (Misc.interval 0 64)

let fold_regs (_fc, _fs) acc _ins = acc
let map_regs _fc _fs ins = ins
let fold_addrs _f acc _ins = acc
let map_addrs _f ins = ins
let norm_ins ins = ins
let get_next _ins = Warn.fatal "C get_next not implemented"
(* End of copied from CBase.ml *)

(* As in CBase, we don't need those functions now. *)
let get_macro _s = assert false
let hash_pteval _ = assert false

include Pseudo.Make (struct
  type ins = instruction
  type pins = parsedInstruction
  type reg_arg = reg

  (* As in JavaBase, we don't do anything to the ast here. *)
  let parsed_tr e = e

  let get_naccesses =
    (* A little subtlety for memory accesses:
       For EGet, LESet or SAssign, as we make only one access once the full address is known,
       we don't add any accesses to add to those made by the left-expression. *)
    let rec get_lexpr k = function
      | LEVar _ -> k + 1
      | LESet (le, e) -> get_expr (get_lexpr k le) e
    and get_expr k = function
      | ELiteral _ -> k
      | EVar _ -> k + 1
      | EUnop (_o, e) -> get_expr k e
      | EBinop (e1, _o, e2) -> get_expr (get_expr k e1) e2
      | EGet (e1, e2) -> get_expr (get_expr k e1) e2
    and get_stmt k = function
      | SPass | SExit | SReturn -> k
      | SAssign (le, e) -> get_expr (get_lexpr k le) e
      | SThen (s1, s2) -> get_stmt (get_stmt k s2) s1
      | SCond (e, s1, s2) -> get_stmt (get_stmt (get_expr k e) s1) s2
      | SCall (_, args) -> List.fold_left get_expr k args
    in

    fun s -> get_stmt 0 s

  (* As in CBase, we don't implement fold and maps over labels. *)
  let fold_labels acc _f _ins = acc
  let map_labels _f ins = ins
end)

let reg_arg f_name i = f_name ^ "%" ^ Int.to_string i

let make_func name args_names body =
  let body' =
    if List.length args_names == 0 then body
    else
      let one_arg i x = SAssign (LEVar x, EVar (reg_arg name i)) in
      let assign_args = List.mapi one_arg args_names in
      SThen (stmt_from_list assign_args, body)
  in
  let body'' = SThen (body', SReturn) in
  Label (name, Instruction body'')

let asl_top_level =
  make_func "__asl_top_level__" []
    (stmt_from_list [ SCall ("main", []); SExit ])
